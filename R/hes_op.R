# Outpatients (NHSE_HES_OUTPATIENTS) ----------------------------------------------

# Appointments come in courses: a first attendance after a referral, then
# follow-ups in the same specialty and provider. Follow-ups are on average 70
# days apart (assumption).
.hes_op <- function(people, frailty, profile, death_day) {
  # courses also start in the year before coverage, so early follow-ups exist
  months <- c(seq(profile$months[1], by = "-1 month", length.out = 13)[13:2], profile$months)
  period <- c(rep(profile$rates$op$period[1], 12), profile$rates$op$period)
  window_start <- as.numeric(profile$window[["start"]])
  window_end <- as.numeric(profile$window[["end"]])
  rules <- profile$fill_rules$op

  rates <- .hes_base_rate(people, profile$rates$op$par, months)
  courses <- .hes_draw_events(rates$base * rep(period, each = nrow(people)) * frailty)
  n_c <- length(courses$person)
  if (n_c == 0) return(NULL)
  course_start <- pmax(.hes_day_in_month(months, courses$month), ceiling(people$birth[courses$person]))
  n_follow <- stats::rgeom(n_c, 1 / profile$appointments_per_course)

  course <- rep(seq_len(n_c), n_follow + 1L)
  order_in_course <- sequence(n_follow + 1L)
  gap <- ifelse(order_in_course == 1L, 0, round(stats::rexp(length(course), 1 / 70)))
  appt <- course_start[course] + stats::ave(gap, course, FUN = cumsum)
  person <- courses$person[course]
  keep <- appt >= window_start & appt <= window_end & appt <= death_day[person]
  course <- course[keep]
  order_in_course <- order_in_course[keep]
  appt <- appt[keep]
  person <- person[keep]
  n_a <- length(appt)
  if (n_a == 0) return(NULL)
  first <- order_in_course == 1L
  last <- order_in_course == stats::ave(order_in_course, course, FUN = max)
  age <- (appt - people$birth[person]) / 365.25
  female <- people$female[person]

  wait <- pmax(0, .hes_from_quantiles(profile$op_wait, n_c))
  request <- course_start - wait

  # diagnoses are rarely recorded in outpatients: "R69X" means not known
  n_diag <- as.integer(.hes_sample(rules$NODIAGS, n_a))
  n_diag[is.na(n_diag)] <- 0L
  diag_record <- rep(seq_len(n_a), pmin(n_diag, 12L))
  diag_code <- .hes_sample_by_group(.hes_band_key(age, female, profile)[diag_record],
                                    profile$diagnosis$codes, function(k) {
    .hes_diagnosis_prob(profile, k %/% 2L, k %% 2L == 1L)
  })
  diag <- .hes_codes_wide(diag_record, diag_code, n_a, 12L, "DIAG_4_")
  no_diag <- diag$count == 0

  n_proc <- as.integer(.hes_sample(rules$NOPROCS, n_a))
  n_proc[is.na(n_proc)] <- 0L
  proc_record <- rep(seq_len(n_a), pmin(n_proc, 24L))
  proc <- .hes_codes_wide(proc_record, .hes_sample(profile$op_procedures, length(proc_record)),
                          n_a, 24L, "OPERTN_4_")
  no_proc <- proc$count == 0
  dash_share <- profile$no_procedure_dash[["op"]]

  first_att <- .hes_sample(rules$FIRSTATT, n_a)
  first_share <- function(codes) {
    v <- rules$FIRSTATT
    sum(v$prob[v$values %in% codes[1]]) / sum(v$prob[v$values %in% codes])
  }
  outcome <- rules$OUTCOME

  out <- list()
  out$SUBJECT_ID <- people$SUBJECT_ID[person]
  out$ATTENDKEY <- rep(NA_character_, n_a)  # assigned once all chunks are combined
  out$APPTDATE <- appt
  out$REQDATE <- request[course]
  out$WAITDAYS <- ifelse(first, course_start[course] - request[course], NA)
  out$FIRSTATT <- ifelse(first,
                         ifelse(stats::runif(n_a) < first_share(c("1", "3")), "1", "3"),
                         ifelse(stats::runif(n_a) < first_share(c("2", "4")), "2", "4"))
  share_of <- function(a, b) outcome$prob[outcome$values == a] / sum(outcome$prob[outcome$values %in% c(a, b)])
  out$OUTCOME <- ifelse(last,
                        ifelse(stats::runif(n_a) < share_of("1", "9"), "1", "9"),
                        ifelse(stats::runif(n_a) < share_of("2", "3"), "2", "3"))
  out$ACTIVAGE <- floor(age)
  out$SEX <- people$sex[person]
  out$ETHNOS <- .hes_record_ethnicity(people$ethnicity[person], profile)
  out$PARTYEAR <- .hes_partyear(as.Date(appt, origin = "1970-01-01"))
  out$NODIAGS <- diag$count
  out$NOPROCS <- proc$count
  for (j in seq_len(12)) {
    code4 <- diag$columns[, j]
    if (j == 1) code4[no_diag] <- "R69X"
    out[[sprintf("DIAG_4_%02d", j)]] <- code4
    out[[sprintf("DIAG_3_%02d", j)]] <- substr(code4, 1, 3)
    out[[sprintf("DIAG_%02d", j)]] <- ifelse(code4 %in% "R69X", "R69X6", code4)
  }
  out$DIAG_4_CONCAT <- ifelse(no_diag, "R69X", diag$concat)
  out$DIAG_3_CONCAT <- gsub("([A-Z][0-9]{2})[0-9A-Z]", "\\1", out$DIAG_4_CONCAT)
  none_code <- ifelse(stats::runif(n_a) < dash_share, "-", "-   ")
  for (j in seq_len(24)) {
    code4 <- proc$columns[, j]
    if (j == 1) code4[no_proc] <- none_code[no_proc]
    out[[sprintf("OPERTN_4_%02d", j)]] <- code4
    out[[sprintf("OPERTN_3_%02d", j)]] <- ifelse(code4 %in% c("-", "-   "), "-", substr(code4, 1, 3))
    out[[sprintf("OPERTN_%02d", j)]] <- ifelse(code4 %in% c("-", "-   "), "-", code4)
  }
  out$OPERTN_4_CONCAT <- ifelse(no_proc, "-", proc$concat)

  types <- stats::setNames(profile$schema$op$type, profile$schema$op$column)
  .hes_fill_remaining(out, rules, types, n_a, person, course, appt)
}
