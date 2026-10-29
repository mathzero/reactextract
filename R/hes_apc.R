# Admitted patient care (NHSE_HES_ADMITTED_PATIENTS) ----------------------------

.hes_eta <- function(par, age, female) {
  z <- (age - 50) / 20
  par[["b0"]] + par[["b1"]] * z + par[["b2"]] * z^2 + par[["b3"]] * z^3 +
    par[["bf"]] * female + par[["bm"]] * female * (age >= 18 & age < 45)
}

# expected events per person-month (people x months), before calendar effects
.hes_base_rate <- function(people, par, months) {
  age <- outer(-people$birth, as.numeric(months) + 14, "+") / 365.25
  age[age < 0] <- NA
  base <- exp(.hes_eta(par, age, people$female)) / 12
  base[is.na(base)] <- 0
  list(age = age, base = base)
}

# Poisson event counts from a people x months rate matrix, as one row per event
.hes_draw_events <- function(lam) {
  n <- stats::rpois(length(lam), lam)
  hit <- which(n > 0)
  idx <- rep(hit, n[hit])
  list(person = (idx - 1L) %% nrow(lam) + 1L, month = (idx - 1L) %/% nrow(lam) + 1L)
}

# a random day within each event's month
.hes_day_in_month <- function(months, month) {
  starts <- as.numeric(months)
  ends <- c(starts[-1], as.numeric(seq(months[length(months)], by = "month", length.out = 2)[2]))
  starts[month] + floor(stats::runif(length(month)) * (ends - starts)[month])
}

.hes_band_key <- function(age, female, profile) {
  findInterval(age, profile$age_bands) * 2L + as.integer(female)
}

.hes_apc <- function(people, frailty, profile) {
  months <- profile$months
  window_end <- as.numeric(profile$window[["end"]])
  rules <- profile$fill_rules$apc
  dx <- profile$diagnosis

  # 1. spells: general admissions, plus COVID-19 admissions from REACT's monthly counts
  rates <- .hes_base_rate(people, profile$rates$apc$par, months)
  general <- .hes_draw_events(rates$base * rep(profile$rates$apc$period, each = nrow(people)) * frailty)
  relative <- profile$covid$relative_by_band[findInterval(rates$age, profile$age_bands)]
  relative[is.na(relative)] <- 0
  covid <- .hes_draw_events(rates$base * relative * rep(profile$covid$scale, each = nrow(people)) * frailty)
  sp <- data.frame(
    person = c(general$person, covid$person),
    month = c(general$month, covid$month),
    covid = rep(c(FALSE, TRUE), c(length(general$person), length(covid$person)))
  )
  if (nrow(sp) == 0) return(list(data = NULL, death_day = rep(Inf, nrow(people))))
  sp$admit <- pmax(.hes_day_in_month(months, sp$month), ceiling(people$birth[sp$person]))
  n_sp <- nrow(sp)
  female <- people$female[sp$person]
  age_admit <- (sp$admit - people$birth[sp$person]) / 365.25

  # 2. primary diagnosis by age and sex; COVID-19 is primary for some COVID spells
  key <- .hes_band_key(age_admit, female, profile)
  sp$primary <- .hes_sample_by_group(key, dx$codes, function(k) {
    .hes_diagnosis_prob(profile, k %/% 2L, k %% 2L == 1L)
  })
  covid_primary <- sp$covid & stats::runif(n_sp) < profile$covid$primary_share
  covid_codes <- list(values = names(profile$covid$code_share), prob = unname(profile$covid$code_share))
  sp$primary[covid_primary] <- .hes_sample(covid_codes, sum(covid_primary))
  sp$covid_secondary <- sp$covid & !covid_primary

  # 3. admission type follows the diagnosis; day cases are elective admissions
  code_i <- match(sp$primary, dx$codes)
  share <- dx$admission_share[code_i, , drop = FALSE]
  u <- stats::runif(n_sp)
  c1 <- share[, 1]
  c2 <- c1 + share[, 2]
  c3 <- c2 + share[, 3]
  sp$admission <- c("emergency", "waitlist", "planned", "other")[1L + (u > c1) + (u > c2) + (u > c3)]
  sp$ADMIMETH <- NA_character_
  for (a in names(profile$admission_method)) {
    i <- sp$admission == a
    sp$ADMIMETH[i] <- .hes_sample(profile$admission_method[[a]], sum(i))
  }
  elective <- sp$admission %in% c("waitlist", "planned")
  p_day <- dx$elective_day_case[code_i]
  odds <- p_day / (1 - p_day) * profile$day_case_multiplier
  p_day <- ifelse(is.finite(odds), odds / (1 + odds), 1)
  sp$day_case <- elective & stats::runif(n_sp) < p_day
  mean_days <- pmax(dx$spell_days_mean[code_i], 0.5)
  sp$duration <- ifelse(sp$day_case, 0, stats::rnbinom(n_sp, size = 0.7, mu = mean_days))

  # 4. one spell at a time per person, then in-hospital deaths end later activity
  sp <- sp[order(sp$person, sp$admit), ]
  for (pass in 1:20) {
    prev_end <- c(-Inf, (sp$admit + sp$duration)[-n_sp])
    clash <- c(FALSE, sp$person[-1] == sp$person[-n_sp]) & sp$admit <= prev_end
    if (!any(clash)) break
    sp$admit[clash] <- prev_end[clash] + 1 + stats::rgeom(sum(clash), 0.1)
  }
  age_admit <- (sp$admit - people$birth[sp$person]) / 365.25
  # the death risk is calibrated over all spells; nobody dies in a day case, so
  # the same expected deaths are spread over ordinary admissions
  p_death <- profile$death$base * exp(profile$death$slope * (pmin(age_admit, 95) - 75))
  p_death <- pmin(0.5, p_death * sum(p_death) / max(sum(p_death[!sp$day_case]), 1e-12))
  died <- !sp$day_case & stats::runif(n_sp) < p_death
  discharge <- sp$admit + sp$duration
  death_day <- rep(Inf, nrow(people))
  first_death <- tapply(discharge[died], sp$person[died], min)
  death_day[as.integer(names(first_death))] <- first_death
  sp$died <- died & discharge == death_day[sp$person]
  keep <- sp$admit <= death_day[sp$person] & discharge <= window_end
  sp <- sp[keep, ]
  n_sp <- nrow(sp)
  if (n_sp == 0) return(list(data = NULL, death_day = death_day))
  sp$spell <- seq_len(n_sp)

  # 5. episodes within spells: day cases have one, so ordinary admissions carry
  # all of REACT's multi-episode spells (episodes can start and end the same day)
  eps <- profile$episodes_per_spell
  ordinary_prob <- eps$prob
  ordinary_prob[eps$n == 1] <- max(eps$prob[eps$n == 1] - profile$day_case_target, 0.05)
  n_ep <- rep(1L, n_sp)
  n_ep[!sp$day_case] <- .hes_sample(list(values = eps$n, prob = ordinary_prob / sum(ordinary_prob)),
                                    sum(!sp$day_case))
  s <- rep(seq_len(n_sp), n_ep)
  order_in_spell <- sequence(n_ep)
  first <- order_in_spell == 1L
  last <- order_in_spell == n_ep[s]
  boundary_spell <- rep(seq_len(n_sp), n_ep - 1L)
  boundary <- floor(stats::runif(length(boundary_spell)) * (sp$duration[boundary_spell] + 1))
  boundary <- boundary[order(boundary_spell, boundary)]
  start_offset <- numeric(length(s))
  start_offset[!first] <- boundary
  end_offset <- c(start_offset[-1], 0)
  end_offset[last] <- sp$duration[s][last]
  episode_start <- sp$admit[s] + start_offset
  episode_end <- sp$admit[s] + end_offset
  n_e <- length(s)
  person <- sp$person[s]
  ep_female <- people$female[person]
  ep_age <- (episode_start - people$birth[person]) / 365.25
  ep_key <- .hes_band_key(ep_age, ep_female, profile)

  # 6. diagnoses: later episodes usually keep the spell's primary diagnosis
  ep_primary <- sp$primary[s]
  redraw <- !first & !(sp$covid[s] & sp$primary[s] %in% names(profile$covid$code_share)) &
    stats::runif(n_e) > 0.6
  ep_primary[redraw] <- .hes_sample_by_group(ep_key[redraw], dx$codes, function(k) {
    .hes_diagnosis_prob(profile, k %/% 2L, k %% 2L == 1L)
  })
  diag_count <- .hes_count_by_age(rules$DIAG_COUNT, ep_age)
  sec_record <- rep(seq_len(n_e), diag_count - 1L)
  sec_code <- .hes_sample_by_group(ep_key[sec_record], profile$secondary$codes, function(k) {
    .hes_secondary_prob(profile, k %/% 2L, k %% 2L == 1L)
  })
  covid_record <- which(sp$covid_secondary[s])
  covid_position <- .hes_sample(list(values = profile$covid$secondary_position$position,
                                     prob = profile$covid$secondary_position$prob),
                                length(covid_record))
  record <- c(seq_len(n_e), sec_record, covid_record)
  position <- c(rep(1, n_e), sequence(diag_count - 1L) + 1, covid_position - 0.5)
  code <- c(ep_primary, sec_code, .hes_sample(covid_codes, length(covid_record)))
  o <- order(record, position)
  diag <- .hes_codes_wide(record[o], code[o], n_e, 12L, "DIAG_4_")

  # 7. procedures by age, sex and admission type
  admission_i <- match(sp$admission[s], c("emergency", "waitlist", "planned", "other"))
  proc_key <- ep_key * 4L + admission_i - 1L
  proc_prob <- function(k, with_none) {
    p <- .hes_procedure_prob(profile, (k %/% 4L) %/% 2L, (k %/% 4L) %% 2L == 1L,
                             c("emergency", "waitlist", "planned", "other")[k %% 4L + 1L])
    if (!with_none) {
      p[profile$procedure$codes == "-"] <- 0
      p <- p / sum(p)
    }
    p
  }
  first_proc <- .hes_sample_by_group(proc_key, profile$procedure$codes, function(k) proc_prob(k, TRUE))
  has_proc <- first_proc != "-"
  proc_count_rule <- rules$OPERTN_COUNT
  counts <- !is.na(proc_count_rule$values)
  n_proc <- rep(0L, n_e)
  n_proc[has_proc] <- as.integer(.hes_sample(list(values = proc_count_rule$values[counts],
                                                  prob = proc_count_rule$prob[counts]), sum(has_proc)))
  extra_record <- rep(which(has_proc), pmax(n_proc[has_proc] - 1L, 0L))
  extra_code <- .hes_sample_by_group(proc_key[extra_record], profile$procedure$codes,
                                     function(k) proc_prob(k, FALSE))
  record <- c(which(has_proc), extra_record)
  position <- c(rep(1, sum(has_proc)), sequence(pmax(n_proc[has_proc] - 1L, 0L)) + 1)
  o <- order(record, position)
  proc <- .hes_codes_wide(record[o], c(first_proc[has_proc], extra_code)[o], n_e, 24L, "OPERTN_4_")
  dash_share <- profile$no_procedure_dash[["apc"]]

  # 8. assemble the columns the model owns
  out <- list()
  out$SUBJECT_ID <- people$SUBJECT_ID[person]
  out$EPIKEY <- rep(NA_character_, n_e)  # assigned once all chunks are combined
  out$ADMIDATE <- sp$admit[s]
  out$EPISTART <- episode_start
  out$EPIEND <- episode_end
  out$DISDATE <- sp$admit[s] + sp$duration[s]
  out$EPIDUR <- episode_end - episode_start
  out$SPELDUR <- ifelse(last, sp$duration[s], NA)
  out$EPIORDER <- order_in_spell
  out$SPELBGIN <- ifelse(first, "2", "0")
  out$SPELEND <- ifelse(last, "Y", "N")
  out$FAE <- ifelse(first, "1", "0")
  out$FAE_EMERGENCY <- ifelse(first & sp$admission[s] == "emergency", "1", "0")
  out$FCE <- rep("1", n_e)
  out$EPISTAT <- rep("3", n_e)
  out$EPITYPE <- ifelse(grepl("^O(6[0-9]|7[0-5]|8[0-4])", ep_primary), "2",
                        ifelse(grepl("^Z38", ep_primary), "3", "1"))
  out$CLASSPAT <- ifelse(sp$day_case[s], ifelse(grepl("^Z(49|51)", ep_primary), "3", "2"), "1")
  out$ADMIMETH <- sp$ADMIMETH[s]
  out$ADMISORC <- .hes_sample(profile$admission_source, n_sp)[s]
  elective_wait <- ifelse(sp$admission %in% c("waitlist", "planned"),
                          .hes_from_quantiles(profile$elective_wait, n_sp), NA)
  out$ELECDUR <- elective_wait[s]
  discharge_method <- .hes_sample(profile$discharge_method, n_sp)
  discharge_destination <- .hes_sample(profile$discharge_destination, n_sp)
  out$DISMETH <- ifelse(!last, "8", ifelse(sp$died[s], "4", discharge_method[s]))
  out$DISDEST <- ifelse(!last, "98", ifelse(sp$died[s], "79", discharge_destination[s]))
  admit_age <- (sp$admit[s] - people$birth[person]) / 365.25
  out$ADMIAGE <- .hes_age_code(admit_age)
  out$STARTAGE <- .hes_age_code(ep_age)
  out$STARTAGE_CALC <- ifelse(ep_age < 1, round(ep_age, 3), floor(ep_age))
  out$SEX <- people$sex[person]
  out$ETHNOS <- .hes_record_ethnicity(people$ethnicity[person], profile)
  out$PARTYEAR <- .hes_partyear(as.Date(episode_end, origin = "1970-01-01"))
  for (j in seq_len(12)) {
    out[[sprintf("DIAG_4_%02d", j)]] <- diag$columns[, j]
    out[[sprintf("DIAG_3_%02d", j)]] <- substr(diag$columns[, j], 1, 3)
  }
  out$DIAG_4_CONCAT <- diag$concat
  out$DIAG_3_CONCAT <- gsub("([A-Z][0-9]{2})[0-9A-Z]", "\\1", diag$concat)
  out$DIAG_COUNT <- diag$count
  none <- !has_proc
  none_code <- ifelse(stats::runif(n_e) < dash_share, "-", "-   ")
  for (j in seq_len(24)) {
    code4 <- proc$columns[, j]
    if (j == 1) code4[none] <- none_code[none]
    out[[sprintf("OPERTN_4_%02d", j)]] <- code4
    out[[sprintf("OPERTN_3_%02d", j)]] <- ifelse(code4 %in% c("-", "-   "), "-", substr(code4, 1, 3))
    out[[sprintf("OPERTN_%02d", j)]] <- ifelse(code4 %in% c("-", "-   "), "-", code4)
    op_date <- episode_start + floor(stats::runif(n_e) * (out$EPIDUR + 1))
    op_date[is.na(proc$columns[, j])] <- NA
    out[[sprintf("OPDATE_%02d", j)]] <- op_date
  }
  out$OPERTN_4_CONCAT <- ifelse(none, "-", proc$concat)
  out$OPERTN_3_CONCAT <- ifelse(none, "-", gsub("([A-Z][0-9]{2})[0-9]", "\\1", proc$concat))
  out$OPERTN_COUNT <- ifelse(none, NA, proc$count)

  # 9. every other column from REACT or artificial-HES value frequencies
  types <- stats::setNames(profile$schema$apc$type, profile$schema$apc$column)
  out <- .hes_fill_remaining(out, rules, types, n_e, person, s, episode_start,
                             maternity = out$EPITYPE %in% c("2", "3"))
  list(data = out, death_day = death_day)
}

# HES ethnicity per record: the person's code, with REACT's share of records
# coded "not stated" (Z) or "not known" (99)
.hes_record_ethnicity <- function(person_code, profile) {
  ns <- profile$ethnicity$not_stated
  u <- stats::runif(length(person_code))
  out <- person_code
  out[u < ns[["Z"]]] <- "Z"
  out[u >= ns[["Z"]] & u < ns[["Z"]] + ns[["99"]]] <- "99"
  out
}
