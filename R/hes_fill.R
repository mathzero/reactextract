# HES sampling helpers and generic column filling ---------------------------------

.hes_sample <- function(dist, n) {
  if (n == 0) return(dist$values[0])
  dist$values[sample.int(length(dist$values), n, replace = TRUE, prob = dist$prob)]
}

# sample one code per element, with weights depending on the element's group
.hes_sample_by_group <- function(group, values, prob_fun) {
  out <- rep(NA_character_, length(group))
  for (idx in split(seq_along(group), group)) {
    p <- prob_fun(group[idx[1]])
    out[idx] <- values[sample.int(length(values), length(idx), replace = TRUE, prob = p)]
  }
  out
}

# draw from a distribution known only at the 5th, 25th, 50th, 75th and 95th
# percentiles: linear between them, with short linear tails
.hes_from_quantiles <- function(q, n) {
  lower <- max(0, q[1] - (q[2] - q[1]) * 0.25)
  upper <- q[5] + (q[5] - q[4]) * 2
  x <- stats::approx(c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), c(lower, q, upper),
                     xout = stats::runif(n), ties = "ordered")$y
  if (all(q == round(q))) x <- round(x)
  x
}

# random strings with the shape of a pattern: 9 = digit, A = letter, a = lower case
.hes_from_pattern <- function(patterns) {
  out <- patterns
  for (p in unique(patterns)) {
    idx <- which(patterns == p)
    chars <- strsplit(p, "")[[1]]
    cols <- lapply(chars, function(ch) {
      switch(ch,
             "9" = as.character(sample.int(10, length(idx), replace = TRUE) - 1L),
             "A" = sample(LETTERS, length(idx), replace = TRUE),
             "a" = sample(letters, length(idx), replace = TRUE),
             rep(ch, length(idx)))
    })
    out[idx] <- do.call(paste0, cols)
  }
  out
}

# values for n records from one fill rule (before applying the record level)
.hes_fill_values <- function(rule, n) {
  x <- switch(rule$method,
              values = .hes_sample(rule, n),
              quantiles = .hes_from_quantiles(rule$q, n),
              pattern = .hes_from_pattern(.hes_sample(rule, n)),
              rep(NA, n))
  # REACT value frequencies already include NULLs; other sources need the
  # REACT completeness applied
  apply_completeness <- !(rule$method == "values" && rule$source == "react") &&
    !is.na(rule$completeness) && rule$completeness < 1
  if (apply_completeness) x[stats::runif(n) > rule$completeness] <- NA
  x
}

# Fill every column of `out` that is still NULL from the profile's rules.
# `person` and `group` index each record's person and spell/course, so values
# at those levels repeat across a person's or a spell's records.
.hes_fill_remaining <- function(out, rules, types, n_rows, person, group, main_date,
                               maternity = NULL) {
  maternity_columns <- "^(BIR|DEL|GESTAT|NUMBABY|NEOCARE|NEODUR|HNEOIND)"
  for (column in names(rules)) {
    if (!is.null(out[[column]])) next
    rule <- rules[[column]]
    if (types[[column]] == "DATE") {
      # other dates (e.g. RTT periods) are anchored on the record's main date
      x <- main_date
      completeness <- if (is.na(rule$completeness)) 0 else rule$completeness
      x[stats::runif(n_rows) > completeness] <- NA
      out[[column]] <- x
      next
    }
    if (!is.null(maternity) && grepl(maternity_columns, column)) {
      # birth and delivery details belong to maternity episodes only
      x <- rep(NA, n_rows)
      if (any(maternity)) {
        if (rule$method == "values") {
          known <- !is.na(rule$values)
          if (any(known)) {
            x[maternity] <- .hes_sample(list(values = rule$values[known], prob = rule$prob[known]),
                                        sum(maternity))
          }
        } else {
          x[maternity] <- .hes_fill_values(c(rule[setdiff(names(rule), "completeness")],
                                             completeness = 1), sum(maternity))
        }
      }
      out[[column]] <- x
      next
    }
    out[[column]] <- switch(rule$level,
                            person = .hes_fill_values(rule, max(person))[person],
                            group = .hes_fill_values(rule, max(group))[group],
                            .hes_fill_values(rule, n_rows))
  }
  out
}

# HES age fields: completed years, with codes 7001-7007 for infants
.hes_age_code <- function(age_years) {
  days <- age_years * 365.25
  code <- floor(age_years)
  infant <- !is.na(age_years) & age_years < 1
  code[infant] <- 7000 + findInterval(days[infant], c(0, 1, 7, 29, 91, 183, 274))
  code
}

# financial year of a date as HES PARTYEAR (e.g. "201999" for 2019-20)
.hes_partyear <- function(date) {
  lt <- as.POSIXlt(date)
  start_year <- lt$year + 1900 - (lt$mon < 3)
  ifelse(is.na(date), NA_character_, paste0(start_year, "99"))
}

# a count that rises with age while keeping its overall distribution
.hes_count_by_age <- function(dist, age, rho = 0.4) {
  values <- as.numeric(dist$values)
  keep <- !is.na(values)
  o <- order(values[keep])
  values <- values[keep][o]
  prob <- dist$prob[keep][o] / sum(dist$prob[keep])
  z <- rho * (age - 60) / 18 + sqrt(1 - rho^2) * stats::rnorm(length(age))
  values[pmin(findInterval(stats::pnorm(z), cumsum(prob)) + 1L, length(values))]
}

# long-format codes (record, code) to one row per record: first `width` codes in
# columns, plus a comma-separated list and a count; repeated codes are dropped
.hes_codes_wide <- function(record, code, n_records, width, prefix) {
  keep <- !duplicated(paste(record, code)) & !is.na(code)
  record <- record[keep]
  code <- code[keep]
  o <- order(record)
  record <- record[o]
  code <- code[o]
  position <- stats::ave(seq_along(record), record, FUN = seq_along)
  m <- matrix(NA_character_, n_records, width)
  in_width <- position <= width
  m[cbind(record[in_width], position[in_width])] <- code[in_width]
  colnames(m) <- sprintf("%s%02d", prefix, seq_len(width))
  concat <- rep(NA_character_, n_records)
  joined <- tapply(code, record, paste, collapse = ",")
  concat[as.integer(names(joined))] <- joined
  count <- tabulate(record, nbins = n_records)
  list(columns = m, concat = concat, count = count, record = record, position = position)
}

.hes_unique_keys <- function(n, digits = 12) {
  base <- floor(stats::runif(1, 1, 9) * 10^(digits - 1))
  sprintf(paste0("%0", digits, ".0f"), base + sample.int(n * 10L, n))
}
