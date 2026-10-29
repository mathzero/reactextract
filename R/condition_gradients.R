# Age and sex gradients for reported pre-existing conditions ------------------

# Checksum of the gradient table. It is built by
# scripts/condition_age_gradients/build.R from public prevalence statistics
# only, and its provenance is recorded in
# inst/extdata/condition-age-gradients-v1-provenance.csv. See
# inst/CONDITION_AGE_GRADIENTS.md.
.condition_gradients_sha256 <- "7d9cbcec9dcfe1c49723010081431dd891192cac51cc41447e299c41a57d8717"

.condition_gradients_release <- "condition-age-gradients-v1"

# Reported prevalence below this is treated as this, so that a zero printed for
# a small age band still has finite log-odds.
.condition_prevalence_floor <- 0.05

# Upper limit for the shared tendency to report conditions (standard deviation
# on the logit scale).
.condition_shared_sd_max <- 4

.condition_concept_prefix <- "health.preexisting."

.condition_gradients <- function() {
  if (is.null(.reactextract_env$condition_gradients)) {
    path <- system.file("extdata", "condition-age-gradients-v1.rds", package = "reactextract")
    if (!nzchar(path) || !identical(.sha256_file(path), .condition_gradients_sha256)) {
      stop("Condition age-gradient checksum mismatch.", call. = FALSE)
    }
    provenance_path <- system.file(
      "extdata", "condition-age-gradients-v1-provenance.csv", package = "reactextract"
    )
    provenance <- if (nzchar(provenance_path)) .read_literal_csv(provenance_path) else data.frame()
    if (nrow(provenance) != 1L ||
        !all(c("release", "sha256", "data_origin") %in% names(provenance)) ||
        provenance$release[[1L]] != .condition_gradients_release ||
        provenance$sha256[[1L]] != .condition_gradients_sha256 ||
        provenance$data_origin[[1L]] != "public_statistics_only") {
      stop("The condition age-gradient provenance record is invalid.", call. = FALSE)
    }
    gradients <- readRDS(path)
    .validate_condition_gradients(gradients)
    .reactextract_env$condition_gradients <- gradients
  }
  .reactextract_env$condition_gradients
}

.validate_condition_gradients <- function(x) {
  figures <- x$figures
  conditions <- x$conditions
  required <- c("condition", "sex", "age_lower", "age_upper", "prevalence_percent", "source_id")
  if (!identical(x$release, .condition_gradients_release) ||
      !is.data.frame(figures) || !all(required %in% names(figures)) ||
      !is.data.frame(x$sources) || anyDuplicated(x$sources$source_id) ||
      !all(figures$source_id %in% x$sources$source_id) ||
      !is.data.frame(conditions) || anyDuplicated(conditions$condition) ||
      !all(conditions$gradient %in% c("age_and_sex", "age", "none"))) {
    stop("Invalid condition age-gradient table.", call. = FALSE)
  }
  # Every condition with figures has an age gradient, and an age and sex
  # gradient has figures for men and women
  graded <- conditions$condition[conditions$gradient != "none"]
  by_sex <- conditions$condition[conditions$gradient == "age_and_sex"]
  if (!setequal(unique(figures$condition), graded) ||
      !all(vapply(by_sex, function(condition) {
        all(c("male", "female") %in% figures$sex[figures$condition == condition])
      }, logical(1L)))) {
    stop("Condition age-gradient figures do not match their conditions.", call. = FALSE)
  }
  if (!all(figures$sex %in% c("male", "female", "all")) ||
      anyNA(figures$age_lower) || anyNA(figures$prevalence_percent) ||
      any(figures$prevalence_percent < 0 | figures$prevalence_percent >= 100)) {
    stop("Invalid condition age-gradient figures.", call. = FALSE)
  }
  for (group in split(figures, paste(figures$condition, figures$sex))) {
    group <- group[order(group$age_lower), , drop = FALSE]
    upper <- group$age_upper
    # Bands are whole years, contiguous, and the last is open-ended
    if (!is.na(upper[[length(upper)]]) || anyNA(upper[-length(upper)]) ||
        any(group$age_lower[-1L] != upper[-length(upper)] + 1)) {
      stop("Condition age bands must be contiguous and end open: ",
           group$condition[[1L]], ", ", group$sex[[1L]], ".", call. = FALSE)
    }
  }
  invisible(TRUE)
}

# Reported prevalence (percent) for one condition at each age and sex. Ages
# below the youngest published band take that band's figure. Sex is "male",
# "female" or NA; NA uses all persons, or the mean of men and women when the
# source gives no all-persons figure.
.condition_prevalence <- function(figures, condition, age, sex) {
  rows <- figures[figures$condition == condition, , drop = FALSE]
  lookup <- function(band, at) {
    band <- band[order(band$age_lower), , drop = FALSE]
    index <- findInterval(at, band$age_lower)
    band$prevalence_percent[pmax(index, 1L)]
  }
  curve <- function(which) {
    if (any(rows$sex == which)) return(lookup(rows[rows$sex == which, ], age))
    if (which == "all") {
      return((lookup(rows[rows$sex == "male", ], age) +
                lookup(rows[rows$sex == "female", ], age)) / 2)
    }
    NULL
  }
  all_persons <- curve("all")
  value <- all_persons
  for (which in c("male", "female")) {
    specific <- curve(which)
    if (!is.null(specific)) {
      selected <- !is.na(sex) & sex == which
      value[selected] <- specific[selected]
    }
  }
  pmax(value, .condition_prevalence_floor)
}

.condition_block <- function(occurrences) {
  block <- occurrences[startsWith(occurrences$primary_concept_id, .condition_concept_prefix), , drop = FALSE]
  block$condition <- substring(block$primary_concept_id, nchar(.condition_concept_prefix) + 1L)
  block
}

# Synthetic conditions depend on age and sex, and on each other through
# "none of these". Whenever one is requested, generate the whole block with its
# age and sex context, so the values do not depend on which families or
# concepts were selected.
.condition_context_occurrences <- function(dictionary, selected, requested_rounds) {
  rounds <- intersect(unique(.condition_block(selected)$round_id), requested_rounds)
  occurrences <- dictionary$occurrences[dictionary$occurrences$round_id %in% rounds, , drop = FALSE]
  occurrences[startsWith(occurrences$primary_concept_id, .condition_concept_prefix) |
                occurrences$variable %in% c("U_AGE", "U_GENDER"), , drop = FALSE]
}

# Equal-probability nodes for a standard normal shared term
.condition_normal_nodes <- function(count = 32L) {
  stats::qnorm((seq_len(count) - 0.5) / count)
}

# Logit, given the shared term, whose average over the shared term is
# `marginal`. Newton steps for every element at once; exact when `shared_sd` is 0.
.condition_conditional_logit <- function(marginal, shared_sd, nodes = .condition_normal_nodes()) {
  logit <- stats::qlogis(marginal)
  if (shared_sd == 0) return(logit)
  for (iteration in seq_len(100L)) {
    p <- stats::plogis(outer(as.vector(logit), shared_sd * nodes, `+`))
    step <- (rowMeans(p) - as.vector(marginal)) / rowMeans(p * (1 - p))
    step <- pmax(pmin(step, 2), -2)
    logit[] <- as.vector(logit) - step
    if (max(abs(step)) < 1e-10) break
  }
  logit
}

# Calibrate one round's condition block. Averaged over the shared term, each
# condition's odds in every age-sex cell (`offsets`: logit of the published
# prevalence, one row per cell with `weights` people) keep the published ratios
# to every other cell, and its prevalence over the round's synthetic people
# equals the approved profile (`targets`). The shared term's standard deviation
# then makes the share reporting no condition equal the profile's "none of
# these" (`none_target`). Returns the logit for each cell and condition given
# the shared term, its standard deviation, the share expected to report no
# condition and the probability of ticking "none of these" when none is
# reported.
.calibrate_condition_block <- function(offsets, weights, targets, none_target = NA_real_) {
  nodes <- .condition_normal_nodes()
  weights <- weights / sum(weights)
  marginal <- vapply(seq_along(targets), function(k) {
    gap <- function(a) sum(weights * stats::plogis(a + offsets[, k])) - targets[[k]]
    stats::plogis(stats::uniroot(gap, c(-50, 50), tol = 1e-12)$root + offsets[, k])
  }, numeric(nrow(offsets)))
  marginal <- matrix(marginal, nrow = nrow(offsets), dimnames = dimnames(offsets))
  none_rate <- function(logits, shared_sd) {
    none <- matrix(1, nrow(offsets), length(nodes))
    for (k in seq_along(targets)) {
      none <- none * (1 - stats::plogis(outer(logits[, k], shared_sd * nodes, `+`)))
    }
    sum(weights * rowMeans(none))
  }
  result <- function(shared_sd) {
    logits <- .condition_conditional_logit(marginal, shared_sd, nodes)
    all_clear <- none_rate(logits, shared_sd)
    list(
      logits = logits, shared_sd = shared_sd, all_clear = all_clear,
      none_scale = if (is.na(none_target)) NA_real_ else min(1, none_target / all_clear)
    )
  }
  independent <- result(0)
  if (is.na(none_target) || independent$all_clear >= none_target) return(independent)
  gap <- function(shared_sd) result(shared_sd)$all_clear - none_target
  if (gap(.condition_shared_sd_max) <= 0) return(result(.condition_shared_sd_max))
  result(stats::uniroot(gap, c(0, .condition_shared_sd_max), tol = 1e-6)$root)
}

.condition_probability_one <- function(source, occurrence) {
  support <- .synthetic_support(source, occurrence)
  if (is.null(support) || !setequal(support$values, c("0", "1"))) return(NA_real_)
  support$probability[support$values == "1"]
}

# Redraw the reported pre-existing conditions of one round so that they depend
# on age and sex. Each condition keeps the probability the approved profile
# gives it, solved per round against the synthetic age and sex distribution.
# Every condition box shares the questionnaire state of "none of these", so a
# person who did not answer the question has the same missing code in every
# box; "none of these" is ticked only when no condition is. Missing values and
# codes are never filled in.
.apply_condition_age_gradients <- function(data, occurrences, source, dictionary) {
  empty <- list(data = data, issues = .empty_issues(), summary = character())
  block <- .condition_block(occurrences)
  block <- block[block$variable %in% names(data), , drop = FALSE]
  if (!nrow(block)) return(empty)
  round_id <- block$round_id[[1L]]
  block$target <- vapply(seq_len(nrow(block)), function(index) {
    .condition_probability_one(source, block[index, , drop = FALSE])
  }, numeric(1L))
  block <- block[!is.na(block$target), , drop = FALSE]
  anchor <- block[block$condition == "none_of_these", , drop = FALSE]
  modelled <- block[block$condition != "none_of_these", , drop = FALSE]
  if (!nrow(modelled)) return(empty)
  gradients <- .condition_gradients()

  n <- nrow(data)
  answered_in <- function(variable) {
    value <- suppressWarnings(as.numeric(data[[variable]]))
    !is.na(value) & value %in% c(0, 1)
  }
  if (nrow(anchor)) {
    answered <- answered_in(anchor$variable[[1L]])
    for (variable in modelled$variable) {
      data[[variable]][!answered] <- data[[anchor$variable[[1L]]]][!answered]
    }
  }

  age <- if ("U_AGE" %in% names(data)) suppressWarnings(as.numeric(data$U_AGE)) else rep(NA_real_, n)
  age[!is.na(age) & age < 0] <- NA
  gender <- if ("U_GENDER" %in% names(data)) as.character(data$U_GENDER) else rep(NA_character_, n)
  sex <- ifelse(gender %in% "1", "male", ifelse(gender %in% "2", "female", NA_character_))
  cell <- paste(ifelse(is.na(age), "unknown", floor(age)), ifelse(is.na(sex), "unknown", sex))

  # Logit of the published prevalence for each age-sex cell of `rows`, with the
  # number of people in each cell. Unknown ages take the cell-weighted mean;
  # conditions without figures have no gradient, and an age-only gradient
  # ignores sex.
  gradient <- gradients$conditions$gradient[match(modelled$condition, gradients$conditions$condition)]
  gradient[is.na(gradient)] <- "none"
  offsets_for <- function(rows) {
    cells <- unique(cell[rows])
    first <- match(cells, cell)
    weights <- tabulate(match(cell[rows], cells), length(cells))
    known <- !is.na(age[first])
    offsets <- vapply(seq_len(nrow(modelled)), function(k) {
      prevalence <- rep(50, length(cells))
      if (gradient[[k]] != "none" && any(known)) {
        cell_sex <- if (gradient[[k]] == "age_and_sex") sex[first][known] else NA_character_
        prevalence[known] <- .condition_prevalence(
          gradients$figures, modelled$condition[[k]], age[first][known], cell_sex
        )
        prevalence[!known] <- stats::weighted.mean(prevalence[known], weights[known])
      }
      stats::qlogis(prevalence / 100)
    }, numeric(length(cells)))
    list(
      offsets = matrix(offsets, nrow = length(cells), dimnames = list(cells, modelled$condition)),
      weights = weights
    )
  }

  shared <- .with_stream_seed(source$seed, round_id, "health.preexisting::shared_tendency", stats::rnorm(n))
  uniform <- function(occurrence_id) {
    .with_stream_seed(source$seed, round_id, paste0(occurrence_id, "::age_gradient"), stats::runif(n))
  }
  # Draw condition `k` for `rows` from column `column` of the fitted logits
  draw <- function(rows, k, fit, column = k) {
    eta <- fit$logits[match(cell[rows], rownames(fit$logits)), column] + fit$shared_sd * shared[rows]
    as.numeric(uniform(modelled$occurrence_id[[k]])[rows] < stats::plogis(eta))
  }
  issues <- .empty_issues()

  if (nrow(anchor)) {
    rows <- which(answered)
    if (!length(rows)) return(list(data = data, issues = issues, summary = character()))
    cells <- offsets_for(rows)
    fit <- .calibrate_condition_block(cells$offsets, cells$weights, modelled$target, anchor$target[[1L]])
    any_condition <- rep(FALSE, length(rows))
    for (k in seq_len(nrow(modelled))) {
      value <- draw(rows, k, fit)
      data[[modelled$variable[[k]]]][rows] <- .coerce_like(value, data[[modelled$variable[[k]]]])
      any_condition <- any_condition | value == 1
    }
    none <- as.numeric(!any_condition & uniform(anchor$occurrence_id[[1L]])[rows] < fit$none_scale)
    data[[anchor$variable[[1L]]]][rows] <- .coerce_like(none, data[[anchor$variable[[1L]]]])
    if (fit$all_clear < anchor$target[[1L]] - 1e-6) {
      issues <- .issue(
        "warning", "synthetic", "condition_none_below_profile",
        paste(
          "Too few people report no condition for 'none of these' to keep its",
          "profile prevalence; it is ticked by everyone who reports no condition."
        ),
        round_id, "synthetic", anchor$variable[[1L]]
      )
    }
    summary <- sprintf(
      "%s:conditions=%d:shared_sd=%.3f:none_target=%.4f:none_expected=%.4f",
      round_id, nrow(modelled), fit$shared_sd, anchor$target[[1L]],
      fit$all_clear * fit$none_scale
    )
    return(list(data = data, issues = issues, summary = summary))
  }

  # Without "none of these", each condition is calibrated on the people who
  # answered it and no shared tendency is needed
  for (k in seq_len(nrow(modelled))) {
    rows <- which(answered_in(modelled$variable[[k]]))
    if (!length(rows)) next
    cells <- offsets_for(rows)
    fit <- .calibrate_condition_block(cells$offsets[, k, drop = FALSE], cells$weights, modelled$target[[k]])
    value <- draw(rows, k, fit, column = 1L)
    data[[modelled$variable[[k]]]][rows] <- .coerce_like(value, data[[modelled$variable[[k]]]])
  }
  list(
    data = data, issues = issues,
    summary = sprintf("%s:conditions=%d:shared_sd=0.000:none_target=NA", round_id, nrow(modelled))
  )
}
