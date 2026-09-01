.dependency_outcome_levels <- function(outcome_id = NULL) {
  if (identical(outcome_id, "react2_igg_positive")) {
    c("negative", "positive", "non_evaluable", "missing")
  } else {
    c("negative", "positive", "missing")
  }
}

.dependency_split_ids <- function(value) {
  if (length(value) != 1L || is.na(value) || !nzchar(value)) return(character())
  unique(strsplit(value, "|", fixed = TRUE)[[1L]])
}

.dependency_specs <- function(dictionary, rounds = NULL) {
  specs <- dictionary$synthetic_dependencies
  if (!is.data.frame(specs) || nrow(specs) == 0L) return(data.frame())
  required <- c(
    "dependency_id", "outcome_id", "predictor_id", "round_ids",
    "source_fields", "derivation_rule_id", "levels", "review_state"
  )
  if (!all(required %in% names(specs))) {
    stop("The pinned synthetic dependency contract is incomplete.", call. = FALSE)
  }
  specs <- specs[specs$review_state == "approved", , drop = FALSE]
  expanded <- lapply(seq_len(nrow(specs)), function(index) {
    row <- specs[index, , drop = FALSE]
    scope <- .dependency_split_ids(row$round_ids[[1L]])
    if (!is.null(rounds)) scope <- intersect(scope, rounds)
    if (!length(scope)) return(NULL)
    rows <- row[rep(1L, length(scope)), , drop = FALSE]
    rows$round_id <- scope
    rows
  })
  .bind_rows(expanded, specs[0, , drop = FALSE])
}

.dependency_occurrences <- function(spec, dictionary) {
  round_id <- spec$round_id[[1L]]
  field_specs <- .dependency_split_ids(spec$source_fields[[1L]])
  available <- dictionary$occurrences[
    dictionary$occurrences$round_id == round_id, , drop = FALSE
  ]
  variables <- unique(unlist(lapply(field_specs, function(field) {
    if (endsWith(field, "*")) {
      prefix <- substr(field, 1L, nchar(field) - 1L)
      available$variable[startsWith(available$variable, prefix)]
    } else {
      field[field %in% available$variable]
    }
  }), use.names = FALSE))
  selected <- available[match(variables, available$variable), , drop = FALSE]
  # Wildcard field families include free-text "Other" write-ins in several
  # rounds. Dependency profiling uses only reviewed numeric option fields and
  # never requests respondent text from the enclave source.
  selected[selected$data_type == "NUMBER", , drop = FALSE]
}

.dependency_outcome_variables <- function(round_id, outcome_id) {
  if (identical(outcome_id, "react1_pcr_positive")) {
    number <- suppressWarnings(as.integer(sub("^react1[.]r", "", round_id)))
    if (is.na(number) || number < 1L || number > 19L) return(character())
    if (number == 1L) return(c("RESULT", "LAB", "CT_VALUE1", "CT_VALUE2"))
    if (number == 5L) return("FINALRESULT")
    if (number <= 7L) return(c("RESULT", "CT_VALUE1", "CT_VALUE2"))
    return(c("RESULT", "NGENE_CTVALUE", "EGENE_CTVALUE"))
  }
  if (identical(outcome_id, "react2_igg_positive")) {
    if (identical(round_id, "react2.r01")) {
      return(c("NEWRESULT", "NEWRESULT_2"))
    }
    return("NEWRESULT")
  }
  stop("Unsupported approved synthetic outcome: ", outcome_id, ".",
       call. = FALSE)
}

.dependency_lab_result_missing <- function(value, round_id = NULL,
                                           variable = NULL,
                                           dictionary = react_dictionary()) {
  if (is.null(round_id) || is.null(variable)) {
    text <- as.character(value)
    return(is.na(value) | text %in% c(
      .lab_result_common_admin_codes(), " ", "Void"
    ))
  }
  .lab_result_missing(value, round_id, variable, dictionary)
}

.dependency_lab_result_unknown <- function(value, round_id = NULL,
                                           variable = NULL,
                                           dictionary = react_dictionary()) {
  if (is.null(round_id) || is.null(variable)) {
    text <- as.character(value)
    return(!.dependency_lab_result_missing(value) &
      !(text %in% c("Detected", "Not Detected")))
  }
  .lab_result_unknown(value, round_id, variable, dictionary)
}

.dependency_legacy_ct_positive <- function(result, ct1, ct2, round_id,
                                           variable = "RESULT",
                                           dictionary = react_dictionary()) {
  result_state <- .lab_result_field_state(
    result, round_id, variable, dictionary
  )
  result_state[result_state == "unknown"] <- "missing"
  ct1 <- suppressWarnings(as.numeric(ct1))
  ct2 <- suppressWarnings(as.numeric(ct2))
  ifelse(
    result_state == "missing", "missing",
    ifelse(
      result_state == "negative", "negative",
      ifelse(
        result_state == "detected",
        ifelse(
          ct1 > 0 & ct2 > 0, "positive",
          ifelse(ct1 > 0 & ct1 < 37, "positive", "negative")
        ),
        "missing"
      )
    )
  )
}

.dependency_react1_outcome <- function(data, round_id,
                                       dictionary = react_dictionary()) {
  number <- suppressWarnings(as.integer(sub("^react1[.]r", "", round_id)))
  n <- nrow(data)
  if (number == 1L) {
    result_state <- .lab_result_field_state(
      data$RESULT, round_id, "RESULT", dictionary
    )
    result_state[result_state == "unknown"] <- "missing"
    lab <- as.character(data$LAB)
    ct1 <- suppressWarnings(as.numeric(data$CT_VALUE1))
    ct2 <- suppressWarnings(as.numeric(data$CT_VALUE2))
    return(ifelse(
      result_state == "missing", "missing",
      ifelse(
        result_state == "negative", "negative",
        ifelse(
          result_state == "detected" & lab != "Eurofin", "positive",
          ifelse(
            ct1 > 0 & ct2 > 0, "positive",
            ifelse(ct1 > 0 & ct1 < 37, "positive", "negative")
          )
        )
      )
    ))
  }
  if (number == 5L) {
    result_state <- .lab_result_field_state(
      data$FINALRESULT, round_id, "FINALRESULT", dictionary
    )
    result_state[result_state == "unknown"] <- "missing"
    return(ifelse(
      result_state == "detected", "positive",
      ifelse(result_state == "negative", "negative", "missing")
    ))
  }
  fields <- if (number <= 7L) {
    c("RESULT", "CT_VALUE1", "CT_VALUE2")
  } else {
    c("RESULT", "NGENE_CTVALUE", "EGENE_CTVALUE")
  }
  if (!all(fields %in% names(data))) return(rep("missing", n))
  .dependency_legacy_ct_positive(
    data[[fields[[1L]]]], data[[fields[[2L]]]], data[[fields[[3L]]]],
    round_id = round_id, variable = fields[[1L]], dictionary = dictionary
  )
}

.dependency_react2_outcome <- function(data, round_id) {
  if ("NEWRESULT" %in% names(data)) {
    result <- suppressWarnings(as.numeric(data$NEWRESULT))
    return(ifelse(result %in% c(2, 3), "positive",
                  ifelse(result %in% c(0, 1), "negative",
                    ifelse(result %in% 4:7, "non_evaluable", "missing"))))
  }
  if (identical(round_id, "react2.r01") && "NEWRESULT_2" %in% names(data)) {
    result <- suppressWarnings(as.numeric(data$NEWRESULT_2))
    return(ifelse(result == 2, "positive",
                  ifelse(result == 1, "negative",
                    ifelse(result %in% c(3, 4), "non_evaluable", "missing"))))
  }
  rep("missing", nrow(data))
}

.dependency_outcome_state <- function(data, round_id, outcome_id,
                                      dictionary = react_dictionary()) {
  if (identical(outcome_id, "react1_pcr_positive")) {
    result <- .dependency_react1_outcome(data, round_id, dictionary)
    result[is.na(result)] <- "missing"
    return(result)
  }
  if (identical(outcome_id, "react2_igg_positive")) {
    result <- .dependency_react2_outcome(data, round_id)
    result[is.na(result)] <- "missing"
    return(result)
  }
  stop("Unsupported approved synthetic outcome: ", outcome_id, ".",
       call. = FALSE)
}

.dependency_admin_value <- function(value) {
  is.na(value) | as.character(value) %in% c(
    "", "NA", "-91", "-92", "-77", "-66", "-99", "-555"
  )
}

.dependency_map_codes <- function(value, mapping) {
  key <- as.character(value)
  result <- unname(mapping[key])
  result[.dependency_admin_value(value)] <- NA_character_
  result
}

.dependency_public_support <- function(occurrences, dictionary, field = NULL,
                                       fallback = character()) {
  rows <- occurrences
  if (!is.null(field)) rows <- rows[rows$variable == field, , drop = FALSE]
  if (!nrow(rows)) return(unique(as.character(fallback)))
  values <- unique(unlist(lapply(rows$occurrence_id, function(occurrence_id) {
    options <- .profile_occurrence_options(occurrence_id, dictionary)
    if (!nrow(options)) return(character())
    options$return_value[!.dependency_admin_value(options$return_value)]
  }), use.names = FALSE))
  if (!length(values)) values <- fallback
  unique(as.character(values))
}

.dependency_vaccination_map <- function(occurrences, dictionary) {
  support <- .dependency_public_support(occurrences, dictionary)
  mapping <- c(`1` = "yes", `2` = "no",
               `3` = "uncertain_trial_or_unknown")
  if ("4" %in% support) {
    mapping <- c(mapping, `4` = "uncertain_trial_or_unknown")
  }
  mapping
}

.dependency_first_value <- function(data, variables) {
  out <- rep(NA_character_, nrow(data))
  for (variable in variables) {
    if (!(variable %in% names(data))) next
    candidate <- as.character(data[[variable]])
    selected <- is.na(out) & !.dependency_admin_value(candidate)
    out[selected] <- candidate[selected]
  }
  out
}

.dependency_age_band <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  cut(
    value,
    breaks = c(-Inf, 4, 12, 17, 24, 34, 44, 54, 64, 74, 84, Inf),
    labels = c("under_5", "5_12", "13_17", "18_24", "25_34", "35_44",
               "45_54", "55_64", "65_74", "75_84", "85_plus"),
    right = TRUE
  ) |> as.character()
}

.dependency_binary_any <- function(data, variables) {
  available <- variables[variables %in% names(data)]
  if (!length(available)) return(rep("missing", nrow(data)))
  values <- lapply(available, function(variable) {
    suppressWarnings(as.numeric(data[[variable]]))
  })
  matrix <- do.call(cbind, values)
  if (is.null(dim(matrix))) matrix <- matrix(matrix, ncol = 1L)
  administrative <- matrix %in% c(-91, -92, -77, -66, -99, -555)
  dim(administrative) <- dim(matrix)
  observed <- rowSums(!is.na(matrix) & !administrative) > 0L
  selected <- rowSums(matrix == 1, na.rm = TRUE) > 0L
  ifelse(!observed, "missing", ifelse(selected, "yes", "no"))
}

.dependency_count_band <- function(data, variables) {
  available <- variables[variables %in% names(data)]
  if (!length(available)) return(rep("missing", nrow(data)))
  values <- do.call(cbind, lapply(available, function(variable) {
    suppressWarnings(as.numeric(data[[variable]]))
  }))
  if (is.null(dim(values))) values <- matrix(values, ncol = 1L)
  administrative <- values %in% c(-91, -92, -77, -66, -99, -555)
  dim(administrative) <- dim(values)
  observed <- rowSums(!is.na(values) & !administrative) > 0L
  count <- rowSums(values == 1, na.rm = TRUE)
  ifelse(!observed, "missing",
         ifelse(count == 0L, "0", ifelse(count == 1L, "1",
           ifelse(count <= 3L, "2_3", "4_plus"))))
}

.dependency_household_band <- function(data, variables) {
  adult_variables <- variables[grepl("^NADULTS", variables)]
  child_variables <- variables[grepl("^NCHILD", variables)]
  adults <- suppressWarnings(as.numeric(.dependency_first_value(data, adult_variables)))
  children <- suppressWarnings(as.numeric(.dependency_first_value(data, child_variables)))
  total <- adults + children
  total[is.na(adults) | is.na(children) | total < 0] <- NA_real_
  ifelse(is.na(total), "missing",
         ifelse(total <= 1, "one", ifelse(total == 2, "two",
           ifelse(total <= 5, "three_to_five", "six_plus"))))
}

.dependency_checkbox_state <- function(data, variables, labels, classic = FALSE) {
  available <- variables[variables %in% names(data)]
  if (!length(available)) return(rep("missing", nrow(data)))
  selected <- lapply(available, function(variable) {
    value <- suppressWarnings(as.numeric(data[[variable]]))
    !is.na(value) & value == 1
  })
  selected <- do.call(cbind, selected)
  if (is.null(dim(selected))) selected <- matrix(selected, ncol = 1L)
  observed <- lapply(available, function(variable) {
    !.dependency_admin_value(data[[variable]])
  })
  observed <- do.call(cbind, observed)
  if (is.null(dim(observed))) observed <- matrix(observed, ncol = 1L)
  if (!classic) return(ifelse(rowSums(observed) == 0L, "missing", "none"))
  classic_columns <- grepl(
    "persistent cough|loss or change to sense of (smell|taste)|fever",
    labels, ignore.case = TRUE
  )
  none_columns <- grepl("none of these", labels, ignore.case = TRUE)
  any_classic <- if (any(classic_columns)) {
    rowSums(selected[, classic_columns, drop = FALSE]) > 0L
  } else rep(FALSE, nrow(data))
  any_other <- if (any(!classic_columns & !none_columns)) {
    rowSums(selected[, !classic_columns & !none_columns, drop = FALSE]) > 0L
  } else rep(FALSE, nrow(data))
  if ("FEELUN" %in% available) {
    feel <- suppressWarnings(as.numeric(data$FEELUN))
    any_other <- any_other | (!is.na(feel) & feel == 1)
  }
  ifelse(rowSums(observed) == 0L, "missing",
         ifelse(any_classic, "classic", ifelse(any_other, "other", "none")))
}

.dependency_predictor_state <- function(data, spec, dictionary) {
  occurrences <- .dependency_occurrences(spec, dictionary)
  variables <- occurrences$variable
  definition <- spec$derivation_rule_id[[1L]]
  if (definition == "age_band_v1") {
    value <- .dependency_first_value(data, variables)
    result <- .dependency_age_band(value)
  } else if (definition == "imd_quintile_v1") {
    value <- suppressWarnings(as.numeric(.dependency_first_value(data, variables)))
    result <- .dependency_map_codes(value, stats::setNames(
      rep(c("q1_most_deprived", "q2", "q3", "q4", "q5_least_deprived"), each = 2L),
      as.character(1:10)
    ))
  } else if (definition == "household_size_band_v1") {
    result <- .dependency_household_band(data, variables)
  } else if (definition == "gender_v1") {
    value <- suppressWarnings(as.numeric(.dependency_first_value(data, variables)))
    result <- .dependency_map_codes(
      value,
      c(`0` = "not_specified", `1` = "male", `2` = "female",
        `9` = "not_specified")
    )
  } else if (definition == "ethnicity_broad_v1") {
    value <- suppressWarnings(as.numeric(.dependency_first_value(data, variables)))
    support <- .dependency_public_support(occurrences, dictionary)
    mapping <- c(
      stats::setNames(rep("white", 4L), as.character(1:4)),
      stats::setNames(rep("mixed", 4L), as.character(5:8)),
      stats::setNames(rep("asian", 5L), as.character(9:13)),
      stats::setNames(rep("black", 3L), as.character(14:16)),
      `17` = "other", `18` = "other", `19` = "withheld_or_missing",
      `101` = "white", `102` = "mixed", `103` = "asian",
      `104` = "black", `105` = "other"
    )
    if ("20" %in% support) mapping <- c(mapping, `20` = "other")
    result <- .dependency_map_codes(value, mapping)
  } else if (definition == "region_v1") {
    value <- suppressWarnings(as.numeric(.dependency_first_value(data, variables)))
    region <- c("north_east", "north_west", "yorkshire_humber", "east_midlands",
                "west_midlands", "east_of_england", "london", "south_east", "south_west")
    result <- .dependency_map_codes(
      value, stats::setNames(region, as.character(1:9))
    )
  } else if (definition == "economic_activity_broad_v1") {
    value <- suppressWarnings(as.numeric(.dependency_first_value(data, variables)))
    activity <- c(
      "employed", "employed", "self_employed", "education", "unemployed",
      "retired", "education", "home_or_caring", "sick_or_disabled",
      "other_or_withheld", "other_or_withheld"
    )
    result <- .dependency_map_codes(
      value, stats::setNames(activity, as.character(seq_along(activity)))
    )
  } else if (definition == "covid_history_v1") {
    value <- suppressWarnings(as.numeric(.dependency_first_value(data, variables)))
    result <- .dependency_map_codes(value, c(
      `1` = "test_confirmed", `2` = "clinician_suspected",
      `3` = "self_suspected", `4` = "none"
    ))
  } else if (definition == "confirmed_contact_v1") {
    single <- variables[variables == "COVIDCON"]
    if (length(single) && single %in% names(data)) {
      value <- suppressWarnings(as.numeric(data[[single]]))
      result <- .dependency_map_codes(
        value, c(`1` = "confirmed", `2` = "suspected_only", `3` = "none")
      )
    } else {
      result <- rep("missing", nrow(data))
      confirmed <- variables[grepl("_1$", variables)]
      suspected <- variables[grepl("_2$", variables)]
      none <- variables[grepl("_3$", variables)]
      observed <- unique(c(confirmed, suspected, none))
      if (length(observed)) {
        any_observed <- Reduce(`|`, lapply(observed, function(x) !.dependency_admin_value(data[[x]])))
        is_selected <- function(fields) {
          if (!length(fields)) return(rep(FALSE, nrow(data)))
          Reduce(`|`, lapply(fields, function(x) suppressWarnings(as.numeric(data[[x]])) == 1))
        }
        result <- ifelse(!any_observed, "missing", ifelse(is_selected(confirmed), "confirmed",
          ifelse(is_selected(suspected), "suspected_only", "none")))
      }
    }
  } else if (definition == "classic_symptom_status_v1") {
    result <- .dependency_checkbox_state(data, variables, occurrences$label, classic = TRUE)
  } else if (definition == "vaccination_status_v1") {
    value <- suppressWarnings(as.numeric(.dependency_first_value(data, variables)))
    result <- .dependency_map_codes(
      value, .dependency_vaccination_map(occurrences, dictionary)
    )
  } else if (definition == "key_worker_care_role_v1") {
    values <- lapply(variables, function(x) suppressWarnings(as.numeric(data[[x]])) == 1)
    selected <- do.call(cbind, values)
    if (is.null(dim(selected))) selected <- matrix(selected, ncol = 1L)
    direct <- grepl("direct patient|direct contact", occurrences$label, ignore.case = TRUE)
    non_facing <- grepl("no patient|without contact", occurrences$label, ignore.case = TRUE)
    unknown <- grepl("don't know|don.t know", occurrences$label, ignore.case = TRUE)
    none <- grepl(
      "none of these|not currently required to work outside",
      occurrences$label, ignore.case = TRUE
    )
    result <- ifelse(rowSums(selected[, direct, drop = FALSE], na.rm = TRUE) > 0L,
      "patient_or_client_facing_health_or_care",
      ifelse(rowSums(selected[, non_facing, drop = FALSE], na.rm = TRUE) > 0L,
        "non_facing_health_or_care",
        ifelse(rowSums(selected[, !direct & !non_facing & !none & !unknown, drop = FALSE], na.rm = TRUE) > 0L,
          "other_key_worker", ifelse(rowSums(selected[, none, drop = FALSE], na.rm = TRUE) > 0L,
            "none", "unknown_or_missing"))))
  } else {
    stop("Unsupported approved synthetic predictor definition: ", definition, ".",
         call. = FALSE)
  }
  missing_level <- if ("withheld_or_missing" %in% .dependency_split_ids(spec$levels[[1L]])) {
    "withheld_or_missing"
  } else if ("unknown_or_missing" %in% .dependency_split_ids(spec$levels[[1L]])) {
    "unknown_or_missing"
  } else "missing"
  result[is.na(result) | !nzchar(result)] <- missing_level
  allowed <- .dependency_split_ids(spec$levels[[1L]])
  result[!(result %in% allowed)] <- missing_level
  result
}

.dependency_predictor_levels <- function(spec, dictionary, observed = character()) {
  .dependency_split_ids(spec$levels[[1L]])
}

.dependency_encodable_levels <- function(spec, dictionary) {
  levels <- .dependency_predictor_levels(spec, dictionary)
  if (identical(spec$derivation_rule_id[[1L]], "gender_v1")) {
    occurrences <- .dependency_occurrences(spec, dictionary)
    support <- .dependency_public_support(occurrences, dictionary)
    mapping <- c(`0` = "not_specified", `1` = "male", `2` = "female",
                 `9` = "not_specified")
    encodable <- unique(unname(mapping[intersect(names(mapping), support)]))
    # Database missingness is encodable in every round, but a distinct
    # not-specified response is only generated where the questionnaire exposes
    # an explicit code for it.
    levels <- levels[levels %in% c(encodable, "missing")]
  }
  levels
}

.dependency_count_rows <- function(data, round_id, specs, dictionary) {
  outcome_rows <- list()
  dependency_rows <- list()
  outcomes <- split(seq_len(nrow(specs)), specs$outcome_id)
  states <- list()
  for (definition in names(outcomes)) {
    state <- .dependency_outcome_state(
      data, round_id, definition, dictionary = dictionary
    )
    states[[definition]] <- state
    outcome_levels <- .dependency_outcome_levels(definition)
    counts <- table(factor(state, levels = outcome_levels))
    outcome_rows[[length(outcome_rows) + 1L]] <- data.frame(
      outcome_id = definition,
      round_id = round_id,
      outcome_level = names(counts),
      count = as.integer(counts),
      stringsAsFactors = FALSE
    )
  }
  for (index in seq_len(nrow(specs))) {
    spec <- specs[index, , drop = FALSE]
    outcome <- states[[spec$outcome_id[[1L]]]]
    predictor <- .dependency_predictor_state(data, spec, dictionary)
    levels <- .dependency_predictor_levels(spec, dictionary, predictor)
    grid <- expand.grid(
      outcome_level = .dependency_outcome_levels(spec$outcome_id[[1L]]),
      predictor_level = levels,
      stringsAsFactors = FALSE
    )
    key <- paste(outcome, predictor, sep = "\r")
    observed <- table(key)
    grid_key <- paste(grid$outcome_level, grid$predictor_level, sep = "\r")
    count <- as.integer(observed[grid_key])
    count[is.na(count)] <- 0L
    dependency_rows[[length(dependency_rows) + 1L]] <- data.frame(
      dependency_id = spec$dependency_id[[1L]],
      round_id = round_id,
      outcome_id = spec$outcome_id[[1L]],
      predictor_id = spec$predictor_id[[1L]],
      grid,
      count = count,
      stringsAsFactors = FALSE
    )
  }
  list(
    outcome_counts = .bind_rows(outcome_rows, data.frame()),
    dependency_counts = .bind_rows(dependency_rows, data.frame())
  )
}

.dependency_expected_profile_keys <- function(dictionary, rounds) {
  specs <- .dependency_specs(dictionary, rounds)
  dependency_rows <- lapply(seq_len(nrow(specs)), function(index) {
    spec <- specs[index, , drop = FALSE]
    grid <- expand.grid(
      outcome_level = .dependency_outcome_levels(spec$outcome_id[[1L]]),
      predictor_level = .dependency_predictor_levels(spec, dictionary),
      stringsAsFactors = FALSE
    )
    data.frame(
      dependency_id = spec$dependency_id[[1L]],
      round_id = spec$round_id[[1L]],
      outcome_id = spec$outcome_id[[1L]],
      predictor_id = spec$predictor_id[[1L]],
      grid,
      stringsAsFactors = FALSE
    )
  })
  outcome_pairs <- unique(specs[c("outcome_id", "round_id")])
  outcome_rows <- lapply(seq_len(nrow(outcome_pairs)), function(index) {
    data.frame(
      outcome_id = outcome_pairs$outcome_id[[index]],
      round_id = outcome_pairs$round_id[[index]],
      outcome_level = .dependency_outcome_levels(
        outcome_pairs$outcome_id[[index]]
      ),
      stringsAsFactors = FALSE
    )
  })
  list(
    outcomes = .bind_rows(outcome_rows, data.frame()),
    dependencies = .bind_rows(dependency_rows, data.frame())
  )
}

.validate_dependency_profile_contract <- function(profile, dictionary,
                                                   rounds = "all") {
  requested_rounds <- .resolve_rounds(rounds, dictionary$rounds)
  expected <- .dependency_expected_profile_keys(dictionary, requested_rounds)
  denominators <- profile$round_denominators
  if (!is.data.frame(denominators) ||
      !all(c("round_id", "count") %in% names(denominators)) ||
      anyDuplicated(denominators$round_id) ||
      !setequal(denominators$round_id, requested_rounds) ||
      any(!is.finite(suppressWarnings(as.numeric(denominators$count)))) ||
      any(suppressWarnings(as.numeric(denominators$count)) <= 0)) {
    stop(
      "The v5 profile requires one positive source denominator for every requested round.",
      call. = FALSE
    )
  }
  compare_keys <- function(actual, reference, columns, label) {
    if (!is.data.frame(actual) || !all(columns %in% names(actual))) {
      stop("The v5 profile is missing its ", label, " table contract.", call. = FALSE)
    }
    actual_key <- do.call(paste, c(actual[columns], sep = "\r"))
    expected_key <- do.call(paste, c(reference[columns], sep = "\r"))
    if (anyDuplicated(actual_key) || !setequal(actual_key, expected_key)) {
      stop(
        "The v5 ", label, " grid is incomplete or contains unexpected rows: expected ",
        length(expected_key), "; observed ", length(actual_key), ".",
        call. = FALSE
      )
    }
  }
  compare_keys(
    profile$outcome_counts, expected$outcomes,
    c("outcome_id", "round_id", "outcome_level"), "outcome"
  )
  compare_keys(
    profile$dependency_counts, expected$dependencies,
    c(
      "dependency_id", "round_id", "outcome_id", "predictor_id",
      "outcome_level", "predictor_level"
    ),
    "outcome-predictor"
  )
  invisible(TRUE)
}

.profile_dependency_tables <- function(source, requested_rounds, dictionary,
                                       progress = interactive()) {
  specs <- .dependency_specs(dictionary, requested_rounds)
  empty_outcomes <- data.frame(
    outcome_id = character(), round_id = character(),
    outcome_level = character(), count = integer(), stringsAsFactors = FALSE
  )
  empty_dependencies <- data.frame(
    dependency_id = character(), round_id = character(),
    outcome_id = character(), predictor_id = character(),
    outcome_level = character(), predictor_level = character(), count = integer(),
    stringsAsFactors = FALSE
  )
  if (!nrow(specs)) {
    return(list(outcome_counts = empty_outcomes,
                dependency_counts = empty_dependencies, issues = .empty_issues(),
                round_denominators = data.frame(
                  round_id = character(), count = integer(), stringsAsFactors = FALSE
                )))
  }
  registry <- if (inherits(source, "react_oracle_source")) {
    source$registry
  } else {
    dictionary$source_registry
  }
  outcomes <- list()
  dependencies <- list()
  issues <- list()
  for (round_index in seq_along(requested_rounds)) {
    round_id <- requested_rounds[[round_index]]
    round_specs <- specs[specs$round_id == round_id, , drop = FALSE]
    if (!nrow(round_specs)) next
    outcome_variables <- unique(unlist(lapply(
      unique(round_specs$outcome_id),
      function(outcome_id) .dependency_outcome_variables(round_id, outcome_id)
    ), use.names = FALSE))
    required_outcome_variables <- unique(unlist(lapply(
      unique(round_specs$outcome_id),
      function(outcome_id) {
        variables <- .dependency_outcome_variables(round_id, outcome_id)
        if (identical(outcome_id, "react2_igg_positive") &&
            !identical(round_id, "react2.r01")) {
          "NEWRESULT"
        } else {
          variables
        }
      }
    ), use.names = FALSE))
    predictor_variables <- unique(unlist(lapply(seq_len(nrow(round_specs)), function(index) {
      .dependency_occurrences(round_specs[index, , drop = FALSE], dictionary)$variable
    }), use.names = FALSE))
    fields <- unique(c(outcome_variables, predictor_variables))
    fields <- fields[!is.na(fields) & nzchar(fields)]
    registry_row <- registry[registry$round_id == round_id, , drop = FALSE]
    .progress_message(
      progress, "Profile dependencies ", round_index, "/",
      length(requested_rounds), " ", round_id, " | requesting ",
      length(fields), " fields"
    )
    fetched <- .profile_source_fetch(source, registry_row, fields)
    issues[[length(issues) + 1L]] <- fetched$issues
    if (is.null(fetched$data)) next
    missing_fields <- setdiff(fields, names(fetched$data))
    if (length(missing_fields)) {
      issues[[length(issues) + 1L]] <- .bind_rows(lapply(missing_fields, function(field) {
        .issue(
          "warning", "profile", "dependency_field_unavailable",
          "A reviewed dependency field was unavailable; the affected table was omitted.",
          round_id, registry_row$object_name[[1L]], field
        )
      }), .empty_issues())
      if (any(required_outcome_variables %in% missing_fields)) next
      keep <- vapply(seq_len(nrow(round_specs)), function(index) {
        all(.dependency_occurrences(
          round_specs[index, , drop = FALSE], dictionary
        )$variable %in% names(fetched$data))
      }, logical(1L))
      round_specs <- round_specs[keep, , drop = FALSE]
      if (!nrow(round_specs)) next
    }
    if (identical(round_id, "react2.r01") &&
        all(c("NEWRESULT", "NEWRESULT_2") %in% names(fetched$data))) {
      primary <- .dependency_react2_outcome(fetched$data, round_id)
      companion_value <- suppressWarnings(as.numeric(fetched$data$NEWRESULT_2))
      companion <- ifelse(companion_value == 1, "negative",
        ifelse(companion_value == 2, "positive",
          ifelse(companion_value %in% c(3, 4), "non_evaluable", "missing")))
      conflict <- primary != "missing" & companion != "missing" &
        primary != companion
      if (any(conflict)) {
        issues[[length(issues) + 1L]] <- .issue(
          "warning", "profile", "react2_outcome_companion_conflict",
          paste(
            "NEWRESULT and NEWRESULT_2 imply different antibody-result states;",
            "the primary NEWRESULT coding was retained."
          ),
          round_id, registry_row$object_name[[1L]],
          "NEWRESULT|NEWRESULT_2", affected_count = sum(conflict)
        )
      }
    }
    lab_result_fields <- intersect(
      c("RESULT", "FINALRESULT"),
      intersect(outcome_variables, names(fetched$data))
    )
    for (field in lab_result_fields) {
      unknown <- .dependency_lab_result_unknown(
        fetched$data[[field]], round_id, field, dictionary
      )
      unknown[is.na(unknown)] <- FALSE
      if (any(unknown)) {
        issues[[length(issues) + 1L]] <- .issue(
          "warning", "profile", "unrecognised_lab_result",
          paste(
            "An exact laboratory-result value is not in the reviewed support;",
            "it was retained as PCR missing and never classified as negative."
          ),
          round_id, registry_row$object_name[[1L]], field,
          affected_count = sum(unknown)
        )
      }
    }
    rows <- .dependency_count_rows(fetched$data, round_id, round_specs, dictionary)
    outcomes[[length(outcomes) + 1L]] <- rows$outcome_counts
    dependencies[[length(dependencies) + 1L]] <- rows$dependency_counts
  }
  outcome_counts <- .bind_rows(outcomes, empty_outcomes)
  if (nrow(outcome_counts)) {
    key <- paste(outcome_counts$outcome_id, outcome_counts$round_id,
                 outcome_counts$outcome_level, sep = "\r")
    outcome_counts <- outcome_counts[!duplicated(key), , drop = FALSE]
  }
  list(
    outcome_counts = outcome_counts,
    dependency_counts = .bind_rows(dependencies, empty_dependencies),
    issues = .bind_rows(issues, .empty_issues()),
    round_denominators = .bind_rows(lapply(outcomes, function(rows) {
      if (!nrow(rows)) return(NULL)
      data.frame(round_id = unique(rows$round_id),
                 count = sum(rows$count),
                 stringsAsFactors = FALSE)
    }), data.frame(round_id = character(), count = integer()))
  )
}

.synthetic_dependencies_available <- function(source) {
  is.data.frame(source$profile$outcome_counts) && nrow(source$profile$outcome_counts) > 0L &&
    is.data.frame(source$profile$dependency_counts) && nrow(source$profile$dependency_counts) > 0L
}

.synthetic_dependency_fallbacks <- function(profile) {
  rows <- profile$dependency_counts
  if (!is.data.frame(rows) || !nrow(rows)) return(character())
  key <- paste(rows$dependency_id, rows$round_id, rows$outcome_level, sep = "::")
  groups <- split(seq_len(nrow(rows)), key)
  names(groups)[vapply(groups, function(index) {
    counts <- suppressWarnings(as.numeric(rows$count[index]))
    !any(!is.na(counts) & counts > 0)
  }, logical(1L))]
}

.sample_dependency_values <- function(values, size, probability = NULL) {
  if (!length(values) || size == 0L) return(values[0])
  values[sample.int(
    length(values), size = size, replace = TRUE, prob = probability
  )]
}

.sample_dependency_outcome <- function(source, round_id, outcome_id, n) {
  rows <- source$profile$outcome_counts[
    source$profile$outcome_counts$round_id == round_id &
      source$profile$outcome_counts$outcome_id == outcome_id, , drop = FALSE
  ]
  levels <- .dependency_outcome_levels(outcome_id)
  counts <- suppressWarnings(as.numeric(rows$count[match(levels, rows$outcome_level)]))
  probability <- .smoothed_probabilities(counts, source$safe_prior_fraction)
  .with_stream_seed(source$seed, round_id, paste0("outcome::", outcome_id), {
    .sample_dependency_values(levels, n, probability)
  })
}

.sample_dependency_predictor <- function(source, round_id, spec, outcome, n,
                                         dictionary = react_dictionary()) {
  rows <- source$profile$dependency_counts[
    source$profile$dependency_counts$round_id == round_id &
      source$profile$dependency_counts$dependency_id == spec$dependency_id[[1L]],
    , drop = FALSE
  ]
  levels <- .dependency_encodable_levels(spec, dictionary)
  result <- rep(NA_character_, n)
  for (outcome_level in unique(outcome)) {
    positions <- which(outcome == outcome_level)
    local <- rows[rows$outcome_level == outcome_level, , drop = FALSE]
    counts <- suppressWarnings(as.numeric(local$count[match(levels, local$predictor_level)]))
    if (!any(!is.na(counts) & counts > 0L)) {
      marginal <- stats::aggregate(
        suppressWarnings(as.numeric(rows$count)),
        list(predictor_level = rows$predictor_level), sum, na.rm = TRUE
      )
      counts <- marginal$x[match(levels, marginal$predictor_level)]
    }
    probability <- .smoothed_probabilities(counts, source$safe_prior_fraction)
    result[positions] <- .with_stream_seed(
      source$seed, round_id,
      paste0("dependency::", spec$dependency_id[[1L]], "::", outcome_level),
      .sample_dependency_values(levels, length(positions), probability)
    )
  }
  result
}

.set_numeric_fields <- function(data, fields, values) {
  for (field in intersect(fields, names(data))) data[[field]] <- as.numeric(values)
  data
}

.resample_dependency_scalar <- function(data, fields, original_state, desired_state,
                                        fallback, source, spec) {
  for (field in intersect(fields, names(data))) {
    value <- fallback
    for (level in unique(desired_state)) {
      if (identical(level, "missing")) next
      positions <- which(desired_state == level)
      pool <- data[[field]][original_state == level &
        !.dependency_admin_value(data[[field]])]
      if (!length(pool)) next
      value[positions] <- .with_stream_seed(
        source$seed, spec$round_id[[1L]],
        paste0("dependency_raw::", spec$dependency_id[[1L]], "::", field, "::", level),
        .sample_dependency_values(pool, length(positions))
      )
    }
    if (is.numeric(data[[field]])) value <- suppressWarnings(as.numeric(value))
    data[[field]] <- value
  }
  data
}

.set_dependency_categorical_scalar <- function(data, state, code_map,
                                                occurrences, dictionary,
                                                source, spec,
                                                fallback_support = character()) {
  fields <- intersect(occurrences$variable, names(data))
  for (field in fields) {
    raw <- data[[field]]
    support <- .dependency_public_support(
      occurrences, dictionary, field = field, fallback = fallback_support
    )
    support <- intersect(support, names(code_map))
    raw_state <- .dependency_map_codes(raw, code_map)
    value <- rep(NA_character_, length(state))
    for (level in unique(state)) {
      if (is.na(level) || identical(level, "missing")) next
      positions <- which(state == level)
      pool <- as.character(raw[
        raw_state == level & as.character(raw) %in% support &
          !.dependency_admin_value(raw)
      ])
      candidates <- support[unname(code_map[support]) == level]
      available <- if (length(pool)) pool else candidates
      if (!length(available)) next
      value[positions] <- .with_stream_seed(
        source$seed, spec$round_id[[1L]],
        paste0("dependency_raw::", spec$dependency_id[[1L]], "::", field,
               "::", level),
        .sample_dependency_values(available, length(positions))
      )
    }
    if (is.numeric(raw)) value <- suppressWarnings(as.numeric(value))
    data[[field]] <- value
  }
  data
}

.initialise_dependency_checkboxes <- function(data, fields, state,
                                              missing_levels = "missing") {
  observed <- !(state %in% missing_levels) & !is.na(state)
  for (field in intersect(fields, names(data))) {
    value <- rep(NA_real_, length(state))
    value[observed] <- 0
    data[[field]] <- value
  }
  data
}

.set_dependency_predictor <- function(data, state, spec, dictionary, source) {
  occurrences <- .dependency_occurrences(spec, dictionary)
  fields <- occurrences$variable
  rule <- spec$derivation_rule_id[[1L]]
  n <- nrow(data)
  original_state <- .dependency_predictor_state(data, spec, dictionary)
  if (rule == "age_band_v1") {
    ranges <- list(
      `5_12` = 5:12, `13_17` = 13:17, `18_24` = 18:24, `25_34` = 25:34,
      `35_44` = 35:44, `45_54` = 45:54, `55_64` = 55:64,
      `65_74` = 65:74, `75_84` = 75:84, `85_plus` = 85:95
    )
    value <- rep(NA_real_, n)
    for (level in intersect(names(ranges), unique(state))) {
      positions <- which(state == level)
      value[positions] <- rep(ranges[[level]], length.out = length(positions))
    }
    return(.resample_dependency_scalar(
      data, fields, original_state, state, value, source, spec
    ))
  }
  if (rule == "gender_v1") {
    return(.set_dependency_categorical_scalar(
      data, state,
      c(`0` = "not_specified", `1` = "male", `2` = "female",
        `9` = "not_specified"),
      occurrences, dictionary, source, spec, fallback_support = c("1", "2")
    ))
  }
  if (rule == "ethnicity_broad_v1") {
    support <- .dependency_public_support(occurrences, dictionary)
    mapping <- c(
      stats::setNames(rep("white", 4L), as.character(1:4)),
      stats::setNames(rep("mixed", 4L), as.character(5:8)),
      stats::setNames(rep("asian", 5L), as.character(9:13)),
      stats::setNames(rep("black", 3L), as.character(14:16)),
      `17` = "other", `18` = "other", `19` = "withheld_or_missing",
      `101` = "white", `102` = "mixed", `103` = "asian",
      `104` = "black", `105` = "other"
    )
    if ("20" %in% support) mapping <- c(mapping, `20` = "other")
    return(.set_dependency_categorical_scalar(
      data, state, mapping, occurrences, dictionary, source, spec
    ))
  }
  if (rule == "region_v1") {
    map <- stats::setNames(1:9, c(
      "north_east", "north_west", "yorkshire_humber", "east_midlands",
      "west_midlands", "east_of_england", "london", "south_east", "south_west"
    ))
    return(.resample_dependency_scalar(
      data, fields, original_state, state, map[state], source, spec
    ))
  }
  if (rule == "imd_quintile_v1") {
    map <- c(q1_most_deprived = 1, q2 = 3, q3 = 5, q4 = 7, q5_least_deprived = 9)
    return(.resample_dependency_scalar(
      data, fields, original_state, state, map[state], source, spec
    ))
  }
  if (rule == "household_size_band_v1") {
    adults <- fields[grepl("^NADULTS", fields)]
    children <- fields[grepl("^NCHILD", fields)]
    adult_value <- c(one = 1, two = 2, three_to_five = 3, six_plus = 6)[state]
    data <- .set_numeric_fields(data, adults, adult_value)
    return(.set_numeric_fields(data, children, ifelse(is.na(adult_value), NA, 0)))
  }
  if (rule == "economic_activity_broad_v1") {
    mapping <- c(
      `1` = "employed", `2` = "employed", `3` = "self_employed",
      `4` = "education", `5` = "unemployed", `6` = "retired",
      `7` = "education", `8` = "home_or_caring",
      `9` = "sick_or_disabled", `10` = "other_or_withheld",
      `11` = "other_or_withheld"
    )
    return(.set_dependency_categorical_scalar(
      data, state, mapping, occurrences, dictionary, source, spec
    ))
  }
  if (rule == "covid_history_v1") {
    return(.set_dependency_categorical_scalar(
      data, state,
      c(`1` = "test_confirmed", `2` = "clinician_suspected",
        `3` = "self_suspected", `4` = "none"),
      occurrences, dictionary, source, spec
    ))
  }
  if (rule == "confirmed_contact_v1") {
    if ("COVIDCON" %in% fields) {
      return(.set_dependency_categorical_scalar(
        data, state,
        c(`1` = "confirmed", `2` = "suspected_only", `3` = "none"),
        occurrences[occurrences$variable == "COVIDCON", , drop = FALSE],
        dictionary, source, spec
      ))
    }
    data <- .initialise_dependency_checkboxes(data, fields, state)
    set_selected <- function(pattern, positions) {
      target <- intersect(fields[grepl(pattern, fields)], names(data))
      if (length(target)) data[[target[[1L]]]][positions] <<- 1
    }
    set_selected("_1$", which(state == "confirmed"))
    set_selected("_2$", which(state == "suspected_only"))
    set_selected("_3$", which(state == "none"))
    return(data)
  }
  if (rule == "classic_symptom_status_v1") {
    if ("FEELUN" %in% names(data)) data$FEELUN <- ifelse(state == "none", 2, ifelse(state == "missing", NA, 1))
    symptom_fields <- setdiff(fields, "FEELUN")
    data <- .initialise_dependency_checkboxes(data, symptom_fields, state)
    classic <- symptom_fields[grepl(
      "persistent cough|loss or change to sense of (smell|taste)|fever",
      occurrences$label[match(symptom_fields, occurrences$variable)], ignore.case = TRUE
    )]
    other <- symptom_fields[!symptom_fields %in% classic & !grepl(
      "none of these", occurrences$label[match(symptom_fields, occurrences$variable)],
      ignore.case = TRUE
    )]
    none <- symptom_fields[grepl(
      "none of these", occurrences$label[match(symptom_fields, occurrences$variable)],
      ignore.case = TRUE
    )]
    if (length(classic)) data[[classic[[1L]]]][state == "classic"] <- 1
    if (length(other)) data[[other[[1L]]]][state == "other"] <- 1
    if (length(none)) data[[none[[1L]]]][state == "none"] <- 1
    return(data)
  }
  if (rule == "vaccination_status_v1") {
    return(.set_dependency_categorical_scalar(
      data, state, .dependency_vaccination_map(occurrences, dictionary),
      occurrences, dictionary, source, spec
    ))
  }
  if (rule == "key_worker_care_role_v1") {
    data <- .initialise_dependency_checkboxes(
      data, fields, state, missing_levels = c("unknown_or_missing", "missing")
    )
    labels <- occurrences$label
    groups <- list(
      patient_or_client_facing_health_or_care = which(grepl("direct patient|direct contact", labels, ignore.case = TRUE)),
      non_facing_health_or_care = which(grepl("no patient|without contact", labels, ignore.case = TRUE)),
      other_key_worker = which(!grepl(
        "direct patient|direct contact|no patient|without contact|none of these|don.t know|not currently required to work outside",
        labels, ignore.case = TRUE
      )),
      none = which(grepl(
        "none of these|not currently required to work outside",
        labels, ignore.case = TRUE
      )),
      unknown_or_missing = which(grepl("don.t know", labels, ignore.case = TRUE))
    )
    for (level in names(groups)) {
      candidates <- fields[groups[[level]]]
      candidates <- intersect(candidates, names(data))
      if (length(candidates)) data[[candidates[[1L]]]][state == level] <- 1
    }
    return(data)
  }
  data
}

.set_dependency_outcome <- function(data, state, round_id, outcome_id, source,
                                    dictionary = react_dictionary()) {
  n <- nrow(data)
  if (outcome_id == "react1_pcr_positive") {
    outcome_fields <- intersect(
      .dependency_outcome_variables(round_id, outcome_id),
      c("RESULT", "FINALRESULT")
    )
    for (field in intersect(outcome_fields, names(data))) {
      original <- as.character(data[[field]])
      value <- rep(NA_character_, n)
      occurrence_id <- .lab_result_occurrence_id(round_id, field, dictionary)
      raw_state <- if (is.na(occurrence_id)) {
        rep("unknown", length(original))
      } else {
        .lab_result_value_state(original, occurrence_id, dictionary)
      }
      support <- if (is.na(occurrence_id)) {
        data.frame(raw_value = c("Detected", "Not Detected", "Void"),
                   outcome_state = c("detected", "negative", "missing"),
                   stringsAsFactors = FALSE)
      } else {
        .lab_result_support(occurrence_id, dictionary)
      }
      desired_state <- ifelse(is.na(state), "missing", state)
      for (level in c("positive", "negative", "missing")) {
        positions <- which(desired_state == level)
        if (!length(positions)) next
        support_state <- if (level == "positive") "detected" else level
        pool <- original[raw_state == support_state]
        if (!length(pool)) {
          pool <- support$raw_value[support$outcome_state == support_state]
        }
        if (!length(pool)) {
          pool <- switch(
            level, positive = "Detected", negative = "Not Detected", "Void"
          )
        }
        value[positions] <- .with_stream_seed(
          source$seed, round_id,
          paste0("outcome::react1_pcr_positive::", field, "::raw_", level),
          .sample_dependency_values(pool, length(positions))
        )
      }
      data[[field]] <- value
    }
    if ("LAB" %in% names(data)) data$LAB <- ifelse(state == "missing", NA, "Eurofin")
    for (field in intersect(
      c("CT_VALUE1", "CT_VALUE2", "NGENE_CTVALUE", "EGENE_CTVALUE"),
      names(data)
    )) {
      original <- suppressWarnings(as.numeric(data[[field]]))
      pool <- original[!is.na(original) & original > 0 & original <= 50]
      positive_positions <- which(state == "positive")
      positive_ct <- if (length(pool)) {
        .with_stream_seed(
          source$seed, round_id,
          paste0("outcome::react1_pcr_positive::", field),
          .sample_dependency_values(pool, length(positive_positions))
        )
      } else {
        .with_stream_seed(
          source$seed, round_id,
          paste0("outcome::react1_pcr_positive::", field, "::fallback"),
          round(stats::runif(length(positive_positions), 15, 36.9), 1)
        )
      }
      value <- rep(NA_real_, n)
      value[state == "negative"] <- 0
      value[positive_positions] <- positive_ct
      data[[field]] <- value
    }
    return(data)
  }
  if (outcome_id == "react2_igg_positive") {
    detailed <- rep(NA_real_, n)
    original <- if ("NEWRESULT" %in% names(data)) {
      suppressWarnings(as.numeric(data$NEWRESULT))
    } else rep(NA_real_, n)
    original_state <- ifelse(original %in% c(0, 1), "negative",
      ifelse(original %in% c(2, 3), "positive",
        ifelse(original %in% 4:7, "non_evaluable", "missing")))
    fallback <- list(
      negative = c(0, 1), positive = c(2, 3),
      non_evaluable = if (round_id == "react2.r01") 4:7 else 4:6
    )
    for (level in names(fallback)) {
      positions <- which(state == level)
      pool <- original[original_state == level]
      support <- if (length(pool)) pool else fallback[[level]]
      detailed[positions] <- .with_stream_seed(
        source$seed, round_id, paste0("outcome::react2_igg_positive::raw::", level),
        .sample_dependency_values(support, length(positions))
      )
    }
    if ("NEWRESULT" %in% names(data)) data$NEWRESULT <- detailed
    if ("NEWRESULT_2" %in% names(data)) {
      data$NEWRESULT_2 <- ifelse(detailed %in% c(0, 1), 1,
        ifelse(detailed %in% c(2, 3), 2, ifelse(detailed == 4, 3,
          ifelse(detailed %in% 5:7, 4, NA))))
    }
    observed <- !is.na(detailed)
    if ("ABATTEMPT" %in% names(data)) data$ABATTEMPT[observed] <- 1
    if ("ABCOMP" %in% names(data)) {
      data$ABCOMP[detailed %in% 0:6] <- 1
      data$ABCOMP[detailed == 7] <- 3
    }
    return(data)
  }
  data
}

.apply_synthetic_dependencies <- function(data, occurrences, source, dictionary) {
  if (!.synthetic_dependencies_available(source)) return(data)
  round_id <- unique(occurrences$round_id)
  specs <- .dependency_specs(dictionary, round_id)
  if (!nrow(specs)) return(data)
  outcomes <- unique(specs$outcome_id)
  for (outcome_id in outcomes) {
    state <- .sample_dependency_outcome(source, round_id, outcome_id, nrow(data))
    data <- .set_dependency_outcome(
      data, state, round_id, outcome_id, source, dictionary
    )
    local <- specs[specs$outcome_id == outcome_id, , drop = FALSE]
    local <- local[order(as.integer(local$generation_order)), , drop = FALSE]
    for (index in seq_len(nrow(local))) {
      predictor <- .sample_dependency_predictor(
        source, round_id, local[index, , drop = FALSE], state, nrow(data),
        dictionary = dictionary
      )
      data <- .set_dependency_predictor(
        data, predictor, local[index, , drop = FALSE], dictionary, source
      )
    }
  }
  data
}

.reapply_synthetic_outcomes <- function(data, occurrences, source, dictionary) {
  if (!.synthetic_dependencies_available(source)) return(data)
  round_id <- unique(occurrences$round_id)
  specs <- .dependency_specs(dictionary, round_id)
  for (outcome_id in unique(specs$outcome_id)) {
    state <- .sample_dependency_outcome(source, round_id, outcome_id, nrow(data))
    data <- .set_dependency_outcome(
      data, state, round_id, outcome_id, source, dictionary
    )
  }
  data
}

.synchronise_synthetic_age_group <- function(data, occurrences, dictionary) {
  if (!("U_AGE" %in% names(data)) || !("AGE_GROUP" %in% names(data))) return(data)
  occurrence <- occurrences[occurrences$variable == "AGE_GROUP", , drop = FALSE]
  if (!nrow(occurrence)) return(data)
  options <- .profile_occurrence_options(occurrence$occurrence_id[[1L]], dictionary)
  if (!nrow(options)) {
    same_study <- dictionary$occurrences[
      sub("[.].*$", "", dictionary$occurrences$round_id) ==
        sub("[.].*$", "", occurrence$round_id[[1L]]) &
        dictionary$occurrences$variable == "AGE_GROUP", , drop = FALSE
    ]
    for (id in same_study$occurrence_id) {
      options <- .profile_occurrence_options(id, dictionary)
      if (nrow(options)) break
    }
  }
  age <- suppressWarnings(as.numeric(data$U_AGE))
  value <- rep(NA_real_, length(age))
  for (index in seq_len(nrow(options))) {
    label <- tolower(options$display_value[[index]])
    numbers <- suppressWarnings(as.numeric(unlist(regmatches(
      label, gregexpr("[0-9]+", label)
    ))))
    selected <- rep(FALSE, length(age))
    if (length(numbers) >= 2L) {
      selected <- !is.na(age) & age >= numbers[[1L]] & age <= numbers[[2L]]
    } else if (length(numbers) == 1L && grepl("[+]", label)) {
      selected <- !is.na(age) & age >= numbers[[1L]]
    }
    value[selected] <- suppressWarnings(as.numeric(options$return_value[[index]]))
  }
  data$AGE_GROUP <- value
  data
}

.synthetic_dependency_occurrences <- function(dictionary, source, requested_rounds) {
  if (!.synthetic_dependencies_available(source)) return(dictionary$occurrences[0, , drop = FALSE])
  specs <- .dependency_specs(dictionary, requested_rounds)
  predictor <- .bind_rows(lapply(seq_len(nrow(specs)), function(index) {
    .dependency_occurrences(specs[index, , drop = FALSE], dictionary)
  }), dictionary$occurrences[0, , drop = FALSE])
  outcome_variables <- unique(unlist(lapply(seq_len(nrow(specs)), function(index) {
    .dependency_outcome_variables(specs$round_id[[index]], specs$outcome_id[[index]])
  }), use.names = FALSE))
  outcome <- dictionary$occurrences[
    dictionary$occurrences$round_id %in% requested_rounds &
      dictionary$occurrences$variable %in% outcome_variables, , drop = FALSE
  ]
  unique(rbind(predictor, outcome))
}
