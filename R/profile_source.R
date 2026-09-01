.profile_v2_empty <- function() {
  list(
    metadata = data.frame(key = character(), value = character(), stringsAsFactors = FALSE),
    round_denominators = data.frame(
      round_id = character(), count = integer(), stringsAsFactors = FALSE
    ),
    missingness = data.frame(
      occurrence_id = character(), round_id = character(), status = character(),
      count = integer(), stringsAsFactors = FALSE
    ),
    categorical_counts = data.frame(
      occurrence_id = character(), round_id = character(), value = character(),
      display_value = character(), count = integer(), stringsAsFactors = FALSE
    ),
    numeric_bin_counts = data.frame(
      occurrence_id = character(), round_id = character(), bin_spec_id = character(),
      bin_id = character(), count = integer(), stringsAsFactors = FALSE
    ),
    text_presence = data.frame(
      occurrence_id = character(), round_id = character(), status = character(),
      count = integer(), stringsAsFactors = FALSE
    ),
    routing_validation = data.frame(
      routing_rule_id = character(), round_id = character(), status = character(),
      count = integer(), stringsAsFactors = FALSE
    ),
    outcome_counts = data.frame(
      outcome_id = character(), round_id = character(), outcome_level = character(),
      count = integer(), stringsAsFactors = FALSE
    ),
    dependency_counts = data.frame(
      dependency_id = character(), round_id = character(), outcome_id = character(),
      predictor_id = character(), outcome_level = character(),
      predictor_level = character(), count = integer(), stringsAsFactors = FALSE
    ),
    dependency_specs = data.frame(),
    profile_specs = data.frame(),
    safe_bins = data.frame(),
    issues = .empty_issues()
  )
}

.profile_source_fetch <- function(source, registry_row, fields) {
  if (inherits(source, "react_oracle_source")) {
    .read_oracle_round(source, registry_row, fields)
  } else if (inherits(source, "react_file_source")) {
    .read_file_round(source, registry_row, fields)
  } else {
    stop("`react_profile_source()` accepts Oracle or round-file sources.", call. = FALSE)
  }
}

.administrative_missing_domain <- function() {
  data.frame(
    return_value = c("-91", "-92", "-77", "-66", "-99", "-555"),
    display_value = c(
      "Not Applicable", "Item Non-Response", "Survey Non-Response",
      "Administrative non-response", "Other documented missing value",
      "Missing from source file"
    ),
    stringsAsFactors = FALSE
  )
}

.coded_missing_domain <- function(options, observed = NULL) {
  reviewed_missing <- if ("outcome_state" %in% names(options)) {
    !is.na(options$outcome_state) & options$outcome_state == "missing"
  } else {
    rep(FALSE, nrow(options))
  }
  local <- options[reviewed_missing | grepl(
    "not applicable|non-response|non response|missing|refus|prefer not|don't know|do not know|unknown",
    options$display_value,
    ignore.case = TRUE
  ), , drop = FALSE]
  local <- local[c("return_value", "display_value")]
  fixed <- .administrative_missing_domain()
  present_codes <- unique(c(options$return_value, as.character(observed)))
  fixed <- fixed[fixed$return_value %in% present_codes, , drop = FALSE]
  unique(rbind(local, fixed))
}

.oracle_presence_fetch_batch <- function(source, registry_row, base_keys, fields) {
  key <- registry_row$observation_key[[1L]]
  object <- registry_row$object_name[[1L]]
  expressions <- vapply(fields, function(field) {
    paste0(
      "CASE WHEN ", .quoted_field(field), " IS NULL THEN 0 ELSE 1 END AS ",
      .quoted_field(field)
    )
  }, character(1L))
  sql <- paste0(
    "SELECT ", .quoted_field(key), ", ", paste(expressions, collapse = ", "),
    " FROM ", object, " ORDER BY ", .quoted_field(key)
  )
  result <- tryCatch(source$query_fn(source$connection, sql), error = identity)
  if (inherits(result, "error")) {
    if (length(fields) > 1L) {
      midpoint <- floor(length(fields) / 2L)
      left <- .oracle_presence_fetch_batch(
        source, registry_row, base_keys, fields[seq_len(midpoint)]
      )
      right <- .oracle_presence_fetch_batch(
        source, registry_row, base_keys,
        fields[seq.int(midpoint + 1L, length(fields))]
      )
      return(list(
        data = c(left$data, right$data),
        issues = .bind_rows(list(left$issues, right$issues), .empty_issues())
      ))
    }
    return(list(
      data = list(),
      issues = .issue(
        "warning", "extract", "field_presence_query_failed",
        conditionMessage(result), registry_row$round_id[[1L]], object, fields[[1L]]
      )
    ))
  }
  if (!all(c(key, fields) %in% names(result))) {
    missing <- setdiff(c(key, fields), names(result))
    return(list(
      data = list(),
      issues = .bind_rows(lapply(missing, function(field) {
        .issue(
          "warning", "extract", "query_column_missing",
          paste0("The presence query did not contain exact field `", field, "`."),
          registry_row$round_id[[1L]], object, field
        )
      }), .empty_issues())
    ))
  }
  result_keys <- as.character(result[[key]])
  if (anyNA(result_keys) || anyDuplicated(result_keys) ||
      length(result_keys) != length(base_keys) || !setequal(result_keys, base_keys)) {
    return(list(
      data = list(),
      issues = .bind_rows(lapply(fields, function(field) {
        .issue(
          "error", "extract", "unsafe_keyed_recovery",
          "The presence query did not return exactly the validated observation-key set.",
          registry_row$round_id[[1L]], object, field, length(result_keys)
        )
      }), .empty_issues())
    ))
  }
  index <- match(base_keys, result_keys)
  data <- lapply(fields, function(field) {
    present <- suppressWarnings(as.integer(result[[field]][index])) == 1L
    ifelse(!is.na(present) & present, TRUE, NA)
  })
  names(data) <- fields
  list(data = data, issues = .empty_issues())
}

.profile_source_fetch_presence <- function(source, registry_row, fields) {
  if (length(fields) == 0L) {
    return(.profile_source_fetch(source, registry_row, character()))
  }
  if (inherits(source, "react_file_source")) {
    fetched <- .profile_source_fetch(source, registry_row, fields)
    if (!is.null(fetched$data)) {
      for (field in intersect(fields, names(fetched$data))) {
        present <- !is.na(fetched$data[[field]]) & nzchar(as.character(fetched$data[[field]]))
        fetched$data[[field]] <- ifelse(present, TRUE, NA)
      }
    }
    return(fetched)
  }
  key <- registry_row$observation_key[[1L]]
  base <- .read_oracle_round(source, registry_row, character())
  if (is.null(base$data)) return(base)
  base_keys <- as.character(base$data[[key]])
  batches <- split(fields, ceiling(seq_along(fields) / source$batch_size))
  recovered <- list()
  issues <- list(base$issues)
  for (batch in batches) {
    part <- .oracle_presence_fetch_batch(source, registry_row, base_keys, batch)
    recovered <- c(recovered, part$data)
    issues[[length(issues) + 1L]] <- part$issues
  }
  list(
    data = data.frame(base$data[key], recovered, check.names = FALSE),
    source_object = registry_row$object_name[[1L]],
    issues = .bind_rows(issues, .empty_issues())
  )
}

.profile_source_fetch_safe <- function(source, registry_row, fields,
                                       presence_fields = character()) {
  fields <- unique(fields)
  presence_fields <- intersect(unique(presence_fields), fields)
  value_fields <- setdiff(fields, presence_fields)
  value_result <- .profile_source_fetch(source, registry_row, value_fields)
  presence_result <- .profile_source_fetch_presence(source, registry_row, presence_fields)
  issues <- .bind_rows(
    list(value_result$issues, presence_result$issues), .empty_issues()
  )
  if (is.null(value_result$data) && is.null(presence_result$data)) {
    return(list(data = NULL, issues = issues))
  }
  key <- registry_row$observation_key[[1L]]
  available <- Filter(Negate(is.null), list(value_result$data, presence_result$data))
  base <- available[[1L]]
  for (part in available[-1L]) {
    if (!identical(as.character(base[[key]]), as.character(part[[key]]))) {
      return(list(
        data = NULL,
        issues = .bind_rows(list(issues, .issue(
          "error", "profile", "unsafe_presence_alignment",
          "Value and presence-only profile queries did not return the same keyed rows.",
          registry_row$round_id[[1L]], registry_row$object_name[[1L]], key
        )), .empty_issues())
      ))
    }
    for (field in setdiff(names(part), key)) base[[field]] <- part[[field]]
  }
  list(data = base, issues = issues)
}

.safe_date_numeric <- function(value) {
  if (inherits(value, "Date")) return(as.numeric(value))
  if (inherits(value, "POSIXt")) return(as.numeric(as.Date(value)))
  if (is.numeric(value)) return(as.numeric(value))

  text <- trimws(as.character(value))
  parsed <- rep(as.Date(NA), length(text))
  iso_datetime <- !is.na(text) & grepl(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}[ T]", text
  )
  parsed[iso_datetime] <- suppressWarnings(as.Date(substr(text[iso_datetime], 1L, 10L)))
  formats <- c(
    "%Y-%m-%d", "%Y/%m/%d", "%d/%m/%Y", "%d-%m-%Y", "%d.%m.%Y",
    "%d-%b-%Y", "%d-%B-%Y", "%d/%m/%y", "%d-%m-%y"
  )
  for (format in formats) {
    pending <- which(is.na(parsed) & !is.na(text) & nzchar(text))
    if (length(pending) == 0L) break
    candidate <- suppressWarnings(as.Date(text[pending], format = format))
    accepted <- !is.na(candidate) &
      tolower(base::format(candidate, format = format)) == tolower(text[pending])
    parsed[pending[accepted]] <- candidate[accepted]
  }
  as.numeric(parsed)
}

.safe_bin_boundary_rule <- function(bins, index) {
  rule <- if ("boundary_rules" %in% names(bins)) {
    bins$boundary_rules[[index]]
  } else {
    "inclusive"
  }
  allowed <- c(
    "inclusive", "lower_exclusive_upper_inclusive",
    "lower_inclusive_upper_exclusive", "exclusive"
  )
  if (!rule %in% allowed) {
    stop("Unsupported safe-bin boundary rule: ", rule, ".", call. = FALSE)
  }
  rule
}

.bin_values <- function(value, bins, value_type) {
  if (nrow(bins) == 0L) return(rep(NA_character_, length(value)))
  numeric_value <- if (value_type == "date") {
    .safe_date_numeric(value)
  } else {
    suppressWarnings(as.numeric(value))
  }
  out <- rep(NA_character_, length(value))
  for (index in seq_len(nrow(bins))) {
    lower <- bins$lower[[index]]
    upper <- bins$upper[[index]]
    lower_value <- if (!nzchar(lower)) -Inf else if (value_type == "date") {
      as.numeric(as.Date(lower))
    } else as.numeric(lower)
    upper_value <- if (!nzchar(upper)) Inf else if (value_type == "date") {
      as.numeric(as.Date(upper))
    } else as.numeric(upper)
    boundary_rule <- .safe_bin_boundary_rule(bins, index)
    lower_selected <- if (boundary_rule %in% c(
      "lower_exclusive_upper_inclusive", "exclusive"
    )) {
      numeric_value > lower_value
    } else {
      numeric_value >= lower_value
    }
    upper_selected <- if (boundary_rule %in% c(
      "lower_inclusive_upper_exclusive", "exclusive"
    )) {
      numeric_value < upper_value
    } else {
      numeric_value <= upper_value
    }
    selected <- !is.na(numeric_value) & lower_selected & upper_selected
    out[selected & is.na(out)] <- bins$bin_id[[index]]
  }
  out
}

.profile_occurrence_options <- function(occurrence_id, dictionary) {
  rows <- dictionary$response_options[
    dictionary$response_options$occurrence_id == occurrence_id,
    c("return_value", "display_value", "option_source_order"),
    drop = FALSE
  ]
  placeholder <- rows$return_value == "NA" & rows$display_value == "NA"
  rows <- rows[!placeholder, , drop = FALSE]
  rows <- rows[
    order(suppressWarnings(as.integer(rows$option_source_order))),
    , drop = FALSE
  ]
  rows <- rows[!duplicated(rows$return_value), , drop = FALSE]
  rows[c("return_value", "display_value")]
}

.approved_profile_specs <- function(dictionary) {
  specs <- dictionary$synthetic_profile_specs[
    dictionary$synthetic_profile_specs$review_state == "approved", , drop = FALSE
  ]
  overrides <- dictionary$synthetic_profile_overrides
  if (is.data.frame(overrides) && nrow(overrides)) {
    overrides <- overrides[overrides$review_state == "approved", , drop = FALSE]
    index <- match(overrides$occurrence_id, specs$occurrence_id)
    matched <- !is.na(index)
    specs$profile_kind[index[matched]] <- overrides$profile_kind[matched]
    specs$generation_action[index[matched]] <- overrides$generation_action[matched]
    specs$support_source[index[matched]] <- paste0(
      "public_support:", overrides$support_id[matched]
    )
  }
  specs
}

.profile_response_options <- function(occurrence, spec, dictionary) {
  public_support <- length(spec$support_source) == 1L &&
    is.character(spec$support_source[[1L]]) &&
    !is.na(spec$support_source[[1L]]) &&
    startsWith(spec$support_source[[1L]], "public_support:")
  if (public_support) {
    support_id <- sub("^public_support:", "", spec$support_source[[1L]])
    rows <- dictionary$synthetic_public_supports[
      dictionary$synthetic_public_supports$support_id == support_id &
        dictionary$synthetic_public_supports$review_state == "approved", , drop = FALSE
    ]
    rows <- rows[order(suppressWarnings(as.integer(rows$sort_order))), , drop = FALSE]
    result <- data.frame(
      return_value = rows$raw_value, display_value = rows$label,
      stringsAsFactors = FALSE
    )
    if ("outcome_state" %in% names(rows)) {
      result$outcome_state <- rows$outcome_state
    }
    return(result)
  }
  options <- .profile_occurrence_options(
    occurrence$occurrence_id[[1L]], dictionary
  )
  inferred_domain <- length(spec$support_source) == 1L &&
    identical(
      spec$support_source[[1L]],
      "inferred_public_response_domain"
    )
  administrative <- .administrative_missing_domain()$return_value
  usable_local <- options[
    nzchar(options$return_value) &
      options$return_value != "NA" &
      !(options$return_value %in% administrative) &
      nzchar(options$display_value) &
      options$display_value != "NA",
    , drop = FALSE
  ]
  if (nrow(usable_local) || !inferred_domain) {
    return(options)
  }

  study_id <- sub("[.].*$", "", occurrence$round_id[[1L]])
  candidates <- dictionary$occurrences[
    sub("[.].*$", "", dictionary$occurrences$round_id) == study_id &
      dictionary$occurrences$variable == occurrence$variable[[1L]] &
      dictionary$occurrences$label == occurrence$label[[1L]],
    , drop = FALSE
  ]
  candidate_options <- lapply(
    candidates$occurrence_id,
    .profile_occurrence_options,
    dictionary = dictionary
  )
  signatures <- vapply(candidate_options, function(rows) {
    usable <- rows[
      nzchar(rows$return_value) &
        rows$return_value != "NA" &
        !(rows$return_value %in% administrative) &
        nzchar(rows$display_value) &
        rows$display_value != "NA",
      , drop = FALSE
    ]
    if (!nrow(usable)) return(NA_character_)
    paste(
      paste(usable$return_value, usable$display_value, sep = "\u001f"),
      collapse = "\u001e"
    )
  }, character(1))
  available <- !is.na(signatures)
  domains <- unique(signatures[available])
  if (length(domains) != 1L) {
    stop(
      "No single public category domain resolves `",
      occurrence$variable[[1L]], "` in ", occurrence$round_id[[1L]], ".",
      call. = FALSE
    )
  }
  inferred <- candidate_options[[
    which(available & signatures == domains[[1L]])[[1L]]
  ]]
  combined <- rbind(options, inferred)
  combined[!duplicated(combined$return_value), , drop = FALSE]
}

.profile_exact_occurrence <- function(value, occurrence, spec, dictionary) {
  occurrence_id <- occurrence$occurrence_id[[1L]]
  round_id <- occurrence$round_id[[1L]]
  common <- data.frame(
    occurrence_id = occurrence_id,
    round_id = round_id,
    stringsAsFactors = FALSE
  )
  missingness <- data.frame(
    common,
    status = "database_missing",
    count = sum(is.na(value)),
    stringsAsFactors = FALSE
  )
  categorical <- data.frame()
  numeric_bins <- data.frame()
  text_presence <- data.frame()
  issues <- .empty_issues()
  observed <- value[!is.na(value)]

  options <- .profile_response_options(occurrence, spec, dictionary)
  # Some Oracle DATE columns contain the survey's negative administrative
  # codes as Date offsets from 1970. Compare their numeric day representation
  # with the documented codes before binning, rather than treating the
  # resulting 1968/1969 dates as respondent dates.
  observed_code <- as.character(observed)
  if (spec$profile_kind[[1L]] == "date" && length(observed)) {
    date_numeric <- .safe_date_numeric(observed)
    for (code in .administrative_missing_domain()$return_value) {
      selected <- !is.na(date_numeric) & date_numeric == as.numeric(code)
      observed_code[selected] <- code
    }
  }
  coded_missing <- .coded_missing_domain(options, observed_code)
  if (nrow(coded_missing)) {
    observed_text <- observed_code
    for (index in seq_len(nrow(coded_missing))) {
      missingness <- rbind(
        missingness,
        data.frame(
          common,
          status = paste0("coded:", coded_missing$return_value[[index]]),
          count = sum(observed_text == coded_missing$return_value[[index]], na.rm = TRUE),
          stringsAsFactors = FALSE
        )
      )
    }
  }

  if (spec$profile_kind[[1L]] %in% c("categorical", "ordered_categorical")) {
    support <- options$return_value
    observed_text <- as.character(observed)
    unexpected <- !(observed_text %in% unique(c(support, coded_missing$return_value)))
    if (any(unexpected)) {
      missingness <- rbind(
        missingness,
        data.frame(
          common,
          status = "outside_safe_support",
          count = sum(unexpected),
          stringsAsFactors = FALSE
        )
      )
      issues <- .issue(
        "warning", "profile", "unrecognised_profile_code",
        "Values outside the public questionnaire response domain were counted but not released.",
        round_id, variable = occurrence$variable[[1L]], affected_count = sum(unexpected)
      )
    }
    counts <- vapply(support, function(code) sum(observed_text == code, na.rm = TRUE), integer(1L))
    categorical <- data.frame(
      common[rep(1L, length(support)), , drop = FALSE],
      value = support,
      display_value = options$display_value,
      count = counts,
      stringsAsFactors = FALSE
    )
  } else if (spec$profile_kind[[1L]] %in% c("integer", "continuous", "date")) {
    values_for_bins <- observed[!(observed_code %in% coded_missing$return_value)]
    bins <- dictionary$safe_bins[dictionary$safe_bins$bin_spec_id == spec$bin_spec_id[[1L]], , drop = FALSE]
    assigned <- .bin_values(values_for_bins, bins, spec$profile_kind[[1L]])
    counts <- vapply(bins$bin_id, function(bin) sum(assigned == bin, na.rm = TRUE), integer(1L))
    numeric_bins <- data.frame(
      common[rep(1L, nrow(bins)), , drop = FALSE],
      bin_spec_id = spec$bin_spec_id[[1L]],
      bin_id = bins$bin_id,
      count = counts,
      stringsAsFactors = FALSE
    )
    outside <- !is.na(values_for_bins) & is.na(assigned)
    if (any(outside)) {
      missingness <- rbind(
        missingness,
        data.frame(
          common,
          status = "outside_safe_support",
          count = sum(outside),
          stringsAsFactors = FALSE
        )
      )
      issues <- .issue(
        "warning", "profile", "value_outside_safe_bins",
        "Values outside the fixed reviewed bins were counted but exact values were not retained.",
        round_id, variable = occurrence$variable[[1L]], affected_count = sum(outside)
      )
    }
  } else if (spec$profile_kind[[1L]] == "free_text") {
    present <- if (is.logical(value)) {
      !is.na(value) & value
    } else {
      !is.na(value) & nzchar(as.character(value))
    }
    text_presence <- data.frame(
      common[rep(1L, 2L), , drop = FALSE],
      status = c("present", "empty_or_missing"),
      count = c(sum(present), length(value) - sum(present)),
      stringsAsFactors = FALSE
    )
  }
  list(
    missingness = missingness,
    categorical_counts = categorical,
    numeric_bin_counts = numeric_bins,
    text_presence = text_presence,
    issues = issues
  )
}

.profile_round_routing <- function(source, registry_row, round_id, dictionary) {
  rules <- dictionary$routing_rules
  instruments <- dictionary$instruments
  if (is.null(rules) || is.null(instruments)) {
    return(list(data = data.frame(), issues = .empty_issues()))
  }
  survey_id <- dictionary$rounds$survey_id[dictionary$rounds$round_id == round_id]
  instrument_ids <- instruments$instrument_id[instruments$round_id == survey_id]
  rules <- rules[
    rules$review_state == "approved" & rules$instrument_id %in% instrument_ids,
    , drop = FALSE
  ]
  if (nrow(rules) == 0L) return(list(data = data.frame(), issues = .empty_issues()))
  conditions <- dictionary$routing_conditions[
    dictionary$routing_conditions$routing_rule_id %in% rules$routing_rule_id,
    , drop = FALSE
  ]
  targets <- dictionary$routing_targets[
    dictionary$routing_targets$routing_rule_id %in% rules$routing_rule_id,
    , drop = FALSE
  ]
  occurrences <- dictionary$occurrences[dictionary$occurrences$round_id == round_id, , drop = FALSE]
  variable_by_id <- stats::setNames(occurrences$variable, occurrences$occurrence_id)
  fields <- unique(c(
    unname(variable_by_id[conditions$parent_occurrence_id]),
    unname(variable_by_id[targets$target_occurrence_id])
  ))
  fields <- fields[!is.na(fields)]
  field_occurrences <- occurrences[occurrences$variable %in% fields, , drop = FALSE]
  specs <- dictionary$synthetic_profile_specs[
    match(field_occurrences$occurrence_id, dictionary$synthetic_profile_specs$occurrence_id),
    , drop = FALSE
  ]
  presence_fields <- field_occurrences$variable[specs$profile_kind == "free_text"]
  fetched <- .profile_source_fetch_safe(
    source, registry_row, fields, presence_fields = presence_fields
  )
  if (is.null(fetched$data)) return(list(data = data.frame(), issues = fetched$issues))
  rows <- list()
  for (rule_index in seq_len(nrow(rules))) {
    rule_id <- rules$routing_rule_id[[rule_index]]
    rule_conditions <- conditions[conditions$routing_rule_id == rule_id, , drop = FALSE]
    rule_targets <- targets[targets$routing_rule_id == rule_id, , drop = FALSE]
    parent_variables <- unname(variable_by_id[rule_conditions$parent_occurrence_id])
    target_variables <- unname(variable_by_id[rule_targets$target_occurrence_id])
    if (!all(c(parent_variables, target_variables) %in% names(fetched$data))) next
    eligible <- rep(FALSE, nrow(fetched$data))
    for (clause_id in unique(rule_conditions$clause_id)) {
      clause <- rule_conditions[rule_conditions$clause_id == clause_id, , drop = FALSE]
      clause_result <- rep(TRUE, nrow(fetched$data))
      for (condition_index in seq_len(nrow(clause))) {
        variable <- variable_by_id[[clause$parent_occurrence_id[[condition_index]]]]
        clause_result <- clause_result & .condition_true(
          fetched$data[[variable]], clause$operator[[condition_index]],
          clause$comparison_values_json[[condition_index]]
        )
      }
      eligible <- eligible | clause_result
    }
    target_present <- vapply(target_variables, function(variable) {
      !is.na(fetched$data[[variable]])
    }, logical(nrow(fetched$data)))
    if (is.null(dim(target_present))) target_present <- matrix(target_present, ncol = 1L)
    any_target <- rowSums(target_present) > 0L
    status_count <- c(
      eligible = sum(eligible),
      ineligible = sum(!eligible),
      eligible_child_missing = sum(eligible & !any_target),
      routed_not_asked = sum(!eligible & !any_target),
      contradiction = sum(!eligible & any_target)
    )
    rows[[length(rows) + 1L]] <- data.frame(
      routing_rule_id = rule_id,
      round_id = round_id,
      status = names(status_count),
      count = as.integer(status_count),
      stringsAsFactors = FALSE
    )
  }
  list(data = .bind_rows(rows), issues = fetched$issues)
}

.profile_distribution_map <- function(occurrences, specs, dictionary) {
  if (nrow(occurrences) == 0L) {
    return(data.frame(
      occurrence_id = character(), distribution_group_id = character(),
      variable = character(), profile_kind = character(), bin_spec_id = character(),
      stringsAsFactors = FALSE
    ))
  }
  spec_index <- match(occurrences$occurrence_id, specs$occurrence_id)
  profile_kind <- specs$profile_kind[spec_index]
  bin_spec_id <- specs$bin_spec_id[spec_index]
  signature <- character(nrow(occurrences))
  for (index in seq_len(nrow(occurrences))) {
    occurrence_id <- occurrences$occurrence_id[[index]]
    option_rows <- .profile_response_options(
      occurrences[index, , drop = FALSE], specs[spec_index[[index]], , drop = FALSE],
      dictionary
    )
    domain <- if (profile_kind[[index]] %in% c("categorical", "ordered_categorical")) {
      sort(unique(option_rows$return_value))
    } else {
      character()
    }
    signature[[index]] <- paste(
      occurrences$variable[[index]], profile_kind[[index]],
      ifelse(is.na(bin_spec_id[[index]]), "", bin_spec_id[[index]]),
      paste(domain, collapse = "\r"), sep = "\r"
    )
  }
  unique_signatures <- sort(unique(signature))
  group_id <- sprintf("dist_%05d", match(signature, unique_signatures))
  data.frame(
    occurrence_id = occurrences$occurrence_id,
    distribution_group_id = group_id,
    variable = occurrences$variable,
    profile_kind = profile_kind,
    bin_spec_id = ifelse(is.na(bin_spec_id), "", bin_spec_id),
    stringsAsFactors = FALSE
  )
}

.profile_group_table <- function(map, rounds) {
  if (nrow(map) == 0L) return(data.frame())
  round_by_occurrence <- stats::setNames(
    rounds$round_id[match(map$occurrence_id, rounds$occurrence_id)], map$occurrence_id
  )
  groups <- split(seq_len(nrow(map)), map$distribution_group_id)
  rows <- lapply(groups, function(indices) {
    round_ids <- unique(unname(round_by_occurrence[map$occurrence_id[indices]]))
    round_ids <- round_ids[!is.na(round_ids)]
    data.frame(
      distribution_group_id = map$distribution_group_id[[indices[[1L]]]],
      variable = map$variable[[indices[[1L]]]],
      profile_kind = map$profile_kind[[indices[[1L]]]],
      bin_spec_id = map$bin_spec_id[[indices[[1L]]]],
      round_count = length(round_ids),
      round_ids = paste(round_ids, collapse = "|"),
      stringsAsFactors = FALSE
    )
  })
  .bind_rows(rows, data.frame())
}

.profile_sum_counts <- function(data, map, fields) {
  if (!is.data.frame(data) || nrow(data) == 0L) return(data.frame())
  group_id <- map$distribution_group_id[match(data$occurrence_id, map$occurrence_id)]
  keep <- !is.na(group_id)
  if (!any(keep)) return(data.frame())
  work <- data[keep, , drop = FALSE]
  work$distribution_group_id <- group_id[keep]
  by <- work[c("distribution_group_id", fields)]
  result <- stats::aggregate(
    as.numeric(work$count), by = by, FUN = sum, na.rm = TRUE
  )
  names(result)[[ncol(result)]] <- "count"
  result
}

.derive_overall_profile <- function(profile, occurrences, specs, dictionary) {
  map <- .profile_distribution_map(occurrences, specs, dictionary)
  round_map <- occurrences[c("occurrence_id", "round_id")]
  groups <- .profile_group_table(map, round_map)
  categorical <- .profile_sum_counts(profile$categorical_counts, map, "value")
  if (nrow(categorical) > 0L) {
    source_rows <- profile$categorical_counts
    source_rows$distribution_group_id <- map$distribution_group_id[
      match(source_rows$occurrence_id, map$occurrence_id)
    ]
    label_key <- paste(
      source_rows$distribution_group_id, source_rows$value, sep = "\r"
    )
    labels <- split(source_rows$display_value, label_key)
    result_key <- paste(
      categorical$distribution_group_id, categorical$value, sep = "\r"
    )
    categorical$display_value <- vapply(result_key, function(key) {
      candidates <- unique(labels[[key]])
      candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
      if (length(candidates) == 0L) "" else candidates[[1L]]
    }, character(1L))
    categorical$display_value_varies <- vapply(result_key, function(key) {
      candidates <- unique(labels[[key]])
      sum(!is.na(candidates) & nzchar(candidates)) > 1L
    }, logical(1L))
  }
  numeric_bins <- .profile_sum_counts(
    profile$numeric_bin_counts, map, c("bin_spec_id", "bin_id")
  )
  missingness <- .profile_sum_counts(profile$missingness, map, "status")
  text_presence <- .profile_sum_counts(profile$text_presence, map, "status")
  list(
    distribution_groups = groups,
    overall_missingness = missingness,
    overall_categorical_counts = categorical,
    overall_numeric_bin_counts = numeric_bins,
    overall_text_presence = text_presence
  )
}

#' Profile exact REACT fields sequentially inside the enclave
#'
#' This function never includes observation keys, subject links, raw text, exact
#' extrema, or data-derived bin boundaries in its return value.
#'
#' @param source A source created by [react_oracle()] or [react_files()].
#' @param rounds `all`, round IDs, or survey IDs.
#' @param progress Show round and batch progress.
#' @param batch_size Maximum fields per profiling batch.
#' @param occurrence_ids Optional exact occurrence IDs for a targeted repair.
#' @param include_routing Profile approved routing checks. Disable this for a
#'   targeted distribution repair when the existing routing table is retained.
#' @param include_overall Create all-round raw-variable distributions from the
#'   unsuppressed aggregates. This is intended for a complete profile run.
#' @param include_dependencies Profile the reviewed v5 outcome-centred
#'   dependencies. This makes one targeted field pull per round and never
#'   returns observation identifiers.
#' @return An unsuppressed schema-2 aggregate profile for
#'   [react_prepare_profile_export()].
#' @export
react_profile_source <- function(source, rounds = "all", progress = interactive(),
                                 batch_size = 50L, occurrence_ids = NULL,
                                 include_routing = is.null(occurrence_ids),
                                 include_overall = is.null(occurrence_ids),
                                 include_dependencies = is.null(occurrence_ids)) {
  if (!inherits(source, c("react_oracle_source", "react_file_source"))) {
    stop("`source` must be created by `react_oracle()` or `react_files()`.", call. = FALSE)
  }
  batch_size <- as.integer(batch_size)
  if (length(batch_size) != 1L || is.na(batch_size) || batch_size < 1L) {
    stop("`batch_size` must be one positive integer.", call. = FALSE)
  }
  dictionary <- react_dictionary()
  requested_rounds <- .resolve_rounds(rounds, dictionary$rounds)
  registry <- if (inherits(source, "react_oracle_source")) source$registry else dictionary$source_registry
  profile <- .profile_v2_empty()
  profile_parts <- list()
  issues <- list()
  started <- .extract_clock()

  specs <- dictionary$synthetic_profile_specs
  if (is.null(specs)) stop("The pinned dictionary has no synthetic profile specifications.", call. = FALSE)
  approved_specs <- .approved_profile_specs(dictionary)
  if (nrow(approved_specs) == 0L) {
    stop(
      "The pinned dictionary has no approved profiling dispositions. Review schema-6 synthetic_profile_specs before enclave profiling.",
      call. = FALSE
    )
  }
  if (!is.null(occurrence_ids)) {
    if (!is.character(occurrence_ids) || length(occurrence_ids) == 0L ||
        anyNA(occurrence_ids) || any(!nzchar(occurrence_ids))) {
      stop("`occurrence_ids` must contain non-empty exact occurrence IDs.", call. = FALSE)
    }
    unknown <- setdiff(unique(occurrence_ids), approved_specs$occurrence_id)
    if (length(unknown) > 0L) {
      stop("Unknown profiling occurrence ID: ", paste(unknown, collapse = ", "), ".", call. = FALSE)
    }
    approved_specs <- approved_specs[
      approved_specs$occurrence_id %in% unique(occurrence_ids), , drop = FALSE
    ]
    requested_rounds <- requested_rounds[
      requested_rounds %in% unique(approved_specs$round_id)
    ]
  }
  required_bins <- unique(approved_specs$bin_spec_id[nzchar(approved_specs$bin_spec_id)])
  approved_bins <- unique(dictionary$safe_bins$bin_spec_id[
    dictionary$safe_bins$review_state == "approved"
  ])
  unapproved_bins <- setdiff(required_bins, approved_bins)
  if (length(unapproved_bins) > 0L) {
    stop(
      "Approved profiling dispositions reference unapproved safe bins: ",
      paste(unapproved_bins, collapse = ", "), ".",
      call. = FALSE
    )
  }

  profiled_occurrences <- list()
  all_profiled_occurrences <- list()
  for (round_index in seq_along(requested_rounds)) {
    round_id <- requested_rounds[[round_index]]
    registry_row <- registry[registry$round_id == round_id, , drop = FALSE]
    round_occurrences <- dictionary$occurrences[
      dictionary$occurrences$round_id == round_id &
        dictionary$occurrences$occurrence_id %in% approved_specs$occurrence_id,
      , drop = FALSE
    ]
    round_specs <- approved_specs[match(round_occurrences$occurrence_id, approved_specs$occurrence_id), , drop = FALSE]
    safe <- !(round_specs$profile_kind == "identifier" |
      round_specs$generation_action %in% c("synthetic_identifier", "excluded"))
    round_occurrences <- round_occurrences[safe, , drop = FALSE]
    round_specs <- round_specs[safe, , drop = FALSE]

    unavailable <- character()
    if (inherits(source, "react_oracle_source") &&
        is.data.frame(dictionary$oracle_unavailable_fields)) {
      unavailable <- dictionary$oracle_unavailable_fields$variable[
        dictionary$oracle_unavailable_fields$round_id == round_id &
          dictionary$oracle_unavailable_fields$source_object == registry_row$object_name[[1L]]
      ]
    }
    requested_round_occurrences <- round_occurrences
    if (length(unavailable) > 0L) {
      skipped <- round_occurrences$variable %in% unavailable
      if (any(skipped)) {
        for (index in which(skipped)) {
          issues[[length(issues) + 1L]] <- .issue(
            "information", "profile", "confirmed_unavailable_skipped",
            "The dictionary records this field as unavailable in the enclave view; it was not queried.",
            round_id, registry_row$object_name[[1L]], round_occurrences$variable[[index]]
          )
        }
      }
      round_occurrences <- round_occurrences[!skipped, , drop = FALSE]
      round_specs <- round_specs[!skipped, , drop = FALSE]
    }
    if (nrow(requested_round_occurrences) > 0L) {
      requested_status <- ifelse(
        requested_round_occurrences$variable %in% unavailable,
        "confirmed_unavailable", "requested"
      )
      profiled_occurrences[[length(profiled_occurrences) + 1L]] <- data.frame(
        occurrence_id = requested_round_occurrences$occurrence_id,
        round_id = requested_round_occurrences$round_id,
        variable = requested_round_occurrences$variable,
        profile_kind = approved_specs$profile_kind[
          match(requested_round_occurrences$occurrence_id, approved_specs$occurrence_id)
        ],
        status = requested_status,
        stringsAsFactors = FALSE
      )
    }
    all_profiled_occurrences[[length(all_profiled_occurrences) + 1L]] <- round_occurrences

    .progress_message(progress, "Profile round ", round_index, "/", length(requested_rounds),
                      " ", round_id, " | ", nrow(round_occurrences), " safe fields")
    denominator_result <- .profile_source_fetch(source, registry_row, character())
    issues[[length(issues) + 1L]] <- denominator_result$issues
    if (is.null(denominator_result$data)) next
    profile$round_denominators <- rbind(
      profile$round_denominators,
      data.frame(round_id = round_id, count = nrow(denominator_result$data), stringsAsFactors = FALSE)
    )

    batches <- split(seq_len(nrow(round_occurrences)),
                     ceiling(seq_len(nrow(round_occurrences)) / batch_size))
    for (batch_index in seq_along(batches)) {
      indices <- batches[[batch_index]]
      fields <- unique(round_occurrences$variable[indices])
      .progress_message(progress, "Profile ", round_id, " batch ", batch_index, "/",
                        length(batches), " | requesting ", length(fields), " fields")
      presence_fields <- round_occurrences$variable[indices][
        round_specs$profile_kind[indices] == "free_text"
      ]
      fetched <- .profile_source_fetch_safe(
        source, registry_row, fields, presence_fields = presence_fields
      )
      issues[[length(issues) + 1L]] <- fetched$issues
      if (is.null(fetched$data)) next
      for (local_index in seq_along(indices)) {
        occurrence <- round_occurrences[indices[[local_index]], , drop = FALSE]
        spec <- round_specs[indices[[local_index]], , drop = FALSE]
        variable <- occurrence$variable[[1L]]
        if (!(variable %in% names(fetched$data))) next
        profiled <- .profile_exact_occurrence(fetched$data[[variable]], occurrence, spec, dictionary)
        profile_parts[[length(profile_parts) + 1L]] <- profiled
      }
    }
    if (isTRUE(include_routing)) {
      routing_profile <- .profile_round_routing(source, registry_row, round_id, dictionary)
      profile$routing_validation <- .bind_rows(
        list(profile$routing_validation, routing_profile$data),
        profile$routing_validation
      )
      issues[[length(issues) + 1L]] <- routing_profile$issues
    }
  }

  for (name in c("missingness", "categorical_counts", "numeric_bin_counts", "text_presence")) {
    profile[[name]] <- .bind_rows(lapply(profile_parts, `[[`, name), profile[[name]])
  }
  profile$issues <- .bind_rows(c(issues, lapply(profile_parts, `[[`, "issues")), .empty_issues())
  profile$profile_specs <- approved_specs
  profile$safe_bins <- dictionary$safe_bins[
    dictionary$safe_bins$review_state == "approved" &
      dictionary$safe_bins$bin_spec_id %in% required_bins,
    , drop = FALSE
  ]
  profile$profiled_occurrences <- .bind_rows(profiled_occurrences, data.frame())
  if (isTRUE(include_overall)) {
    totals <- .derive_overall_profile(
      profile,
      .bind_rows(all_profiled_occurrences, dictionary$occurrences[0, , drop = FALSE]),
      approved_specs,
      dictionary
    )
    for (name in names(totals)) profile[[name]] <- totals[[name]]
  }
  if (isTRUE(include_dependencies)) {
    dependency_profile <- .profile_dependency_tables(
      source, requested_rounds, dictionary, progress = progress
    )
    profile$outcome_counts <- dependency_profile$outcome_counts
    profile$dependency_counts <- dependency_profile$dependency_counts
    profile$dependency_specs <- dictionary$synthetic_dependencies[
      dictionary$synthetic_dependencies$review_state == "approved", , drop = FALSE
    ]
    profile$issues <- .bind_rows(
      list(profile$issues, dependency_profile$issues), .empty_issues()
    )
  }
  version <- react_dictionary_version()
  profile$metadata <- data.frame(
    key = c(
      "profile_schema_version", "profile_version", "status", "package_version",
      "dictionary_release", "dictionary_manifest_sha256", "routing_specification_sha256",
      "elapsed_seconds", "profile_scope", "profiled_occurrence_count",
      "overall_distributions", "safe_prior_fraction",
      "dependency_specification_sha256", "dependency_tables"
    ),
    value = c(
      "2", "enclave-profile-v1", "enclave_internal_unsuppressed",
      as.character(utils::packageVersion("reactextract")), version$dictionary_release,
      version$manifest_sha256,
      if (!is.null(dictionary$routing_rules)) .profile_object_sha256(dictionary$routing_rules) else "",
      sprintf("%.3f", .elapsed_seconds(started)),
      if (is.null(occurrence_ids)) "complete" else "targeted_repair",
      as.character(nrow(profile$profiled_occurrences)),
      if (isTRUE(include_overall)) "included_unsuppressed" else "not_requested",
      "0.01",
      if (is.data.frame(dictionary$synthetic_dependencies)) {
        .profile_object_sha256(dictionary$synthetic_dependencies)
      } else "",
      if (isTRUE(include_dependencies)) "included_unsuppressed" else "not_requested"
    ),
    stringsAsFactors = FALSE
  )
  class(profile) <- c("react_profile_v2", "list")
  profile
}

.profile_v2_required <- c(
  "metadata", "round_denominators", "missingness", "categorical_counts",
  "numeric_bin_counts", "text_presence", "routing_validation", "profile_specs",
  "safe_bins", "issues"
)

.profile_v2_optional <- c(
  "profiled_occurrences", "distribution_groups", "overall_missingness",
  "overall_categorical_counts", "overall_numeric_bin_counts",
  "overall_text_presence", "outcome_counts", "dependency_counts",
  "dependency_specs"
)

#' Profile only the reviewed v5 synthetic dependencies inside the enclave
#'
#' This is the small, preferred re-profile for upgrading an already approved
#' marginal profile. It fetches only the outcome and predictor fields named in
#' the pinned dependency contract, round by round, and returns aggregate count
#' tables with no observation identifiers.
#'
#' @inheritParams react_profile_source
#' @return An unsuppressed schema-2 aggregate profile for
#'   [react_prepare_profile_export()].
#' @export
react_profile_dependencies_source <- function(source, rounds = "all",
                                               progress = interactive()) {
  if (!inherits(source, c("react_oracle_source", "react_file_source"))) {
    stop("`source` must be created by `react_oracle()` or `react_files()`.", call. = FALSE)
  }
  dictionary <- react_dictionary()
  requested_rounds <- .resolve_rounds(rounds, dictionary$rounds)
  started <- .extract_clock()
  dependency_profile <- .profile_dependency_tables(
    source, requested_rounds, dictionary, progress = progress
  )
  profile <- .profile_v2_empty()
  profile$round_denominators <- dependency_profile$round_denominators
  profile$outcome_counts <- dependency_profile$outcome_counts
  profile$dependency_counts <- dependency_profile$dependency_counts
  profile$dependency_specs <- dictionary$synthetic_dependencies[
    dictionary$synthetic_dependencies$review_state == "approved", , drop = FALSE
  ]
  profile$profile_specs <- .approved_profile_specs(dictionary)
  profile$safe_bins <- dictionary$safe_bins[
    dictionary$safe_bins$review_state == "approved", , drop = FALSE
  ]
  profile$issues <- dependency_profile$issues
  version <- react_dictionary_version()
  profile$metadata <- data.frame(
    key = c(
      "profile_schema_version", "profile_version", "status", "package_version",
      "dictionary_release", "dictionary_manifest_sha256",
      "dependency_specification_sha256", "elapsed_seconds", "profile_scope",
      "safe_prior_fraction"
    ),
    value = c(
      "2", "enclave-profile-v5-dependencies", "enclave_internal_unsuppressed",
      as.character(utils::packageVersion("reactextract")),
      version$dictionary_release, version$manifest_sha256,
      .profile_object_sha256(dictionary$synthetic_dependencies),
      sprintf("%.3f", .elapsed_seconds(started)), "dependencies_only", "0.01"
    ),
    stringsAsFactors = FALSE
  )
  class(profile) <- c("react_profile_v2", "list")
  profile
}

#' Write a versioned synthetic profile directory
#'
#' @param profile A schema-2 profile after disclosure preparation.
#' @param path A new or empty output directory.
#' @return Invisibly, the normalized profile directory.
#' @export
react_write_profile <- function(profile, path) {
  if (!is.list(profile) || !all(.profile_v2_required %in% names(profile))) {
    stop("`profile` must be a schema-2 reactextract profile.", call. = FALSE)
  }
  path <- .single_string(path, "path")
  if (dir.exists(path) && length(list.files(
    path, all.files = TRUE, no.. = TRUE
  ))) {
    stop(
      "`path` must be a new or empty directory; existing profile files are never overwritten.",
      call. = FALSE
    )
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  optional_names <- intersect(.profile_v2_optional, names(profile))
  optional_names <- optional_names[vapply(optional_names, function(name) {
    is.data.frame(profile[[name]]) && ncol(profile[[name]]) > 0L
  }, logical(1L))]
  table_names <- c(.profile_v2_required, optional_names)
  tables <- profile[unique(table_names)]
  tables$issues <- profile$issues
  csv_paths <- character()
  for (name in names(tables)) {
    if (!is.data.frame(tables[[name]])) next
    file_path <- file.path(path, paste0(name, ".csv"))
    utils::write.csv(tables[[name]], file_path, row.names = FALSE, na = "")
    csv_paths <- c(csv_paths, file_path)
  }
  manifest <- data.frame(
    file = basename(csv_paths),
    sha256 = vapply(csv_paths, .sha256_file, character(1L)),
    byte_size = as.numeric(file.info(csv_paths)$size),
    row_count = vapply(tables[names(tables) %in% tools::file_path_sans_ext(basename(csv_paths))], nrow, integer(1L)),
    stringsAsFactors = FALSE
  )
  utils::write.csv(manifest, file.path(path, "manifest.csv"), row.names = FALSE, na = "")
  invisible(normalizePath(path, mustWork = TRUE))
}

#' Read and verify a versioned synthetic profile directory
#'
#' @param path A directory written by [react_write_profile()].
#' @return A verified schema-2 profile.
#' @export
react_read_profile <- function(path) {
  path <- normalizePath(.single_string(path, "path"), mustWork = TRUE)
  manifest <- .read_literal_csv(file.path(path, "manifest.csv"))
  actual_files <- list.files(path, all.files = TRUE, no.. = TRUE)
  expected_files <- c("manifest.csv", manifest$file)
  if (!setequal(actual_files, expected_files)) {
    stop(
      "Synthetic profile directory contains files not covered by its manifest or is incomplete.",
      call. = FALSE
    )
  }
  for (index in seq_len(nrow(manifest))) {
    file_path <- file.path(path, manifest$file[[index]])
    if (!file.exists(file_path) || .sha256_file(file_path) != manifest$sha256[[index]] ||
        as.character(file.info(file_path)$size) != manifest$byte_size[[index]]) {
      stop("Synthetic profile manifest verification failed for ", manifest$file[[index]], ".", call. = FALSE)
    }
  }
  profile <- lapply(manifest$file, function(file) .read_literal_csv(file.path(path, file)))
  names(profile) <- tools::file_path_sans_ext(manifest$file)
  if (!all(.profile_v2_required %in% names(profile))) {
    stop("Synthetic profile directory is incomplete.", call. = FALSE)
  }
  class(profile) <- c("react_synthetic_profile", "react_profile_v2", "list")
  profile
}

#' Sanitise a previously exported schema-2 synthetic profile
#'
#' This local upgrade is for profiles produced before issue counts received
#' disclosure control and before unsupported source values were represented as
#' synthetic missingness. It does not query the source database.
#'
#' @param profile A profile read by [react_read_profile()].
#' @param policy A disclosure-control policy from [react_sdc_policy()].
#' @return A mechanically protected profile requiring normal disclosure review.
#' @export
react_sanitise_profile <- function(profile, policy = react_sdc_policy()) {
  if (!is.list(profile) || !all(.profile_v2_required %in% names(profile))) {
    stop("`profile` must be an existing schema-2 profile.", call. = FALSE)
  }
  dictionary <- react_dictionary()
  metadata <- .synthetic_profile_metadata(profile)
  expected_hash <- react_dictionary_version()$manifest_sha256[[1L]]
  if (!identical(unname(metadata[["dictionary_manifest_sha256"]]), expected_hash)) {
    stop("Synthetic profile and installed dictionary hashes do not match.", call. = FALSE)
  }

  out <- profile
  issue_rows <- out$issues[out$issues$code %in% c(
    "unrecognised_profile_code", "value_outside_safe_bins"
  ), , drop = FALSE]
  issue_count <- suppressWarnings(as.numeric(issue_rows$affected_count))
  usable <- !is.na(issue_count) & issue_count > 0L
  issue_rows <- issue_rows[usable, , drop = FALSE]
  issue_count <- issue_count[usable]
  if (nrow(issue_rows)) {
    issue_key <- paste(issue_rows$round_id, issue_rows$variable, sep = "\r")
    occurrence_key <- paste(
      dictionary$occurrences$round_id,
      dictionary$occurrences$variable,
      sep = "\r"
    )
    occurrence_id <- dictionary$occurrences$occurrence_id[
      match(issue_key, occurrence_key)
    ]
    if (anyNA(occurrence_id)) {
      stop("One or more profile issues could not be linked to the pinned dictionary.", call. = FALSE)
    }
    added <- data.frame(
      occurrence_id = occurrence_id,
      round_id = issue_rows$round_id,
      status = "outside_safe_support",
      count = issue_count,
      suppressed = issue_count < policy$min_count,
      stringsAsFactors = FALSE
    )
    added <- stats::aggregate(
      count ~ occurrence_id + round_id + status,
      data = added,
      FUN = sum
    )
    added$suppressed <- added$count < policy$min_count
    added$count <- round(added$count / policy$count_rounding) * policy$count_rounding
    added$count[added$suppressed] <- NA_real_

    base <- out$missingness
    if (!"suppressed" %in% names(base)) base$suppressed <- FALSE
    base$suppressed <- tolower(as.character(base$suppressed)) %in% c("true", "1")
    duplicate_key <- paste(base$occurrence_id, base$round_id, base$status, sep = "\r")
    added_key <- paste(added$occurrence_id, added$round_id, added$status, sep = "\r")
    base <- base[!(duplicate_key %in% added_key), , drop = FALSE]
    combined <- .bind_rows(list(base, added), base)

    group_key <- paste(combined$occurrence_id, combined$round_id, sep = "\r")
    for (indices in split(seq_len(nrow(combined)), group_key)) {
      if (sum(combined$suppressed[indices]) == 1L && length(indices) > 1L) {
        candidates <- indices[!combined$suppressed[indices]]
        candidate_counts <- suppressWarnings(as.numeric(combined$count[candidates]))
        candidates <- candidates[!is.na(candidate_counts)]
        candidate_counts <- candidate_counts[!is.na(candidate_counts)]
        if (length(candidates)) {
          chosen <- candidates[[which.min(candidate_counts)]]
          combined$suppressed[[chosen]] <- TRUE
          combined$count[[chosen]] <- NA_real_
        }
      }
    }
    out$missingness <- combined
  }

  out$issues <- .sdc_issue_table(out$issues, policy)
  replace_keys <- c(
    "profile_version", "package_version", "status", "disclosure_approval",
    "unsupported_values_as_missing", "issue_counts_disclosure_controlled",
    "sanitised_without_source_query"
  )
  out$metadata <- out$metadata[!(out$metadata$key %in% replace_keys), , drop = FALSE]
  out$metadata <- rbind(
    out$metadata,
    data.frame(
      key = replace_keys,
      value = c(
        "enclave-profile-v2-sanitised", as.character(utils::packageVersion("reactextract")),
        "requires_enclave_disclosure_review", "not_approved",
        "true", "true", "true"
      ),
      stringsAsFactors = FALSE
    )
  )
  class(out) <- c("react_profile_v2", "list")
  out
}

.profile_contract_rebase_ids <- function(profile, dictionary) {
  metadata <- .synthetic_profile_metadata(profile)
  base_dictionary_hash <- unname(metadata[["dictionary_manifest_sha256"]])
  current_dictionary_hash <- react_dictionary_version()$manifest_sha256[[1L]]
  current_routing_hash <- if (!is.null(dictionary$routing_rules)) {
    .profile_object_sha256(dictionary$routing_rules)
  } else {
    ""
  }
  base_routing_hash <- unname(metadata[["routing_specification_sha256"]])
  routing_hash_matches <- identical(base_routing_hash, current_routing_hash)
  # rc7, rc8 and rc9 contain byte-identical routing CSVs. The original profile
  # used an R-object hash whose serialized representation is not stable enough
  # to reproduce after rebuilding the package, so the exact reviewed
  # predecessor transitions are recorded rather than weakening the check for
  # arbitrary dictionary pairs.
  approved_rc7_transition <-
    identical(
      base_dictionary_hash,
      "7bbb02df4ffb010185f68206dc81f39f6a308b3e97f64f97f5662765522638b3"
    ) &&
    identical(
      base_routing_hash,
      "d0a0b467e3690e24da457227664db8c255afc5aa90790887be00a3e6658de3f0"
    ) &&
    identical(
      current_dictionary_hash,
      "fe4ced6a468b733688eac7129c346ac28c5b4f922036ad3747790251dbcd2c17"
    ) &&
    identical(
      current_routing_hash,
      "70ba0bd048725b3763205a633988dcfee4789c275a9aab8d2659b72f7d9ecd83"
    )
  approved_rc8_transition <-
    identical(
      base_dictionary_hash,
      "fe4ced6a468b733688eac7129c346ac28c5b4f922036ad3747790251dbcd2c17"
    ) &&
    identical(
      base_routing_hash,
      "d0a0b467e3690e24da457227664db8c255afc5aa90790887be00a3e6658de3f0"
    ) &&
    identical(
      current_dictionary_hash,
      "03a2fb41a02becbe292663934e6ed436a85335b93d5004118a82ea9e4460a846"
    ) &&
    identical(
      current_routing_hash,
      "70ba0bd048725b3763205a633988dcfee4789c275a9aab8d2659b72f7d9ecd83"
    )
  approved_rc9_transition <-
    identical(
      base_dictionary_hash,
      "03a2fb41a02becbe292663934e6ed436a85335b93d5004118a82ea9e4460a846"
    ) &&
    identical(
      base_routing_hash,
      "d0a0b467e3690e24da457227664db8c255afc5aa90790887be00a3e6658de3f0"
    ) &&
    identical(
      current_dictionary_hash,
      "28d03054e4b284cd44a040cf473991c441184739e84a7f6235392b1142a79236"
    ) &&
    identical(
      current_routing_hash,
      "70ba0bd048725b3763205a633988dcfee4789c275a9aab8d2659b72f7d9ecd83"
    )
  approved_rc11_transition <-
    identical(
      base_dictionary_hash,
      "f8da578f8aa7827ab3c484ae964853b45aa24a053e20d0423b1e58b59e49410a"
    ) &&
    identical(
      base_routing_hash,
      "d0a0b467e3690e24da457227664db8c255afc5aa90790887be00a3e6658de3f0"
    ) &&
    identical(
      current_dictionary_hash,
      "28d03054e4b284cd44a040cf473991c441184739e84a7f6235392b1142a79236"
    ) &&
    identical(
      current_routing_hash,
      "70ba0bd048725b3763205a633988dcfee4789c275a9aab8d2659b72f7d9ecd83"
    )
  approved_predecessor_transition <-
    approved_rc7_transition || approved_rc8_transition ||
      approved_rc9_transition || approved_rc11_transition
  if (!routing_hash_matches && !approved_predecessor_transition) {
    stop(
      "The base profile cannot be reused because its reviewed routing contract changed.",
      call. = FALSE
    )
  }
  current_specs <- .approved_profile_specs(dictionary)
  old_specs <- profile$profile_specs
  if (!is.data.frame(old_specs) ||
      !setequal(old_specs$occurrence_id, current_specs$occurrence_id) ||
      anyDuplicated(old_specs$occurrence_id) || anyDuplicated(current_specs$occurrence_id)) {
    stop(
      "The base profile cannot be reused because its included occurrences changed.",
      call. = FALSE
    )
  }
  if (!setequal(profile$round_denominators$round_id, dictionary$rounds$round_id)) {
    stop(
      "The base profile cannot be reused because its round registry changed.",
      call. = FALSE
    )
  }
  old_specs <- old_specs[match(current_specs$occurrence_id, old_specs$occurrence_id), , drop = FALSE]
  core <- c(
    "profile_kind", "generation_action", "support_source", "bin_spec_id",
    "routing_rule_id"
  )
  if (!all(core %in% names(old_specs)) || !all(core %in% names(current_specs))) {
    stop("The base profile has an incomplete profiling contract.", call. = FALSE)
  }
  changed <- Reduce(`|`, lapply(core, function(column) {
    old_specs[[column]] != current_specs[[column]]
  }))
  unchanged_bin_ids <- unique(current_specs$bin_spec_id[!changed])
  unchanged_bin_ids <- unchanged_bin_ids[nzchar(unchanged_bin_ids)]
  bin_core <- c(
    "bin_spec_id", "bin_id", "value_type", "lower", "upper",
    "boundary_rules", "sampling_rule"
  )
  old_bins <- profile$safe_bins[
    profile$safe_bins$bin_spec_id %in% unchanged_bin_ids, bin_core, drop = FALSE
  ]
  current_bins <- dictionary$safe_bins[
    dictionary$safe_bins$review_state == "approved" &
      dictionary$safe_bins$bin_spec_id %in% unchanged_bin_ids,
    bin_core, drop = FALSE
  ]
  order_bins <- function(rows) {
    rows <- rows[order(rows$bin_spec_id, rows$bin_id), , drop = FALSE]
    rownames(rows) <- NULL
    rows
  }
  if (!identical(order_bins(old_bins), order_bins(current_bins))) {
    stop(
      "The base profile cannot be reused because a fixed range used by unchanged fields changed.",
      call. = FALSE
    )
  }
  # rc14 changes the exact contents of three occurrence-specific public
  # supports while leaving the base profile's corresponding raw distributions
  # obsolete. Those case-sensitive categories require an explicit source
  # repair when rebasing an rc11 profile.
  public_support_changed_ids <- if (approved_rc11_transition) {
    dictionary$synthetic_profile_overrides$occurrence_id
  } else {
    character()
  }
  changed_ids <- unique(c(
    current_specs$occurrence_id[changed], public_support_changed_ids
  ))
  requires_query <- changed &
    current_specs$profile_kind != "identifier" &
    !(current_specs$generation_action %in% c("synthetic_identifier", "excluded"))
  list(
    changed_ids = changed_ids,
    repair_ids = unique(c(
      current_specs$occurrence_id[requires_query], public_support_changed_ids
    ))
  )
}

.validate_ct_profile_repair <- function(repair, occurrence_ids, round_ids) {
  expected_bins <- c("zero", "1_10", "11_20", "21_30", "31_40", "41_50")
  if (length(occurrence_ids) != 50L || length(round_ids) != 19L) {
    stop("The reviewed Ct/Cp repair scope must contain 50 fields in 19 rounds.", call. = FALSE)
  }
  if (!is.list(repair) || !is.data.frame(repair$numeric_bin_counts) ||
      !is.data.frame(repair$profiled_occurrences) ||
      !is.data.frame(repair$round_denominators)) {
    stop("The Ct/Cp repair has an incomplete profile structure.", call. = FALSE)
  }
  rows <- repair$numeric_bin_counts
  complete_ids <- unique(rows$occurrence_id)
  bin_sets <- split(rows$bin_id, rows$occurrence_id)
  complete_bins <- length(bin_sets) == length(occurrence_ids) &&
    all(vapply(
      bin_sets,
      function(values) length(values) == length(expected_bins) &&
        !anyDuplicated(values) && setequal(values, expected_bins),
      logical(1L)
    ))
  if (!setequal(complete_ids, occurrence_ids) ||
      nrow(rows) != length(occurrence_ids) * length(expected_bins) ||
      !complete_bins ||
      !setequal(
        repair$profiled_occurrences$occurrence_id[
          repair$profiled_occurrences$occurrence_id %in% occurrence_ids
        ],
        occurrence_ids
      )) {
    stop(
      "The Ct/Cp query did not return all six bands for all 50 fields; no repair was written.",
      call. = FALSE
    )
  }
  if (!setequal(repair$round_denominators$round_id, round_ids)) {
    stop(
      "The Ct/Cp query did not return a denominator for every affected round; no repair was written.",
      call. = FALSE
    )
  }
  outside <- is.data.frame(repair$issues) && nrow(repair$issues) > 0L &&
    any(repair$issues$code == "value_outside_safe_bins")
  if (outside) {
    stop(
      "One or more Ct/Cp values fell outside 0-50; review the fixed ranges before release.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.validate_lab_result_profile_repair <- function(repair, occurrence_ids,
                                                round_ids,
                                                dictionary = react_dictionary()) {
  if (length(occurrence_ids) != 37L || length(round_ids) != 19L) {
    stop(
      "The reviewed laboratory-result repair scope must contain 37 fields in 19 rounds.",
      call. = FALSE
    )
  }
  if (!is.list(repair) || !is.data.frame(repair$categorical_counts) ||
      !is.data.frame(repair$profiled_occurrences) ||
      !is.data.frame(repair$round_denominators) ||
      !is.data.frame(repair$issues)) {
    stop("The laboratory-result repair has an incomplete profile structure.", call. = FALSE)
  }
  rows <- repair$categorical_counts[
    repair$categorical_counts$occurrence_id %in% occurrence_ids,
    , drop = FALSE
  ]
  expected_support <- stats::setNames(lapply(
    occurrence_ids, .lab_result_support, dictionary = dictionary
  ), occurrence_ids)
  value_sets <- split(rows$value, rows$occurrence_id)
  complete_values <- length(value_sets) == length(occurrence_ids) &&
    all(vapply(occurrence_ids, function(id) {
      values <- value_sets[[id]]
      expected <- expected_support[[id]]$raw_value
      length(values) == length(expected) &&
        !anyDuplicated(values) && setequal(values, expected)
    }, logical(1L)))
  profiled_ids <- repair$profiled_occurrences$occurrence_id[
    repair$profiled_occurrences$occurrence_id %in% occurrence_ids
  ]
  expected_row_count <- sum(vapply(
    expected_support, nrow, integer(1L)
  ))
  if (nrow(rows) != expected_row_count ||
      !complete_values || !setequal(profiled_ids, occurrence_ids)) {
    stop(
      paste(
        "The laboratory-result query did not return each occurrence's exact",
        "approved support; expected", expected_row_count, "rows across 37 fields;",
        "no repair was written."
      ),
      call. = FALSE
    )
  }
  if (!setequal(repair$round_denominators$round_id, round_ids)) {
    stop(
      "The laboratory-result query did not return a denominator for every REACT-1 round; no repair was written.",
      call. = FALSE
    )
  }
  missing_statuses <- split(
    repair$missingness$status[
      repair$missingness$occurrence_id %in% occurrence_ids
    ],
    repair$missingness$occurrence_id[
      repair$missingness$occurrence_id %in% occurrence_ids
    ]
  )
  coded_complete <- all(vapply(occurrence_ids, function(id) {
    support <- expected_support[[id]]
    options <- data.frame(
      return_value = support$raw_value,
      display_value = support$label,
      outcome_state = support$outcome_state,
      stringsAsFactors = FALSE
    )
    expected <- paste0(
      "coded:", .coded_missing_domain(options)$return_value
    )
    all(expected %in% missing_statuses[[id]])
  }, logical(1L)))
  if (!coded_complete) {
    stop(
      paste(
        "One or more exact occurrence-specific missing result values were not",
        "retained as coded missing; no repair was written."
      ),
      call. = FALSE
    )
  }
  unrecognised <- repair$issues$code == "unrecognised_profile_code" &
    repair$issues$round_id %in% round_ids &
    repair$issues$variable %in% c("RESULT", "FINALRESULT")
  unrecognised[is.na(unrecognised)] <- FALSE
  outside <- is.data.frame(repair$missingness) &&
    all(c("occurrence_id", "status") %in% names(repair$missingness)) &&
    any(
      repair$missingness$occurrence_id %in% occurrence_ids &
        repair$missingness$status == "outside_safe_support"
    )
  if (any(unrecognised) || outside) {
    detail_rows <- repair$issues[
      unrecognised,
      intersect(c("round_id", "variable", "affected_count"), names(repair$issues)),
      drop = FALSE
    ]
    detail <- if (nrow(detail_rows)) {
      paste(unique(paste0(
        detail_rows$round_id, "/", detail_rows$variable,
        ifelse(
          nzchar(as.character(detail_rows$affected_count)),
          paste0(" (", detail_rows$affected_count, " records)"), ""
        )
      )), collapse = ", ")
    } else {
      "one or more affected round/field pairs"
    }
    stop(
      paste(
        "RESULT/FINALRESULT contains a value outside its occurrence-specific",
        "exact approved support in", detail,
        "; no repair was written."
      ),
      call. = FALSE
    )
  }

  profiled <- repair$profiled_occurrences[
    match(occurrence_ids, repair$profiled_occurrences$occurrence_id),
    , drop = FALSE
  ]
  denominator <- suppressWarnings(as.numeric(
    repair$round_denominators$count[
      match(profiled$round_id, repair$round_denominators$round_id)
    ]
  ))
  category_total <- vapply(occurrence_ids, function(id) {
    sum(suppressWarnings(as.numeric(rows$count[rows$occurrence_id == id])))
  }, numeric(1L))
  database_missing <- vapply(occurrence_ids, function(id) {
    values <- suppressWarnings(as.numeric(repair$missingness$count[
      repair$missingness$occurrence_id == id &
        repair$missingness$status == "database_missing"
    ]))
    if (length(values)) sum(values) else NA_real_
  }, numeric(1L))
  reconciled <- category_total + database_missing
  mismatch <- is.na(denominator) | is.na(reconciled) | reconciled != denominator
  if (any(mismatch)) {
    detail <- paste(
      paste0(profiled$round_id[mismatch], "/", profiled$variable[mismatch]),
      collapse = ", "
    )
    stop(
      paste(
        "Laboratory-result category and database-missing counts did not",
        "reconcile to the round denominator for", detail,
        "; no repair was written."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.recover_singleton_profile_categories <- function(profile) {
  if (!is.list(profile) || !all(.profile_v2_required %in% names(profile))) {
    stop("`profile` must be an existing schema-2 profile.", call. = FALSE)
  }
  dictionary <- react_dictionary()
  changes <- .profile_contract_rebase_ids(profile, dictionary)
  changed_ids <- changes$repair_ids
  if (!length(changed_ids)) {
    stop("The profile has no profiling-policy changes to recover.", call. = FALSE)
  }

  old_specs <- profile$profile_specs[
    match(changed_ids, profile$profile_specs$occurrence_id), , drop = FALSE
  ]
  approved_specs <- .approved_profile_specs(dictionary)
  current_specs <- approved_specs[
    match(changed_ids, approved_specs$occurrence_id), , drop = FALSE
  ]
  occurrences <- dictionary$occurrences[
    match(changed_ids, dictionary$occurrences$occurrence_id), , drop = FALSE
  ]
  administrative <- .administrative_missing_domain()$return_value

  mappings <- vector("list", length(changed_ids))
  recoverable <- logical(length(changed_ids))
  for (index in seq_along(changed_ids)) {
    options <- .profile_response_options(
      occurrences[index, , drop = FALSE],
      current_specs[index, , drop = FALSE],
      dictionary
    )
    options <- options[
      nzchar(options$return_value) &
        options$return_value != "NA" &
        !(options$return_value %in% administrative) &
        nzchar(options$display_value) &
        options$display_value != "NA",
      , drop = FALSE
    ]
    options$numeric_code <- suppressWarnings(as.numeric(options$return_value))
    bins <- profile$safe_bins[
      profile$safe_bins$bin_spec_id == old_specs$bin_spec_id[[index]],
      , drop = FALSE
    ]
    bins$lower_numeric <- suppressWarnings(as.numeric(bins$lower))
    bins$upper_numeric <- suppressWarnings(as.numeric(bins$upper))
    bins <- bins[
      !is.na(bins$lower_numeric) & !is.na(bins$upper_numeric) &
        bins$lower_numeric == bins$upper_numeric,
      , drop = FALSE
    ]
    bin_match <- match(options$numeric_code, bins$lower_numeric)
    recoverable[[index]] <- nrow(options) > 0L &&
      all(!is.na(options$numeric_code)) && all(!is.na(bin_match)) &&
      !anyDuplicated(bins$bin_id[bin_match])
    if (!recoverable[[index]]) next
    mappings[[index]] <- data.frame(
      occurrence_id = changed_ids[[index]],
      bin_spec_id = old_specs$bin_spec_id[[index]],
      bin_id = bins$bin_id[bin_match],
      value = options$return_value,
      display_value = options$display_value,
      stringsAsFactors = FALSE
    )
  }

  mapping <- .bind_rows(mappings[recoverable], data.frame())
  recovered_ids <- changed_ids[recoverable]
  remaining_ids <- changed_ids[!recoverable]
  numeric_rows <- profile$numeric_bin_counts[
    profile$numeric_bin_counts$occurrence_id %in% recovered_ids,
    , drop = FALSE
  ]
  recovered <- merge(
    numeric_rows, mapping,
    by = c("occurrence_id", "bin_spec_id", "bin_id"),
    all = FALSE, sort = FALSE
  )
  recovered <- recovered[
    , c(
      "occurrence_id", "round_id", "value", "display_value",
      "count", "suppressed"
    ),
    drop = FALSE
  ]
  if (!setequal(unique(recovered$occurrence_id), recovered_ids)) {
    stop("One or more singleton category distributions could not be recovered.", call. = FALSE)
  }

  out <- profile
  out$categorical_counts <- .bind_rows(list(
    out$categorical_counts[
      !(out$categorical_counts$occurrence_id %in% recovered_ids),
      , drop = FALSE
    ],
    recovered
  ), out$categorical_counts)
  out$numeric_bin_counts <- out$numeric_bin_counts[
    !(out$numeric_bin_counts$occurrence_id %in% recovered_ids),
    , drop = FALSE
  ]
  out$profile_specs <- dictionary$synthetic_profile_specs[
    dictionary$synthetic_profile_specs$review_state == "approved",
    , drop = FALSE
  ]
  required_bins <- unique(
    out$profile_specs$bin_spec_id[nzchar(out$profile_specs$bin_spec_id)]
  )
  out$safe_bins <- dictionary$safe_bins[
    dictionary$safe_bins$review_state == "approved" &
      dictionary$safe_bins$bin_spec_id %in% required_bins,
    , drop = FALSE
  ]
  for (name in setdiff(.profile_v2_optional, "profiled_occurrences")) {
    out[[name]] <- NULL
  }
  out$profiled_occurrences <- data.frame(
    occurrence_id = recovered_ids,
    round_id = occurrences$round_id[match(recovered_ids, occurrences$occurrence_id)],
    variable = occurrences$variable[match(recovered_ids, occurrences$occurrence_id)],
    profile_kind = "categorical",
    status = "recovered_from_approved_singleton_bins",
    stringsAsFactors = FALSE
  )

  version <- react_dictionary_version()
  replace_keys <- c(
    "profile_version", "package_version", "status", "disclosure_approval",
    "dictionary_release", "dictionary_manifest_sha256",
    "routing_specification_sha256", "profile_scope",
    "profiled_occurrence_count", "locally_recovered_occurrence_count",
    "remaining_repair_occurrence_count", "overall_distributions",
    "contract_rebased", "base_dictionary_manifest_sha256"
  )
  metadata <- .synthetic_profile_metadata(profile)
  out$metadata <- out$metadata[
    !(out$metadata$key %in% replace_keys), , drop = FALSE
  ]
  out$metadata <- rbind(
    out$metadata,
    data.frame(
      key = replace_keys,
      value = c(
        "enclave-profile-v2-singleton-recovery", as.character(utils::packageVersion("reactextract")),
        "requires_targeted_enclave_repair", "not_approved",
        version$dictionary_release, version$manifest_sha256,
        .profile_object_sha256(dictionary$routing_rules),
        "complete_with_local_singleton_recovery",
        as.character(length(recovered_ids)), as.character(length(recovered_ids)),
        as.character(length(remaining_ids)),
        "not_available_after_targeted_repair", "true",
        unname(metadata[["dictionary_manifest_sha256"]])
      ),
      stringsAsFactors = FALSE
    )
  )
  class(out) <- c("react_profile_v2", "list")
  list(
    profile = out,
    recovered_occurrence_ids = recovered_ids,
    remaining_occurrence_ids = remaining_ids
  )
}

#' Merge a targeted profile repair into an existing safe profile
#'
#' The repair is disclosure-controlled independently, then replaces only the
#' exact occurrence rows it requested. Existing routing aggregates are retained.
#' All-round totals are removed because they cannot be reconstructed from
#' already-suppressed round tables.
#'
#' @param profile An existing profile read by [react_read_profile()].
#' @param repair An unsuppressed targeted profile returned by
#'   [react_profile_source()].
#' @param policy A disclosure-control policy from [react_sdc_policy()].
#' @return A repaired profile that still requires normal disclosure approval.
#' @export
react_repair_profile <- function(profile, repair, policy = react_sdc_policy()) {
  if (!is.list(profile) || !all(.profile_v2_required %in% names(profile))) {
    stop("`profile` must be an existing schema-2 profile.", call. = FALSE)
  }
  if (!is.list(repair) || !all(.profile_v2_required %in% names(repair)) ||
      !is.data.frame(repair$profiled_occurrences) ||
      nrow(repair$profiled_occurrences) == 0L) {
    stop("`repair` must be a non-empty targeted profile.", call. = FALSE)
  }
  metadata <- .synthetic_profile_metadata(profile)
  repair_metadata <- .synthetic_profile_metadata(repair)
  expected_hash <- react_dictionary_version()$manifest_sha256[[1L]]
  base_hash <- unname(metadata[["dictionary_manifest_sha256"]])
  repair_hash <- unname(repair_metadata[["dictionary_manifest_sha256"]])
  if (is.na(repair_hash) || !identical(repair_hash, expected_hash)) {
    stop("The repair and installed dictionary hashes must match.", call. = FALSE)
  }
  if (!identical(unname(repair_metadata[["profile_scope"]]), "targeted_repair")) {
    stop("`repair` was not created as a targeted occurrence repair.", call. = FALSE)
  }

  repair_ids <- unique(repair$profiled_occurrences$occurrence_id)
  dictionary <- react_dictionary()
  rebased <- !identical(base_hash, expected_hash)
  contract_changes <- if (rebased) {
    .profile_contract_rebase_ids(profile, dictionary)
  } else {
    list(changed_ids = character(), repair_ids = character())
  }
  missing_repairs <- setdiff(contract_changes$repair_ids, repair_ids)
  if (length(missing_repairs)) {
    stop(
      "The targeted repair omitted ", length(missing_repairs),
      " occurrence(s) whose profiling policy changed.",
      call. = FALSE
    )
  }

  safe_repair <- react_prepare_profile_export(repair, policy = policy)
  out <- profile
  replace_ids <- unique(c(repair_ids, contract_changes$changed_ids))
  for (name in c(
    "missingness", "categorical_counts", "numeric_bin_counts", "text_presence"
  )) {
    base <- out[[name]]
    replacement <- safe_repair[[name]]
    if (is.data.frame(base) && "occurrence_id" %in% names(base)) {
      base <- base[!(base$occurrence_id %in% replace_ids), , drop = FALSE]
    }
    out[[name]] <- .bind_rows(list(base, replacement), base)
  }

  replace_occurrences <- dictionary$occurrences[
    dictionary$occurrences$occurrence_id %in% replace_ids, , drop = FALSE
  ]
  repair_keys <- paste(
    replace_occurrences$round_id,
    replace_occurrences$variable,
    sep = "\r"
  )
  base_issue_key <- paste(out$issues$round_id, out$issues$variable, sep = "\r")
  out$issues <- .bind_rows(list(
    out$issues[!(base_issue_key %in% repair_keys), , drop = FALSE],
    safe_repair$issues
  ), .empty_issues())
  out$profiled_occurrences <- repair$profiled_occurrences
  for (name in setdiff(.profile_v2_optional, "profiled_occurrences")) out[[name]] <- NULL

  if (rebased) {
    out$profile_specs <- .approved_profile_specs(dictionary)
    required_bins <- unique(out$profile_specs$bin_spec_id[nzchar(out$profile_specs$bin_spec_id)])
    out$safe_bins <- dictionary$safe_bins[
      dictionary$safe_bins$review_state == "approved" &
        dictionary$safe_bins$bin_spec_id %in% required_bins,
      , drop = FALSE
    ]
  }

  replace_keys <- c(
    "profile_version", "package_version", "status", "disclosure_approval",
    "safe_prior_fraction", "profile_scope", "profiled_occurrence_count",
    "overall_distributions", "repair_elapsed_seconds", "contract_rebased",
    "base_dictionary_manifest_sha256"
  )
  if (rebased) {
    out$metadata <- out$metadata[!(out$metadata$key %in% c(
      "dictionary_release", "dictionary_manifest_sha256",
      "routing_specification_sha256"
    )), , drop = FALSE]
    version <- react_dictionary_version()
    out$metadata <- rbind(
      out$metadata,
      data.frame(
        key = c(
          "dictionary_release", "dictionary_manifest_sha256",
          "routing_specification_sha256"
        ),
        value = c(
          version$dictionary_release, version$manifest_sha256,
          .profile_object_sha256(dictionary$routing_rules)
        ),
        stringsAsFactors = FALSE
      )
    )
  }
  out$metadata <- out$metadata[!(out$metadata$key %in% replace_keys), , drop = FALSE]
  out$metadata <- rbind(
    out$metadata,
    data.frame(
      key = replace_keys,
      value = c(
        if (rebased) "enclave-profile-v2-rebased-repaired" else "enclave-profile-v1-repaired",
        as.character(utils::packageVersion("reactextract")),
        "requires_enclave_disclosure_review", "not_approved", "0.01",
        "complete_with_targeted_repair", as.character(length(repair_ids)),
        "not_available_after_targeted_repair",
        unname(repair_metadata[["elapsed_seconds"]]),
        if (rebased) "true" else "false",
        if (rebased) base_hash else expected_hash
      ),
      stringsAsFactors = FALSE
    )
  )
  class(out) <- c("react_profile_v2", "list")
  out
}
