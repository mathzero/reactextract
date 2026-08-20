.empty_harmonised_values <- function() {
  typed <- .typed_value_frame(character())
  data.frame(
    observation_id = character(),
    round_id = character(),
    concept_id = character(),
    output_column = character(),
    mapping_id = character(),
    transform_id = character(),
    transform_version = character(),
    declared_output_type = character(),
    source_occurrence_id = character(),
    source_raw_variable = character(),
    missing_reason = character(),
    source_status = character(),
    typed,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.parse_code_map <- function(value) {
  entries <- strsplit(value, "|", fixed = TRUE)[[1]]
  keys <- sub("=.*$", "", entries)
  values <- sub("^[^=]*=", "", entries)
  stats::setNames(values, keys)
}

.set_typed_output <- function(out, index, value, output_type) {
  if (length(index) == 0L) {
    return(out)
  }
  if (output_type == "logical") {
    parsed <- ifelse(value == "TRUE", TRUE, ifelse(value == "FALSE", FALSE, NA))
    valid <- !is.na(parsed)
    out$value_type[index[valid]] <- "logical"
    out$value_logical[index[valid]] <- parsed[valid]
  } else if (output_type == "integer") {
    parsed <- suppressWarnings(as.integer(value))
    valid <- !is.na(parsed)
    out$value_type[index[valid]] <- "integer"
    out$value_integer[index[valid]] <- parsed[valid]
  } else if (output_type == "double") {
    parsed <- suppressWarnings(as.double(value))
    valid <- !is.na(parsed)
    out$value_type[index[valid]] <- "double"
    out$value_double[index[valid]] <- parsed[valid]
  } else if (output_type == "date") {
    parsed <- suppressWarnings(as.Date(value))
    valid <- !is.na(parsed)
    out$value_type[index[valid]] <- "date"
    out$value_date[index[valid]] <- parsed[valid]
  } else {
    out$value_type[index] <- "character"
    out$value_character[index] <- value
  }
  out
}

.apply_recode <- function(raw, transform) {
  raw_text <- .typed_to_character(raw)
  n <- nrow(raw)
  out <- .typed_value_frame(rep(NA_character_, n))
  out$value_type[] <- transform$output_type[[1]]
  missing_reason <- rep("", n)
  source_status <- rep("harmonised", n)
  input_missing <- raw$source_is_missing
  missing_reason[input_missing] <- "input_missing"
  source_status[input_missing] <- "input_missing"
  code_map <- .parse_code_map(transform$code_map[[1]])
  missing_labels <- c(
    NA_item = "item_nonresponse",
    NA_not_applicable = "not_applicable",
    NA_survey_nonresponse = "survey_nonresponse",
    NA_unknown = "unknown_missing"
  )
  recognised <- input_missing
  for (key in setdiff(names(code_map), "other")) {
    index <- which(!input_missing & raw_text == key)
    if (length(index) == 0L) {
      next
    }
    recognised[index] <- TRUE
    mapped <- code_map[[key]]
    if (mapped %in% names(missing_labels)) {
      missing_reason[index] <- missing_labels[[mapped]]
      source_status[index] <- "coded_missing"
    } else {
      out <- .set_typed_output(out, index, rep(mapped, length(index)), transform$output_type[[1]])
    }
  }
  unrecognised <- which(!recognised)
  if (length(unrecognised) > 0L) {
    other <- unname(code_map[["other"]])
    if (!is.null(other) && identical(other, "retain_raw")) {
      out$value_type[unrecognised] <- "character"
      out$value_character[unrecognised] <- raw_text[unrecognised]
      missing_reason[unrecognised] <- "unrecognised_raw_retained"
      source_status[unrecognised] <- "unrecognised_raw_retained"
    } else {
      missing_reason[unrecognised] <- "unrecognised_raw"
      source_status[unrecognised] <- "unrecognised_raw"
    }
  }
  list(values = out, missing_reason = missing_reason, source_status = source_status)
}

.apply_identity <- function(raw) {
  list(
    values = raw[c(
      "value_type", "value_logical", "value_integer", "value_double",
      "value_date", "value_character"
    )],
    missing_reason = ifelse(raw$source_is_missing, "input_missing", ""),
    source_status = ifelse(raw$source_is_missing, "input_missing", "harmonised")
  )
}

.clear_typed_values <- function(values, index) {
  values$value_logical[index] <- NA
  values$value_integer[index] <- NA_integer_
  values$value_double[index] <- NA_real_
  values$value_date[index] <- as.Date(NA_character_)
  values$value_character[index] <- NA_character_
  values
}

.apply_source_coding <- function(raw, output_plan, lookup) {
  plan_index <- match(raw$occurrence_id, output_plan$occurrence_id)
  plan <- output_plan[plan_index, , drop = FALSE]
  raw_text <- .typed_to_character(raw)
  values <- raw[c(
    "value_type", "value_logical", "value_integer", "value_double",
    "value_date", "value_character"
  )]
  missing_reason <- rep("", nrow(raw))
  source_status <- rep("source_preserved", nrow(raw))

  lookup_key <- paste(
    lookup$concept_id, lookup$round_id, lookup$raw_field, lookup$raw_value,
    sep = "\r"
  )
  raw_key <- paste(
    plan$concept_id, raw$round_id, raw$raw_variable, raw_text,
    sep = "\r"
  )
  lookup_index <- match(raw_key, lookup_key)
  input_missing <- raw$source_is_missing
  lookup_index[input_missing] <- NA_integer_

  field_key <- paste(lookup$concept_id, lookup$round_id, lookup$raw_field, sep = "\r")
  first_field <- !duplicated(field_key)
  field_mode <- stats::setNames(lookup$field_value_mode[first_field], field_key[first_field])
  raw_field_key <- paste(plan$concept_id, raw$round_id, raw$raw_variable, sep = "\r")
  raw_mode <- unname(field_mode[raw_field_key])

  missing_reason[input_missing] <- "input_missing"
  source_status[input_missing] <- "input_missing"

  matched <- !is.na(lookup_index)
  status <- rep("", nrow(raw))
  harmonised_label <- rep("", nrow(raw))
  status[matched] <- lookup$mapping_status[lookup_index[matched]]
  harmonised_label[matched] <- lookup$harmonised_value[lookup_index[matched]]

  coded_missing <- which(matched & status == "standardized_missing")
  if (length(coded_missing) > 0L) {
    values <- .clear_typed_values(values, coded_missing)
    missing_reason[coded_missing] <- switch_missing_reason(
      harmonised_label[coded_missing]
    )
    source_status[coded_missing] <- "coded_missing"
  }

  labelled <- which(matched & status %in% c("source_label", "inferred_binary"))
  if (length(labelled) > 0L) {
    values <- .clear_typed_values(values, labelled)
    values$value_type[labelled] <- "character"
    values$value_character[labelled] <- harmonised_label[labelled]
    source_status[labelled] <- "source_coded"
  }

  ambiguous <- which(matched & status == "ambiguous_source_labels")
  source_status[ambiguous] <- "source_preserved_ambiguous"

  unrecognised <- which(!input_missing & !matched & raw_mode == "coded")
  if (length(unrecognised) > 0L) {
    source_status[unrecognised] <- "unrecognised_raw_retained"
    missing_reason[unrecognised] <- "unrecognised_raw_retained"
  }

  rows <- data.frame(
    observation_id = raw$observation_id,
    round_id = raw$round_id,
    concept_id = plan$concept_id,
    output_column = plan$output_column,
    mapping_id = rep("", nrow(raw)),
    transform_id = rep("source-preserving-coding", nrow(raw)),
    transform_version = rep("1", nrow(raw)),
    declared_output_type = rep("source", nrow(raw)),
    source_occurrence_id = raw$occurrence_id,
    source_raw_variable = raw$raw_variable,
    missing_reason = missing_reason,
    source_status = source_status,
    values,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  issue_rows <- list()
  if (length(unrecognised) > 0L) {
    groups <- split(
      unrecognised,
      paste(raw$round_id[unrecognised], raw$raw_variable[unrecognised], sep = "\r")
    )
    issue_rows <- lapply(groups, function(index) {
      .issue(
        "warning", "harmonise", "unrecognised_source_code_retained",
        "A coded source value absent from the reviewed response options was retained exactly.",
        raw$round_id[index[[1]]], variable = raw$raw_variable[index[[1]]],
        affected_count = length(index)
      )
    })
  }
  list(data = rows, issues = .bind_rows(issue_rows, .empty_issues()))
}

switch_missing_reason <- function(label) {
  unname(c(
    "Item non-response" = "item_nonresponse",
    "Not applicable" = "not_applicable",
    "Survey non-response" = "survey_nonresponse"
  )[label])
}

.duration_labels <- c(
  "1" = "Less than four weeks",
  "2" = "Four weeks up to two months",
  "3" = "Two months up to three months",
  "4" = "Three months up to six months",
  "5" = "More than six months",
  "6" = "Cannot give an estimate",
  "7" = "Prefer not to say"
)

.duration_missing_from_codes <- function(values, source_missing) {
  if (all(source_missing)) return("input_missing")
  values <- unique(values[!source_missing & !is.na(values) & nzchar(values)])
  if (!length(values)) return("input_missing")
  known <- c(
    "-92" = "item_nonresponse",
    "-91" = "not_applicable",
    "-77" = "survey_nonresponse",
    "-66" = "source_missing_-66",
    "-99" = "source_missing_-99"
  )
  reasons <- unique(unname(known[values]))
  if (anyNA(reasons)) return("unrecognised_duration_input")
  if (length(reasons) == 1L) reasons else "multiple_source_missing_codes"
}

.grouped_output_rows <- function(observation_id, round_id, group, inputs,
                                 values, missing_reason, source_status) {
  typed <- .typed_value_frame(rep(NA_character_, length(observation_id)))
  available <- !is.na(values)
  typed$value_type[] <- "character"
  typed$value_character[available] <- values[available]
  data.frame(
    observation_id = observation_id,
    round_id = rep(round_id, length(observation_id)),
    concept_id = rep(group$concept_id[[1]], length(observation_id)),
    output_column = rep(group$output_column[[1]], length(observation_id)),
    mapping_id = rep("", length(observation_id)),
    transform_id = rep(group$decision_id[[1]], length(observation_id)),
    transform_version = rep("1.0.0", length(observation_id)),
    declared_output_type = rep("character", length(observation_id)),
    source_occurrence_id = rep(
      paste(unique(inputs$occurrence_id), collapse = "|"),
      length(observation_id)
    ),
    source_raw_variable = rep(
      paste(unique(inputs$source_variable), collapse = "|"),
      length(observation_id)
    ),
    missing_reason = missing_reason,
    source_status = source_status,
    typed,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.apply_grouped_categorical <- function(raw, group, inputs) {
  raw_text <- .typed_to_character(raw)
  input_missing <- raw$source_is_missing
  valid <- !input_missing & raw_text %in% names(.duration_labels)
  coded_missing <- !input_missing & raw_text %in% c("-92", "-91", "-77")
  unexpected <- !input_missing & !valid & !coded_missing
  value <- rep(NA_character_, nrow(raw))
  value[valid] <- unname(.duration_labels[raw_text[valid]])
  # Retaining an unexpected value in the detailed output is safer than erasing it.
  value[unexpected] <- raw_text[unexpected]
  missing_reason <- rep("", nrow(raw))
  missing_reason[input_missing] <- "input_missing"
  missing_reason[raw_text == "-92" & !input_missing] <- "item_nonresponse"
  missing_reason[raw_text == "-91" & !input_missing] <- "not_applicable"
  missing_reason[raw_text == "-77" & !input_missing] <- "survey_nonresponse"
  missing_reason[unexpected] <- "unrecognised_raw_retained"
  source_status <- ifelse(
    input_missing, "input_missing",
    ifelse(
      coded_missing, "coded_missing",
      ifelse(unexpected, "unrecognised_raw_retained", "harmonised")
    )
  )
  rows <- .grouped_output_rows(
    raw$observation_id, raw$round_id[[1]], group, inputs,
    value, missing_reason, source_status
  )
  issues <- if (any(unexpected)) .issue(
    "warning", "harmonise", "unrecognised_duration_category_retained",
    "An unexpected Long COVID duration category was retained exactly.",
    raw$round_id[[1]], variable = inputs$source_variable[[1]],
    affected_count = sum(unexpected)
  ) else .empty_issues()
  list(data = rows, issues = issues)
}

.apply_grouped_continuous <- function(raw, group, inputs) {
  observation_id <- unique(raw$observation_id)
  components <- unique(inputs$component_id)
  durations <- matrix(NA_real_, nrow = length(observation_id), ncol = length(components))
  conflicts <- matrix(FALSE, nrow = length(observation_id), ncol = length(components))
  raw_text <- matrix(NA_character_, nrow = length(observation_id), ncol = nrow(inputs))
  source_missing <- matrix(TRUE, nrow = length(observation_id), ncol = nrow(inputs))

  for (input_index in seq_len(nrow(inputs))) {
    source <- raw[raw$occurrence_id == inputs$occurrence_id[[input_index]], , drop = FALSE]
    aligned <- match(observation_id, source$observation_id)
    present <- !is.na(aligned)
    if (any(present)) {
      raw_text[present, input_index] <- .typed_to_character(source[aligned[present], , drop = FALSE])
      source_missing[present, input_index] <- source$source_is_missing[aligned[present]]
    }
  }

  for (component_index in seq_along(components)) {
    component <- components[[component_index]]
    component_inputs <- which(inputs$component_id == component)
    day_column <- component_inputs[inputs$input_role[component_inputs] == "days"]
    week_column <- component_inputs[inputs$input_role[component_inputs] == "weeks"]
    days <- rep(NA_real_, length(observation_id))
    weeks <- rep(NA_real_, length(observation_id))
    if (length(day_column) == 1L) {
      days <- suppressWarnings(as.numeric(raw_text[, day_column]))
    }
    if (length(week_column) == 1L) {
      weeks <- suppressWarnings(as.numeric(raw_text[, week_column]))
    }
    day_valid <- !is.na(days) & days >= 0
    week_valid <- !is.na(weeks) & weeks >= 0
    conflicts[, component_index] <- day_valid & week_valid & days > 0 & weeks > 0
    durations[, component_index] <- ifelse(
      conflicts[, component_index],
      NA_real_,
      ifelse(
        day_valid & days > 0,
        days,
        ifelse(
          week_valid & weeks > 0,
          weeks * 7,
          ifelse(day_valid | week_valid, 0, NA_real_)
        )
      )
    )
  }

  any_conflict <- rowSums(conflicts) > 0L
  has_duration <- rowSums(!is.na(durations)) > 0L
  duration_days <- rep(NA_real_, length(observation_id))
  if (any(has_duration)) {
    if (group$combine_rule[[1]] == "longest_component") {
      duration_days[has_duration] <- apply(
        durations[has_duration, , drop = FALSE], 1L, max, na.rm = TRUE
      )
    } else {
      duration_days[has_duration] <- apply(
        durations[has_duration, , drop = FALSE], 1L,
        function(value) value[which(!is.na(value))[[1]]]
      )
    }
  }
  duration_days[any_conflict] <- NA_real_
  bands <- cut(
    duration_days,
    breaks = c(-Inf, 28, 61, 92, 183, Inf),
    right = FALSE,
    labels = unname(.duration_labels[as.character(1:5)])
  )
  value <- as.character(bands)
  missing_reason <- rep("", length(observation_id))
  missing_reason[any_conflict] <- "conflicting_duration_inputs"
  unresolved <- which(!any_conflict & is.na(duration_days))
  for (index in unresolved) {
    missing_reason[[index]] <- .duration_missing_from_codes(
      raw_text[index, ], source_missing[index, ]
    )
  }
  source_status <- ifelse(
    any_conflict, "conflicting_duration_inputs",
    ifelse(is.na(duration_days), "coded_missing", "harmonised")
  )
  rows <- .grouped_output_rows(
    observation_id, raw$round_id[[1]], group, inputs,
    value, missing_reason, source_status
  )
  issues <- if (any(any_conflict)) .issue(
    "warning", "harmonise", "conflicting_duration_inputs",
    "Both DAYS and WEEKS were positive; raw values were retained and the cleaned value was left unresolved.",
    raw$round_id[[1]], variable = paste(inputs$source_variable, collapse = "|"),
    affected_count = sum(any_conflict)
  ) else .empty_issues()
  list(data = rows, issues = issues)
}

.apply_reviewed_groups <- function(raw_values, dictionary) {
  groups <- dictionary$harmonisation_groups
  inputs <- dictionary$harmonisation_inputs
  if (is.null(groups) || is.null(inputs) || !nrow(raw_values)) {
    return(list(
      data = .empty_harmonised_values(), issues = .empty_issues(),
      occurrence_ids = character()
    ))
  }
  inputs <- inputs[inputs$occurrence_id %in% unique(raw_values$occurrence_id), , drop = FALSE]
  groups <- groups[groups$decision_id %in% unique(inputs$decision_id), , drop = FALSE]
  rows <- list()
  issues <- list()
  for (group_index in seq_len(nrow(groups))) {
    group <- groups[group_index, , drop = FALSE]
    group_inputs <- inputs[inputs$decision_id == group$decision_id[[1]], , drop = FALSE]
    for (round_id in unique(group_inputs$round_id)) {
      round_inputs <- group_inputs[group_inputs$round_id == round_id, , drop = FALSE]
      raw <- raw_values[
        raw_values$round_id == round_id &
          raw_values$occurrence_id %in% round_inputs$occurrence_id,
        , drop = FALSE
      ]
      if (!nrow(raw)) next
      applied <- if (group$operation[[1]] == "categorical_identity") {
        .apply_grouped_categorical(raw, group, round_inputs)
      } else if (all(round_inputs$input_role == "categorical")) {
        .apply_grouped_categorical(raw, group, round_inputs)
      } else {
        .apply_grouped_continuous(raw, group, round_inputs)
      }
      rows[[length(rows) + 1L]] <- applied$data
      issues[[length(issues) + 1L]] <- applied$issues
    }
  }
  list(
    data = .bind_rows(rows, .empty_harmonised_values()),
    issues = .bind_rows(issues, .empty_issues()),
    occurrence_ids = unique(inputs$occurrence_id)
  )
}

.harmonise_values <- function(raw_values, dictionary, mappings = NULL,
                              output_plan = NULL) {
  if (is.null(mappings)) {
    mappings <- dictionary$mappings
  }
  if (is.null(output_plan)) {
    output_plan <- dictionary$concept_output_columns
  }
  if (nrow(raw_values) == 0L || nrow(output_plan) == 0L) {
    return(list(data = .empty_harmonised_values(), issues = .empty_issues()))
  }
  available_occurrences <- unique(raw_values$occurrence_id)
  mappings <- mappings[
    mappings$review_state == "approved" &
      mappings$collection_status == "asked" &
      mappings$occurrence_id %in% available_occurrences,
    ,
    drop = FALSE
  ]
  raw_groups <- split(seq_len(nrow(raw_values)), raw_values$occurrence_id)
  transforms <- dictionary$transforms
  rows <- list()
  issues <- list()
  used <- 0L
  grouped <- .apply_reviewed_groups(raw_values, dictionary)
  if (nrow(grouped$data) > 0L) {
    used <- used + 1L
    rows[[used]] <- grouped$data
  }
  issues[[length(issues) + 1L]] <- grouped$issues
  mappings <- mappings[
    !(mappings$occurrence_id %in% grouped$occurrence_ids),
    , drop = FALSE
  ]
  for (i in seq_len(nrow(mappings))) {
    raw_index <- raw_groups[[mappings$occurrence_id[[i]]]]
    transform_index <- match(mappings$transform_id[[i]], transforms$transform_id)
    if (is.na(transform_index)) {
      issues[[length(issues) + 1L]] <- .issue(
        "error", "harmonise", "transform_missing",
        "An approved mapping references a transform absent from the pinned bundle.",
        raw_values$round_id[[raw_index[[1]]]],
        variable = raw_values$raw_variable[[raw_index[[1]]]],
        affected_count = length(raw_index)
      )
      next
    }
    transform <- transforms[transform_index, , drop = FALSE]
    raw <- raw_values[raw_index, , drop = FALSE]
    operation <- transform$operation[[1]]
    applied <- if (operation == "recode") {
      .apply_recode(raw, transform)
    } else if (operation %in% c("identity", "rename")) {
      .apply_identity(raw)
    } else {
      issues[[length(issues) + 1L]] <- .issue(
        "warning", "harmonise", "unsupported_transform_operation",
        paste0("Reviewed transform operation `", operation, "` is not executable in this package release."),
        raw$round_id[[1]], variable = raw$raw_variable[[1]],
        affected_count = nrow(raw)
      )
      next
    }
    used <- used + 1L
    rows[[used]] <- data.frame(
      observation_id = raw$observation_id,
      round_id = raw$round_id,
      concept_id = rep(mappings$concept_id[[i]], nrow(raw)),
      output_column = rep(
        output_plan$output_column[
          match(mappings$occurrence_id[[i]], output_plan$occurrence_id)
        ],
        nrow(raw)
      ),
      mapping_id = rep(mappings$mapping_id[[i]], nrow(raw)),
      transform_id = rep(transform$transform_id[[1]], nrow(raw)),
      transform_version = rep(transform$transform_version[[1]], nrow(raw)),
      declared_output_type = rep(transform$output_type[[1]], nrow(raw)),
      source_occurrence_id = raw$occurrence_id,
      source_raw_variable = raw$raw_variable,
      missing_reason = applied$missing_reason,
      source_status = applied$source_status,
      applied$values,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    unrecognised <- applied$missing_reason == "unrecognised_raw_retained"
    if (any(unrecognised)) {
      issues[[length(issues) + 1L]] <- .issue(
        "warning", "harmonise", "unrecognised_raw_retained",
        "Unrecognised source codes were retained as exact character values.",
        raw$round_id[[1]], variable = raw$raw_variable[[1]],
        affected_count = sum(unrecognised)
      )
    }
  }

  mapped_occurrences <- mappings$occurrence_id
  fallback_plan <- output_plan[
    output_plan$occurrence_id %in% available_occurrences &
      !(output_plan$occurrence_id %in% c(mapped_occurrences, grouped$occurrence_ids)),
    ,
    drop = FALSE
  ]
  if (nrow(fallback_plan) > 0L) {
    fallback_index <- unlist(
      raw_groups[fallback_plan$occurrence_id],
      use.names = FALSE
    )
    fallback_raw <- raw_values[fallback_index, , drop = FALSE]
    source_coded <- .apply_source_coding(
      fallback_raw,
      fallback_plan,
      dictionary$concept_value_lookup
    )
    used <- used + 1L
    rows[[used]] <- source_coded$data
    issues[[length(issues) + 1L]] <- source_coded$issues
  }
  list(
    data = .bind_rows(rows[seq_len(used)], .empty_harmonised_values()),
    issues = .bind_rows(issues, .empty_issues())
  )
}
