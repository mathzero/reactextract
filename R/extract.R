.resolve_rounds <- function(requested, rounds) {
  if (length(requested) == 1L && identical(requested, "all")) {
    return(rounds$round_id)
  }
  if (!is.character(requested) || length(requested) == 0L || anyNA(requested)) {
    stop("`rounds` must be `all` or a non-empty character vector.", call. = FALSE)
  }
  resolved <- vapply(requested, .resolve_round_name, character(1), rounds = rounds)
  if (anyNA(resolved)) {
    stop(
      "Unknown round or survey ID: ",
      paste(requested[is.na(resolved)], collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  unique(rounds$round_id[rounds$round_id %in% resolved])
}

.taxonomy_descendants <- function(selected, taxonomy) {
  included <- selected
  repeat {
    children <- taxonomy$taxonomy_id[taxonomy$parent_taxonomy_id %in% included]
    expanded <- unique(c(included, children))
    if (identical(expanded, included)) {
      break
    }
    included <- expanded
  }
  included
}

.resolve_families <- function(requested, taxonomy) {
  if (length(requested) == 1L && identical(requested, "all")) {
    return(taxonomy$taxonomy_id)
  }
  if (!is.character(requested) || length(requested) == 0L || anyNA(requested)) {
    stop("`families` must be `all` or a non-empty character vector.", call. = FALSE)
  }
  family_keys <- vapply(taxonomy$taxonomy_id, .family_key, character(1))
  resolved <- character()
  for (request in requested) {
    matches <- which(
      taxonomy$taxonomy_id == request |
        family_keys == request |
        taxonomy$label == request
    )
    if (length(matches) != 1L) {
      stop("Unknown or ambiguous feature family: ", request, ".", call. = FALSE)
    }
    resolved <- c(resolved, taxonomy$taxonomy_id[[matches]])
  }
  .taxonomy_descendants(unique(resolved), taxonomy)
}

.select_occurrences <- function(dictionary, families, rounds, concepts = NULL) {
  round_ids <- .resolve_rounds(rounds, dictionary$rounds)
  taxonomy_ids <- .resolve_families(families, dictionary$taxonomy)
  occurrences <- dictionary$occurrences[
    dictionary$occurrences$round_id %in% round_ids &
      dictionary$occurrences$primary_taxonomy_id %in% taxonomy_ids,
    ,
    drop = FALSE
  ]
  if (!is.null(concepts)) {
    if (!is.character(concepts) || length(concepts) == 0L || anyNA(concepts)) {
      stop("`concepts` must be NULL or a non-empty character vector.", call. = FALSE)
    }
    unknown <- setdiff(concepts, dictionary$concepts$concept_id)
    if (length(unknown) > 0L) {
      stop("Unknown concept ID: ", paste(unknown, collapse = ", "), ".", call. = FALSE)
    }
    occurrences <- occurrences[
      occurrences$primary_concept_id %in% concepts,
      ,
      drop = FALSE
    ]
  }
  round_order <- match(occurrences$round_id, dictionary$rounds$round_id)
  occurrence_order <- suppressWarnings(as.integer(occurrences$display_order))
  occurrences[order(round_order, occurrence_order), , drop = FALSE]
}

.quoted_field <- function(field) {
  paste0('"', field, '"')
}

.oracle_query <- function(source, fields, object, key) {
  sql <- paste0(
    "SELECT ",
    paste(vapply(fields, .quoted_field, character(1)), collapse = ", "),
    " FROM ",
    object,
    " ORDER BY ",
    .quoted_field(key)
  )
  source$query_fn(source$connection, sql)
}

.oracle_fetch_batch <- function(source, registry_row, base_keys, fields) {
  key <- registry_row$observation_key[[1]]
  object <- registry_row$object_name[[1]]
  result <- tryCatch(
    .oracle_query(source, unique(c(key, fields)), object, key),
    error = identity
  )
  if (inherits(result, "error")) {
    if (length(fields) > 1L) {
      midpoint <- floor(length(fields) / 2L)
      left <- .oracle_fetch_batch(source, registry_row, base_keys, fields[seq_len(midpoint)])
      right <- .oracle_fetch_batch(
        source,
        registry_row,
        base_keys,
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
        "warning",
        "extract",
        "field_query_failed",
        conditionMessage(result),
        registry_row$round_id[[1]],
        object,
        fields[[1]]
      )
    ))
  }
  missing_columns <- setdiff(c(key, fields), names(result))
  if (length(missing_columns) > 0L) {
    return(list(
      data = list(),
      issues = .bind_rows(lapply(missing_columns, function(field) {
        .issue(
          "warning", "extract", "query_column_missing",
          paste0("The query result did not contain exact field `", field, "`."),
          registry_row$round_id[[1]], object, field
        )
      }), .empty_issues())
    ))
  }
  result_keys <- as.character(result[[key]])
  if (anyNA(result_keys) || anyDuplicated(result_keys) ||
      length(result_keys) != length(base_keys) ||
      !setequal(result_keys, base_keys)) {
    return(list(
      data = list(),
      issues = .bind_rows(lapply(fields, function(field) {
        .issue(
          "error", "extract", "unsafe_keyed_recovery",
          "The keyed query did not return exactly the validated observation-key set; the field was not combined by row position.",
          registry_row$round_id[[1]], object, field, length(result_keys)
        )
      }), .empty_issues())
    ))
  }
  index <- match(base_keys, result_keys)
  data <- lapply(fields, function(field) result[[field]][index])
  names(data) <- fields
  list(data = data, issues = .empty_issues())
}

.read_oracle_round <- function(source, registry_row, fields) {
  key <- registry_row$observation_key[[1]]
  object <- registry_row$object_name[[1]]
  base <- tryCatch(.oracle_query(source, key, object, key), error = identity)
  if (inherits(base, "error")) {
    return(list(
      data = NULL,
      source_object = object,
      issues = .issue(
        "error", "extract", "round_key_query_failed", conditionMessage(base),
        registry_row$round_id[[1]], object, key
      )
    ))
  }
  if (!(key %in% names(base))) {
    return(list(
      data = NULL,
      source_object = object,
      issues = .issue(
        "error", "extract", "observation_key_missing",
        paste0("The exact observation key `", key, "` was not returned."),
        registry_row$round_id[[1]], object, key
      )
    ))
  }
  base_keys <- as.character(base[[key]])
  if (anyNA(base_keys) || anyDuplicated(base_keys)) {
    return(list(
      data = NULL,
      source_object = object,
      issues = .issue(
        "error", "extract", "observation_key_not_unique",
        "The Oracle observation key contains missing or duplicate values, so safe field assembly is impossible.",
        registry_row$round_id[[1]], object, key, length(base_keys)
      )
    ))
  }
  requested <- setdiff(unique(fields), key)
  batches <- if (length(requested) == 0L) {
    list()
  } else {
    split(requested, ceiling(seq_along(requested) / source$batch_size))
  }
  recovered <- list()
  issues <- list()
  for (batch in batches) {
    batch_result <- .oracle_fetch_batch(source, registry_row, base_keys, batch)
    recovered <- c(recovered, batch_result$data)
    issues[[length(issues) + 1L]] <- batch_result$issues
  }
  data <- if (length(recovered) == 0L) {
    base[key]
  } else {
    data.frame(base[key], recovered, check.names = FALSE)
  }
  list(
    data = data,
    source_object = object,
    issues = .bind_rows(issues, .empty_issues())
  )
}

.read_file_round <- function(source, registry_row, fields) {
  round_id <- registry_row$round_id[[1]]
  entry <- source$rounds[[round_id]]
  if (is.null(entry)) {
    return(list(
      data = NULL,
      source_object = "",
      issues = .issue(
        "warning", "extract", "round_unavailable",
        "No file or in-memory data frame was supplied for this round.",
        round_id
      )
    ))
  }
  data <- if (is.data.frame(entry)) entry else .read_data_file(entry)
  source_object <- if (is.data.frame(entry)) "in_memory" else basename(entry)
  key <- registry_row$observation_key[[1]]
  if (!(key %in% names(data))) {
    return(list(
      data = NULL,
      source_object = source_object,
      issues = .issue(
        "error", "extract", "observation_key_missing",
        paste0("The exact observation key `", key, "` is missing."),
        round_id, source_object, key
      )
    ))
  }
  available <- intersect(unique(c(key, "SUBJECT_ID", fields)), names(data))
  missing <- setdiff(fields, names(data))
  issues <- .bind_rows(lapply(missing, function(field) {
    .issue(
      "warning", "extract", "field_unavailable",
      paste0("The exact requested field `", field, "` is unavailable."),
      round_id, source_object, field
    )
  }), .empty_issues())
  list(
    data = data[available],
    source_object = source_object,
    issues = issues
  )
}

.load_crosswalk <- function(source, registry) {
  object <- registry$crosswalk_object[[1]]
  passcode <- registry$crosswalk_passcode_field[[1]]
  subject <- registry$crosswalk_subject_field[[1]]
  if (inherits(source, "react_synthetic_source")) {
    return(list(data = NULL, issues = .empty_issues()))
  }
  if (inherits(source, "react_file_source")) {
    data <- tryCatch(.read_crosswalk_source(source$crosswalk), error = identity)
    if (inherits(data, "error")) {
      return(list(
        data = NULL,
        issues = .issue("warning", "crosswalk", "crosswalk_read_failed", conditionMessage(data))
      ))
    }
    return(list(data = data, issues = .empty_issues()))
  }
  result <- tryCatch(
    .oracle_query(source, c(subject, passcode), object, passcode),
    error = identity
  )
  if (inherits(result, "error")) {
    return(list(
      data = NULL,
      issues = .issue(
        "warning", "crosswalk", "crosswalk_query_failed",
        conditionMessage(result), source_object = object
      )
    ))
  }
  list(data = result, issues = .empty_issues())
}

.prepare_crosswalk <- function(data, registry) {
  if (is.null(data)) {
    return(list(data = NULL, issues = .empty_issues()))
  }
  passcode <- registry$crosswalk_passcode_field[[1]]
  subject <- registry$crosswalk_subject_field[[1]]
  if (!all(c(passcode, subject) %in% names(data))) {
    return(list(
      data = NULL,
      issues = .issue(
        "warning", "crosswalk", "crosswalk_columns_missing",
        paste0("Crosswalk must contain exact columns `", passcode, "` and `", subject, "`.")
      )
    ))
  }
  compact <- data.frame(
    passcode = as.character(data[[passcode]]),
    subject = as.character(data[[subject]]),
    stringsAsFactors = FALSE
  )
  compact <- compact[!is.na(compact$passcode), , drop = FALSE]
  grouped <- split(compact$subject, compact$passcode)
  subject_values <- vapply(grouped, function(x) {
    unique_values <- unique(x[!is.na(x)])
    if (length(unique_values) == 1L) unique_values else NA_character_
  }, character(1))
  conflict <- vapply(grouped, function(x) length(unique(x[!is.na(x)])) > 1L, logical(1))
  prepared <- data.frame(
    passcode = names(subject_values),
    subject = unname(subject_values),
    stringsAsFactors = FALSE
  )
  issues <- if (any(conflict)) {
    .issue(
      "warning", "crosswalk", "crosswalk_conflict",
      "Conflicting subject IDs were left unmatched; observations were not multiplied.",
      affected_count = sum(conflict)
    )
  } else {
    .empty_issues()
  }
  list(data = prepared, issues = issues)
}

.empty_observations <- function() {
  data.frame(
    observation_id = character(),
    study_id = character(),
    round_id = character(),
    survey_id = character(),
    U_PASSCODE = character(),
    SUBJECT_ID = character(),
    visit_number = integer(),
    total_visits = integer(),
    source_object = character(),
    source_row = integer(),
    display_order = integer(),
    stringsAsFactors = FALSE
  )
}

.make_observations <- function(data, round_row, registry_row, source_object, crosswalk) {
  key <- registry_row$passcode_field[[1]]
  n <- nrow(data)
  passcodes <- as.character(data[[key]])
  subject_ids <- if ("SUBJECT_ID" %in% names(data)) {
    as.character(data$SUBJECT_ID)
  } else {
    rep(NA_character_, n)
  }
  if (!is.null(crosswalk)) {
    matched <- match(passcodes, crosswalk$passcode)
    linked <- crosswalk$subject[matched]
    replace <- is.na(subject_ids) & !is.na(linked)
    subject_ids[replace] <- linked[replace]
  }
  issues <- list()
  missing_key <- is.na(passcodes)
  if (any(missing_key)) {
    issues[[length(issues) + 1L]] <- .issue(
      "warning", "observe", "missing_passcode",
      "Observations with missing passcodes were retained but cannot be linked.",
      round_row$round_id[[1]], source_object, key, sum(missing_key)
    )
  }
  unmatched <- !is.na(passcodes) & is.na(subject_ids)
  if (any(unmatched)) {
    issues[[length(issues) + 1L]] <- .issue(
      "warning", "crosswalk", "subject_id_unmatched",
      "Passcodes without a unique subject link were retained.",
      round_row$round_id[[1]], source_object, key, sum(unmatched)
    )
  }
  observations <- data.frame(
    observation_id = paste0(round_row$round_id[[1]], "::", sprintf("%09d", seq_len(n))),
    study_id = rep(round_row$study_id[[1]], n),
    round_id = rep(round_row$round_id[[1]], n),
    survey_id = rep(round_row$survey_id[[1]], n),
    U_PASSCODE = passcodes,
    SUBJECT_ID = subject_ids,
    visit_number = rep(NA_integer_, n),
    total_visits = rep(NA_integer_, n),
    source_object = rep(source_object, n),
    source_row = seq_len(n),
    display_order = rep(as.integer(round_row$display_order[[1]]), n),
    stringsAsFactors = FALSE
  )
  list(observations = observations, issues = .bind_rows(issues, .empty_issues()))
}

.empty_raw_values <- function() {
  typed <- .typed_value_frame(character())
  data.frame(
    observation_id = character(),
    round_id = character(),
    occurrence_id = character(),
    concept_id = character(),
    taxonomy_id = character(),
    raw_variable = character(),
    source_data_type = character(),
    source_is_missing = logical(),
    typed,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.make_raw_values <- function(data, observations, occurrences) {
  rows <- vector("list", nrow(occurrences))
  used <- 0L
  for (i in seq_len(nrow(occurrences))) {
    variable <- occurrences$variable[[i]]
    if (!(variable %in% names(data))) {
      next
    }
    used <- used + 1L
    value <- data[[variable]]
    typed <- .typed_value_frame(value)
    rows[[used]] <- data.frame(
      observation_id = observations$observation_id,
      round_id = observations$round_id,
      occurrence_id = rep(occurrences$occurrence_id[[i]], nrow(data)),
      concept_id = rep(occurrences$primary_concept_id[[i]], nrow(data)),
      taxonomy_id = rep(occurrences$primary_taxonomy_id[[i]], nrow(data)),
      raw_variable = rep(variable, nrow(data)),
      source_data_type = rep(occurrences$data_type[[i]], nrow(data)),
      source_is_missing = is.na(value),
      typed,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  .bind_rows(rows[seq_len(used)], .empty_raw_values())
}

.assign_visit_order <- function(observations) {
  if (nrow(observations) == 0L) {
    return(observations)
  }
  usable <- which(!is.na(observations$SUBJECT_ID) & nzchar(observations$SUBJECT_ID))
  if (length(usable) == 0L) {
    return(observations)
  }
  ordered <- usable[order(
    observations$SUBJECT_ID[usable],
    observations$display_order[usable],
    observations$source_row[usable],
    method = "radix"
  )]
  subject_runs <- rle(observations$SUBJECT_ID[ordered])$lengths
  observations$visit_number[ordered] <- sequence(subject_runs)
  observations$total_visits[ordered] <- rep.int(subject_runs, subject_runs)
  observations
}

.validate_extraction_result <- function(result) {
  if (anyDuplicated(result$observations$observation_id)) {
    stop("Internal error: observation IDs are not unique.", call. = FALSE)
  }
  if ("raw_values" %in% names(result) &&
      !all(result$raw_values$observation_id %in% result$observations$observation_id)) {
    stop("Internal error: raw values contain an unknown observation ID.", call. = FALSE)
  }
  if (all(c("data", "raw_data") %in% names(result)) &&
      (!identical(result$data$observation_id, result$observations$observation_id) ||
       !identical(result$raw_data$observation_id, result$observations$observation_id))) {
    stop("Internal error: wide output rows do not match observations.", call. = FALSE)
  }
  if ("harmonised_values" %in% names(result)) {
    nonmissing <- .typed_nonmissing_count(result$harmonised_values)
    expected <- result$harmonised_values$missing_reason == ""
    if (any(nonmissing[expected] != 1L) || any(nonmissing[!expected] > 1L)) {
      stop("Internal error: harmonised typed-value invariant failed.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Extract REACT variables by reviewed topic
#'
#' @param source A source created by [react_oracle()], [react_files()], or
#'   [react_synthetic()].
#' @param families `all` or dictionary family selectors returned by
#'   [react_families()].
#' @param rounds `all`, round IDs, or survey IDs.
#' @param concepts Optional exact concept IDs used as an additional filter.
#' @param progress Show round and processing-stage progress. Updates include
#'   requested fields, records received, and elapsed time. Defaults to on in
#'   interactive R sessions and off in scripts.
#' @param output Output shape. `"wide"` (the default) returns researcher-ready
#'   cleaned and raw tables without retaining the large detailed long tables.
#'   `"long"` returns only the detailed typed-value tables, while `"both"`
#'   returns both representations.
#' @return A `react_extract_result` list. In wide output, `data` is the main
#'   cleaned table and `raw_data` retains one column per exact source field.
#'   Concepts with at most one field per round use one concept column; genuine
#'   multi-item concepts use separate `concept__field__EXACT_RAW_FIELD` columns.
#'   `column_dictionary`, `issues`, and `manifest` describe the result. Detailed
#'   `raw_values` and `harmonised_values` are included only for `output = "long"`
#'   or `output = "both"`.
#' @export
react_extract <- function(source, families = "all", rounds = "all", concepts = NULL,
                          progress = interactive(),
                          output = c("wide", "long", "both")) {
  if (!inherits(source, "react_source")) {
    stop("`source` must be created by `react_oracle()`, `react_files()`, or `react_synthetic()`.", call. = FALSE)
  }
  if (!is.logical(progress) || length(progress) != 1L || is.na(progress)) {
    stop("`progress` must be TRUE or FALSE.", call. = FALSE)
  }
  output <- match.arg(output)
  include_wide <- output %in% c("wide", "both")
  include_long <- output %in% c("long", "both")
  total_started <- .extract_clock()
  timings <- numeric()
  round_timings <- numeric()

  stage_started <- .extract_clock()
  .progress_stage_start(progress, "loading dictionary and selecting fields")
  dictionary <- react_dictionary()
  selected <- .select_occurrences(dictionary, families, rounds, concepts)
  requested_rounds <- .resolve_rounds(rounds, dictionary$rounds)
  selected_output_plan <- dictionary$concept_output_columns[
    dictionary$concept_output_columns$occurrence_id %in% selected$occurrence_id,
    ,
    drop = FALSE
  ]
  result_output_plan <- selected_output_plan
  requested_output_columns <- unique(selected_output_plan$output_column)
  generation_occurrences <- if (inherits(source, "react_synthetic_source")) {
    .synthetic_generation_occurrences(dictionary, selected, requested_rounds)
  } else {
    selected
  }
  if (!is.null(concepts)) {
    missing_concepts <- setdiff(concepts, selected_output_plan$concept_id)
    if (length(missing_concepts) > 0L) {
      missing_plan <- dictionary$concept_output_columns[
        dictionary$concept_output_columns$concept_id %in% missing_concepts,
        ,
        drop = FALSE
      ]
      result_output_plan <- rbind(result_output_plan, missing_plan)
      requested_output_columns <- unique(c(
        requested_output_columns,
        missing_plan$output_column
      ))
    }
  }
  registry <- if (inherits(source, "react_oracle_source")) {
    source$registry
  } else {
    .validate_registry(dictionary$source_registry)
  }
  timings[["loading_dictionary_and_selecting_fields"]] <-
    .elapsed_seconds(stage_started)
  .progress_stage_done(
    progress, "loading dictionary and selecting fields",
    timings[["loading_dictionary_and_selecting_fields"]], total_started
  )

  stage_started <- .extract_clock()
  .progress_stage_start(progress, "loading subject links")
  crosswalk_result <- .load_crosswalk(source, registry)
  prepared_crosswalk <- .prepare_crosswalk(crosswalk_result$data, registry)
  timings[["loading_subject_links"]] <- .elapsed_seconds(stage_started)
  .progress_stage_done(
    progress, "loading subject links",
    timings[["loading_subject_links"]], total_started
  )
  issue_parts <- list(crosswalk_result$issues, prepared_crosswalk$issues)
  observation_parts <- list()
  raw_parts <- list()
  harmonised_parts <- list()
  data_parts <- list()
  raw_data_parts <- list()
  raw_value_count <- 0L
  harmonised_value_count <- 0L
  harmonising_seconds <- 0
  cleaned_table_seconds <- 0
  raw_table_seconds <- 0
  selected_mappings <- dictionary$mappings[
    dictionary$mappings$occurrence_id %in% selected$occurrence_id,
    ,
    drop = FALSE
  ]
  .progress_stage_start(progress, "harmonising")

  for (round_index in seq_along(requested_rounds)) {
    round_id <- requested_rounds[[round_index]]
    round_row <- dictionary$rounds[dictionary$rounds$round_id == round_id, , drop = FALSE]
    registry_row <- registry[registry$round_id == round_id, , drop = FALSE]
    round_occurrences <- selected[selected$round_id == round_id, , drop = FALSE]
    generation_round_occurrences <- generation_occurrences[
      generation_occurrences$round_id == round_id, , drop = FALSE
    ]
    fields <- unique(generation_round_occurrences$variable)
    round_started <- .extract_clock()
    .progress_message(
      progress,
      "Round ", round_index, "/", length(requested_rounds), " ", round_id,
      " | requesting ", .format_records(length(fields)), " fields"
    )
    fetched <- if (inherits(source, "react_oracle_source")) {
      .read_oracle_round(source, registry_row, fields)
    } else if (inherits(source, "react_synthetic_source")) {
      .read_synthetic_round(source, registry_row, generation_round_occurrences, dictionary)
    } else {
      .read_file_round(source, registry_row, fields)
    }
    issue_parts[[length(issue_parts) + 1L]] <- fetched$issues
    if (is.null(fetched$data)) {
      round_timings[[gsub("[.]", "_", round_id)]] <- .elapsed_seconds(round_started)
      .progress_message(
        progress,
        "Round ", round_index, "/", length(requested_rounds), " ", round_id,
        " | no records received",
        " | round ", .format_seconds(round_timings[[gsub("[.]", "_", round_id)]]),
        " | elapsed ", .format_seconds(.elapsed_seconds(total_started))
      )
      next
    }
    observed <- .make_observations(
      fetched$data,
      round_row,
      registry_row,
      fetched$source_object,
      prepared_crosswalk$data
    )
    issue_parts[[length(issue_parts) + 1L]] <- observed$issues
    observation_parts[[length(observation_parts) + 1L]] <- observed$observations
    round_raw_values <- .make_raw_values(
      fetched$data,
      observed$observations,
      round_occurrences
    )
    round_output_plan <- selected_output_plan[
      selected_output_plan$round_id == round_id,
      ,
      drop = FALSE
    ]
    round_mappings <- selected_mappings[
      selected_mappings$occurrence_id %in% round_output_plan$occurrence_id,
      ,
      drop = FALSE
    ]
    harmonising_started <- .extract_clock()
    round_harmonised <- .harmonise_values(
      round_raw_values,
      dictionary,
      mappings = round_mappings,
      output_plan = round_output_plan
    )
    harmonising_seconds <- harmonising_seconds +
      .elapsed_seconds(harmonising_started)
    issue_parts[[length(issue_parts) + 1L]] <- round_harmonised$issues
    raw_value_count <- raw_value_count + nrow(round_raw_values)
    harmonised_value_count <- harmonised_value_count + nrow(round_harmonised$data)

    if (include_wide) {
      cleaned_started <- .extract_clock()
      data_parts[[length(data_parts) + 1L]] <- .make_simple_harmonised_data(
        observed$observations,
        round_harmonised$data
      )
      cleaned_table_seconds <- cleaned_table_seconds +
        .elapsed_seconds(cleaned_started)

      raw_started <- .extract_clock()
      raw_data_parts[[length(raw_data_parts) + 1L]] <- .make_simple_raw_data(
        observed$observations,
        round_raw_values
      )
      raw_table_seconds <- raw_table_seconds + .elapsed_seconds(raw_started)
    }
    if (include_long) {
      raw_parts[[length(raw_parts) + 1L]] <- round_raw_values
      harmonised_parts[[length(harmonised_parts) + 1L]] <- round_harmonised$data
    }
    round_key <- gsub("[.]", "_", round_id)
    round_timings[[round_key]] <- .elapsed_seconds(round_started)
    .progress_message(
      progress,
      "Round ", round_index, "/", length(requested_rounds), " ", round_id,
      " | received ", .format_records(nrow(fetched$data)), " records",
      " | round ", .format_seconds(round_timings[[round_key]]),
      " | elapsed ", .format_seconds(.elapsed_seconds(total_started))
    )
  }

  timings[["extracting_rounds"]] <- sum(round_timings)
  timings[["harmonising"]] <- harmonising_seconds
  .progress_stage_done(
    progress, "harmonising", timings[["harmonising"]], total_started
  )

  stage_started <- .extract_clock()
  .progress_stage_start(progress, "combining rounds")
  observations <- .bind_rows(observation_parts, .empty_observations())
  if (include_long) {
    raw_values <- .bind_rows(raw_parts, .empty_raw_values())
    harmonised_values <- .bind_rows(
      harmonised_parts,
      .empty_harmonised_values()
    )
  }
  timings[["combining_rounds"]] <- .elapsed_seconds(stage_started)
  .progress_stage_done(
    progress, "combining rounds", timings[["combining_rounds"]], total_started
  )

  stage_started <- .extract_clock()
  .progress_stage_start(progress, "assigning visits")
  observations <- .assign_visit_order(observations)
  timings[["assigning_visits"]] <- .elapsed_seconds(stage_started)
  .progress_stage_done(
    progress, "assigning visits", timings[["assigning_visits"]], total_started
  )

  if (include_wide) {
    stage_started <- .extract_clock()
    .progress_stage_start(progress, "creating cleaned table")
    data <- .bind_simple_parts(
      data_parts,
      column_order = c(
        names(.simple_identifiers(observations)),
        requested_output_columns
      )
    )
    data <- .sync_simple_identifiers(data, observations)
    timings[["creating_cleaned_table"]] <- cleaned_table_seconds +
      .elapsed_seconds(stage_started)
    .progress_stage_done(
      progress, "creating cleaned table",
      timings[["creating_cleaned_table"]], total_started
    )

    stage_started <- .extract_clock()
    .progress_stage_start(progress, "creating raw table")
    raw_data <- .bind_simple_parts(
      raw_data_parts,
      column_order = names(.simple_identifiers(observations))
    )
    raw_data <- .sync_simple_identifiers(raw_data, observations)
    timings[["creating_raw_table"]] <- raw_table_seconds +
      .elapsed_seconds(stage_started)
    .progress_stage_done(
      progress, "creating raw table", timings[["creating_raw_table"]], total_started
    )
  } else {
    timings[["creating_cleaned_table"]] <- 0
    timings[["creating_raw_table"]] <- 0
  }
  issues <- .bind_rows(issue_parts, .empty_issues())
  dictionary_version <- react_dictionary_version()
  completeness <- if (nrow(issues) == 0L) "complete" else "partial"
  manifest <- data.frame(
    key = c(
      "package_version", "dictionary_release", "dictionary_manifest_sha256",
      "source_type", "requested_rounds", "requested_families",
      "output_mode", "long_tables_included",
      "observation_count", "data_column_count", "raw_data_column_count",
      "concept_output_column_count", "raw_value_count", "harmonised_value_count",
      "issue_count", "completeness"
    ),
    value = c(
      as.character(utils::packageVersion("reactextract")),
      dictionary_version$dictionary_release,
      dictionary_version$manifest_sha256,
      source$kind,
      paste(requested_rounds, collapse = "|"),
      paste(families, collapse = "|"),
      output,
      if (include_long) "true" else "false",
      as.character(nrow(observations)),
      if (include_wide) as.character(ncol(data)) else "0",
      if (include_wide) as.character(ncol(raw_data)) else "0",
      as.character(length(requested_output_columns)),
      as.character(raw_value_count),
      as.character(harmonised_value_count),
      as.character(nrow(issues)),
      completeness
    ),
    stringsAsFactors = FALSE
  )
  if (inherits(source, "react_synthetic_source")) {
    synthetic_manifest <- data.frame(
      key = c(
        "synthetic_seed", "synthetic_profile_version", "synthetic_profile_sha256",
        "synthetic_profile_status", "synthetic_requested_counts",
        "synthetic_safe_prior_fraction", "synthetic_subject_model"
      ),
      value = c(
        as.character(source$seed),
        unname(source$profile_metadata[["profile_version"]]),
        source$profile_sha256,
        unname(source$profile_metadata[["status"]]),
        paste(paste(names(source$n_per_round), source$n_per_round, sep = "="), collapse = "|"),
        as.character(source$safe_prior_fraction),
        "independent_subjects_visit_1"
      ),
      stringsAsFactors = FALSE
    )
    manifest <- rbind(manifest, synthetic_manifest)
  }
  result <- list()
  if (include_wide) {
    result$data <- data
    result$raw_data <- raw_data
  }
  result$observations <- observations
  if (include_long) {
    result$raw_values <- raw_values
    result$harmonised_values <- harmonised_values
  }
  result$column_dictionary <- result_output_plan
  result$issues <- issues
  result$manifest <- manifest

  stage_started <- .extract_clock()
  .progress_stage_start(progress, "validation")
  .validate_extraction_result(result)
  timings[["validation"]] <- .elapsed_seconds(stage_started)
  timings <- c(timings, stats::setNames(round_timings, paste0("round_", names(round_timings))))
  timings[["total"]] <- .elapsed_seconds(total_started)
  result$manifest <- rbind(result$manifest, .timing_manifest_rows(timings))
  .progress_stage_done(
    progress, "validation", timings[["validation"]], total_started
  )
  class(result) <- c("react_extract_result", "list")
  result
}

#' Print an extraction result
#'
#' @param x A result returned by [react_extract()].
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.react_extract_result <- function(x, ...) {
  metadata <- stats::setNames(x$manifest$value, x$manifest$key)
  value <- function(key, fallback = "0") {
    out <- unname(metadata[[key]])
    if (is.null(out) || is.na(out) || !nzchar(out)) fallback else out
  }
  number <- function(key) {
    prettyNum(as.numeric(value(key)), big.mark = ",", scientific = FALSE)
  }
  cat("<reactextract result>\n")
  cat("  Output: ", value("output_mode", "both"), "\n", sep = "")
  cat("  Observations: ", number("observation_count"), "\n", sep = "")
  if (value("output_mode", "both") %in% c("wide", "both")) {
    cat("  Harmonised table: ", number("data_column_count"), " columns\n", sep = "")
    cat("  Raw table: ", number("raw_data_column_count"), " columns\n", sep = "")
  }
  if (value("output_mode", "both") %in% c("long", "both")) {
    cat("  Raw long values: ", number("raw_value_count"), "\n", sep = "")
    cat("  Harmonised long values: ", number("harmonised_value_count"), "\n", sep = "")
  }
  cat("  Issues: ", number("issue_count"), "\n", sep = "")
  cat("  Approximate size: ", format(utils::object.size(x), units = "auto"), "\n", sep = "")
  invisible(x)
}
