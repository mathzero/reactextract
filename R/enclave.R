.count_value <- function(data, name) {
  index <- match(toupper(name), toupper(names(data)))
  if (is.na(index) || nrow(data) != 1L) {
    stop("The Oracle aggregate query did not return `", name, "`.", call. = FALSE)
  }
  as.numeric(data[[index]][[1]])
}

.oracle_schema_query <- function(source, object, fields) {
  sql <- paste0(
    "SELECT ",
    paste(vapply(fields, .quoted_field, character(1)), collapse = ", "),
    " FROM ", object, " WHERE 1 = 0"
  )
  source$query_fn(source$connection, sql)
}

.oracle_field_preflight <- function(source, object, round_id, fields) {
  result <- tryCatch(.oracle_schema_query(source, object, fields), error = identity)
  if (!inherits(result, "error")) {
    return(data.frame(
      round_id = rep(round_id, length(fields)),
      source_object = rep(object, length(fields)),
      variable = fields,
      status = rep("available", length(fields)),
      message = rep("", length(fields)),
      stringsAsFactors = FALSE
    ))
  }
  if (length(fields) > 1L) {
    midpoint <- floor(length(fields) / 2L)
    return(.bind_rows(list(
      .oracle_field_preflight(source, object, round_id, fields[seq_len(midpoint)]),
      .oracle_field_preflight(
        source, object, round_id, fields[seq.int(midpoint + 1L, length(fields))]
      )
    )))
  }
  data.frame(
    round_id = round_id,
    source_object = object,
    variable = fields,
    status = "unavailable",
    message = conditionMessage(result),
    stringsAsFactors = FALSE
  )
}

.oracle_round_counts <- function(source, object, key) {
  sql <- paste0(
    "SELECT COUNT(*) AS N_ROWS, COUNT(", .quoted_field(key),
    ") AS N_NONMISSING_KEYS, COUNT(DISTINCT ", .quoted_field(key),
    ") AS N_DISTINCT_KEYS FROM ", object
  )
  source$query_fn(source$connection, sql)
}

.oracle_crosswalk_counts <- function(source, object, passcode, subject) {
  sql <- paste0(
    "SELECT COUNT(*) AS N_ROWS, COUNT(", .quoted_field(passcode),
    ") AS N_NONMISSING_PASSCODES, COUNT(DISTINCT ", .quoted_field(passcode),
    ") AS N_DISTINCT_PASSCODES, COUNT(", .quoted_field(subject),
    ") AS N_NONMISSING_SUBJECTS FROM ", object
  )
  source$query_fn(source$connection, sql)
}

.oracle_crosswalk_conflicts <- function(source, object, passcode, subject) {
  sql <- paste0(
    "SELECT COUNT(*) AS N_CONFLICTING_PASSCODES FROM (SELECT ",
    .quoted_field(passcode), " FROM ", object, " WHERE ",
    .quoted_field(passcode), " IS NOT NULL GROUP BY ", .quoted_field(passcode),
    " HAVING COUNT(DISTINCT ", .quoted_field(subject), ") > 1)"
  )
  source$query_fn(source$connection, sql)
}

.empty_preflight_fields <- function() {
  data.frame(
    round_id = character(),
    source_object = character(),
    variable = character(),
    status = character(),
    message = character(),
    stringsAsFactors = FALSE
  )
}

.read_confirmed_unavailable <- function() {
  path <- system.file(
    "extdata", "oracle_unavailable_fields.csv",
    package = "reactextract"
  )
  if (!nzchar(path)) {
    stop("The confirmed-unavailable field registry is missing.", call. = FALSE)
  }
  registry <- .read_literal_csv(path)
  required <- c(
    "round_id", "source_object", "variable", "confirmed_by",
    "confirmed_date", "reason"
  )
  if (!identical(names(registry), required) || anyDuplicated(
      registry[c("round_id", "source_object", "variable")]
    )) {
    stop("The confirmed-unavailable field registry is invalid.", call. = FALSE)
  }
  registry
}

.apply_confirmed_unavailable <- function(field_status, registry) {
  if (nrow(field_status) == 0L || nrow(registry) == 0L) {
    return(field_status)
  }
  field_key <- paste(
    field_status$round_id, field_status$source_object, field_status$variable,
    sep = "\r"
  )
  registry_key <- paste(
    registry$round_id, registry$source_object, registry$variable,
    sep = "\r"
  )
  matched <- match(field_key, registry_key)
  confirmed <- field_status$status == "unavailable" & !is.na(matched)
  field_status$status[confirmed] <- "confirmed_unavailable"
  field_status$message[confirmed] <- registry$reason[matched[confirmed]]
  field_status
}

.preflight_round <- function(source, registry_row, fields, confirmed_unavailable) {
  round_id <- registry_row$round_id[[1]]
  object <- registry_row$object_name[[1]]
  key <- registry_row$observation_key[[1]]
  access <- tryCatch(
    .oracle_schema_query(source, object, key),
    error = identity
  )
  if (inherits(access, "error")) {
    message <- conditionMessage(access)
    field_status <- data.frame(
      round_id = rep(round_id, length(fields)),
      source_object = rep(object, length(fields)),
      variable = fields,
      status = rep("unavailable", length(fields)),
      message = rep(message, length(fields)),
      stringsAsFactors = FALSE
    )
    round <- data.frame(
      round_id = round_id,
      source_object = object,
      observation_key = key,
      expected_field_count = length(fields),
      available_field_count = 0L,
      unavailable_field_count = length(fields),
      confirmed_unavailable_field_count = 0L,
      row_count = NA_real_,
      nonmissing_key_count = NA_real_,
      distinct_key_count = NA_real_,
      status = "failed",
      message = message,
      stringsAsFactors = FALSE
    )
    issue <- .issue(
      "error", "preflight", "view_or_key_unavailable", message,
      round_id, object, key, length(fields)
    )
    return(list(round = round, fields = field_status, issues = issue))
  }
  batches <- split(fields, ceiling(seq_along(fields) / source$batch_size))
  field_status <- .bind_rows(
    lapply(batches, function(batch) {
      .oracle_field_preflight(source, object, round_id, batch)
    }),
    .empty_preflight_fields()
  )
  field_status <- .apply_confirmed_unavailable(
    field_status,
    confirmed_unavailable
  )
  count_result <- tryCatch(
    .oracle_round_counts(source, object, key),
    error = identity
  )
  if (inherits(count_result, "error")) {
    counts <- rep(NA_real_, 3L)
    count_message <- conditionMessage(count_result)
  } else {
    counts <- tryCatch(
      c(
        .count_value(count_result, "N_ROWS"),
        .count_value(count_result, "N_NONMISSING_KEYS"),
        .count_value(count_result, "N_DISTINCT_KEYS")
      ),
      error = identity
    )
    if (inherits(counts, "error")) {
      count_message <- conditionMessage(counts)
      counts <- rep(NA_real_, 3L)
    } else {
      count_message <- ""
    }
  }
  unavailable <- sum(field_status$status == "unavailable")
  confirmed <- sum(field_status$status == "confirmed_unavailable")
  key_valid <- !anyNA(counts) && counts[[1]] == counts[[2]] && counts[[2]] == counts[[3]]
  status <- if (unavailable > 0L || nzchar(count_message) || !key_valid) {
    "failed"
  } else if (confirmed > 0L) {
    "passed_with_notes"
  } else {
    "passed"
  }
  round <- data.frame(
    round_id = round_id,
    source_object = object,
    observation_key = key,
    expected_field_count = length(fields),
    available_field_count = sum(field_status$status == "available"),
    unavailable_field_count = unavailable,
    confirmed_unavailable_field_count = confirmed,
    row_count = counts[[1]],
    nonmissing_key_count = counts[[2]],
    distinct_key_count = counts[[3]],
    status = status,
    message = count_message,
    stringsAsFactors = FALSE
  )
  issues <- list()
  if (unavailable > 0L) {
    issues[[length(issues) + 1L]] <- .issue(
      "error", "preflight", "fields_unavailable",
      "One or more exact dictionary fields are unavailable from the view.",
      round_id, object, affected_count = unavailable
    )
  }
  if (confirmed > 0L) {
    issues[[length(issues) + 1L]] <- .issue(
      "information", "preflight", "fields_confirmed_unavailable",
      "Human review confirmed that these dictionary fields are absent from the enclave view.",
      round_id, object, affected_count = confirmed
    )
  }
  if (nzchar(count_message)) {
    issues[[length(issues) + 1L]] <- .issue(
      "error", "preflight", "round_count_failed", count_message,
      round_id, object, key
    )
  } else if (!key_valid) {
    issues[[length(issues) + 1L]] <- .issue(
      "error", "preflight", "observation_key_not_unique",
      "The observation key has missing or duplicate values.",
      round_id, object, key,
      if (anyNA(counts)) NA_integer_ else counts[[1]] - counts[[3]]
    )
  }
  list(round = round, fields = field_status, issues = .bind_rows(issues, .empty_issues()))
}

.preflight_crosswalk <- function(source, registry) {
  object <- registry$crosswalk_object[[1]]
  passcode <- registry$crosswalk_passcode_field[[1]]
  subject <- registry$crosswalk_subject_field[[1]]
  schema <- tryCatch(
    .oracle_schema_query(source, object, c(passcode, subject)),
    error = identity
  )
  counts <- tryCatch(
    .oracle_crosswalk_counts(source, object, passcode, subject),
    error = identity
  )
  conflicts <- tryCatch(
    .oracle_crosswalk_conflicts(source, object, passcode, subject),
    error = identity
  )
  errors <- Filter(
    function(value) inherits(value, "error"),
    list(schema, counts, conflicts)
  )
  if (length(errors) > 0L) {
    message <- paste(vapply(errors, conditionMessage, character(1)), collapse = " | ")
    row <- data.frame(
      source_object = object,
      row_count = NA_real_,
      nonmissing_passcode_count = NA_real_,
      distinct_passcode_count = NA_real_,
      nonmissing_subject_count = NA_real_,
      conflicting_passcode_count = NA_real_,
      status = "failed",
      message = message,
      stringsAsFactors = FALSE
    )
    issue <- .issue(
      "error", "preflight", "crosswalk_preflight_failed", message,
      source_object = object
    )
    return(list(crosswalk = row, issues = issue))
  }
  values <- tryCatch(
    c(
      .count_value(counts, "N_ROWS"),
      .count_value(counts, "N_NONMISSING_PASSCODES"),
      .count_value(counts, "N_DISTINCT_PASSCODES"),
      .count_value(counts, "N_NONMISSING_SUBJECTS"),
      .count_value(conflicts, "N_CONFLICTING_PASSCODES")
    ),
    error = identity
  )
  if (inherits(values, "error")) {
    row <- data.frame(
      source_object = object,
      row_count = NA_real_,
      nonmissing_passcode_count = NA_real_,
      distinct_passcode_count = NA_real_,
      nonmissing_subject_count = NA_real_,
      conflicting_passcode_count = NA_real_,
      status = "failed",
      message = conditionMessage(values),
      stringsAsFactors = FALSE
    )
    return(list(
      crosswalk = row,
      issues = .issue(
        "error", "preflight", "crosswalk_count_invalid",
        conditionMessage(values), source_object = object
      )
    ))
  }
  status <- if (values[[5]] > 0) "warning" else "passed"
  row <- data.frame(
    source_object = object,
    row_count = values[[1]],
    nonmissing_passcode_count = values[[2]],
    distinct_passcode_count = values[[3]],
    nonmissing_subject_count = values[[4]],
    conflicting_passcode_count = values[[5]],
    status = status,
    message = if (status == "warning") {
      "Conflicting passcode-to-subject links will be retained as unmatched."
    } else "",
    stringsAsFactors = FALSE
  )
  issue <- if (status == "warning") {
    .issue(
      "warning", "preflight", "crosswalk_conflict",
      row$message, source_object = object, affected_count = values[[5]]
    )
  } else .empty_issues()
  list(crosswalk = row, issues = issue)
}

#' Run a read-only Oracle enclave preflight
#'
#' Checks all requested view names, exact dictionary fields, observation-key
#' uniqueness, and the subject crosswalk. Field checks retrieve zero rows; only
#' aggregate counts are retrieved for keys. No respondent values are returned.
#'
#' @param source A source created by [react_oracle()].
#' @param families `all` or dictionary family selectors returned by
#'   [react_families()].
#' @param rounds `all`, round IDs, or survey IDs.
#' @return A named list of aggregate and metadata-only validation tables.
#' @export
react_validate_oracle <- function(source, families = "all", rounds = "all") {
  if (!inherits(source, "react_oracle_source")) {
    stop("`source` must be created by `react_oracle()`.", call. = FALSE)
  }
  dictionary <- react_dictionary()
  selected <- .select_occurrences(dictionary, families, rounds)
  requested_rounds <- .resolve_rounds(rounds, dictionary$rounds)
  confirmed_unavailable <- .read_confirmed_unavailable()
  parts <- lapply(requested_rounds, function(round_id) {
    registry_row <- source$registry[source$registry$round_id == round_id, , drop = FALSE]
    fields <- unique(selected$variable[selected$round_id == round_id])
    .preflight_round(source, registry_row, fields, confirmed_unavailable)
  })
  crosswalk <- .preflight_crosswalk(source, source$registry)
  round_table <- .bind_rows(lapply(parts, `[[`, "round"))
  field_table <- .bind_rows(lapply(parts, `[[`, "fields"), .empty_preflight_fields())
  issues <- .bind_rows(
    c(lapply(parts, `[[`, "issues"), list(crosswalk$issues)),
    .empty_issues()
  )
  version <- react_dictionary_version()
  passed <- all(round_table$status %in% c("passed", "passed_with_notes")) &&
    crosswalk$crosswalk$status == "passed"
  has_notes <- any(round_table$status == "passed_with_notes")
  manifest <- data.frame(
    key = c(
      "package_version", "dictionary_release", "dictionary_manifest_sha256",
      "requested_rounds", "requested_families", "checked_round_count",
      "checked_field_count", "issue_count", "status"
    ),
    value = c(
      as.character(utils::packageVersion("reactextract")),
      version$dictionary_release,
      version$manifest_sha256,
      paste(requested_rounds, collapse = "|"),
      paste(families, collapse = "|"),
      nrow(round_table), nrow(field_table), nrow(issues),
      if (!passed) "attention_required" else if (has_notes) "passed_with_notes" else "passed"
    ),
    stringsAsFactors = FALSE
  )
  list(
    rounds = round_table,
    fields = field_table,
    crosswalk = crosswalk$crosswalk,
    issues = issues,
    manifest = manifest
  )
}

#' Write a metadata-only enclave preflight report
#'
#' @param validation A result returned by [react_validate_oracle()].
#' @param path Output directory inside the enclave.
#' @return The normalised output path, invisibly.
#' @export
react_write_enclave_report <- function(validation, path) {
  expected <- c("rounds", "fields", "crosswalk", "issues", "manifest")
  if (!is.list(validation) || !all(expected %in% names(validation)) ||
      !all(vapply(validation[expected], is.data.frame, logical(1)))) {
    stop("`validation` must be returned by `react_validate_oracle()`.", call. = FALSE)
  }
  path <- .single_string(path, "path")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  for (name in expected) {
    utils::write.csv(
      validation[[name]], file.path(path, paste0(name, ".csv")),
      row.names = FALSE, na = ""
    )
  }
  invisible(normalizePath(path, mustWork = TRUE))
}
