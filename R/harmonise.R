.empty_harmonised_values <- function() {
  typed <- .typed_value_frame(character())
  data.frame(
    observation_id = character(),
    round_id = character(),
    concept_id = character(),
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

.harmonise_values <- function(raw_values, dictionary) {
  if (nrow(raw_values) == 0L || nrow(dictionary$mappings) == 0L) {
    return(list(data = .empty_harmonised_values(), issues = .empty_issues()))
  }
  mappings <- dictionary$mappings[
    dictionary$mappings$review_state == "approved" &
      dictionary$mappings$collection_status == "asked",
    ,
    drop = FALSE
  ]
  transforms <- dictionary$transforms
  rows <- list()
  issues <- list()
  used <- 0L
  for (i in seq_len(nrow(mappings))) {
    raw_index <- which(raw_values$occurrence_id == mappings$occurrence_id[[i]])
    if (length(raw_index) == 0L) {
      next
    }
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
  list(
    data = .bind_rows(rows[seq_len(used)], .empty_harmonised_values()),
    issues = .bind_rows(issues, .empty_issues())
  )
}
