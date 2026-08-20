main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1L || length(args) > 2L) {
    stop(
      paste0(
        "Usage: Rscript check_long_covid_durations.R CONNECTION_SCRIPT ",
        "[OUTPUT_DIRECTORY]"
      ),
      call. = FALSE
    )
  }
  connection_script <- normalizePath(args[[1]], mustWork = TRUE)
  output <- if (length(args) == 2L) {
    args[[2]]
  } else {
    "reactextract-long-covid-duration-check"
  }
  dir.create(output, recursive = TRUE, showWarnings = FALSE)

  required <- c("reactextract", "DBI")
  missing_packages <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_packages)) {
    stop(
      "Install the required package(s): ", paste(missing_packages, collapse = ", "),
      call. = FALSE
    )
  }

  connection_environment <- new.env(parent = globalenv())
  sys.source(connection_script, envir = connection_environment)
  if (!exists("con", envir = connection_environment, inherits = FALSE)) {
    stop("The connection script must create an object named `con`.", call. = FALSE)
  }
  con <- get("con", envir = connection_environment, inherits = FALSE)
  if (!inherits(con, "DBIConnection")) {
    stop("The connection script did not create a DBI connection.", call. = FALSE)
  }
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

  dictionary <- reactextract::react_dictionary()
  occurrences <- dictionary$occurrences
  registry <- dictionary$source_registry
  duration_occurrences <- unique(occurrences[
    grepl("^LONGCOVIDB2_[0-9]+$|^LONGCOVIDB_[12]_[0-9]+$", occurrences$variable),
    c("round_id", "variable", "primary_concept_id", "label")
  ])
  relevant_rounds <- dictionary$rounds$round_id[
    dictionary$rounds$round_id %in% unique(duration_occurrences$round_id)
  ]

  field_counts <- list()
  pair_summaries <- list()
  both_value_counts <- list()
  binning_comparisons <- list()
  categorical_summaries <- list()
  pull_summaries <- list()

  for (round_id in relevant_rounds) {
    round_occurrences <- duration_occurrences[
      duration_occurrences$round_id == round_id,
      ,
      drop = FALSE
    ]
    fields <- unique(round_occurrences$variable)
    registry_row <- registry[registry$round_id == round_id, , drop = FALSE]
    if (nrow(registry_row) != 1L) {
      stop("No unique source view is configured for ", round_id, ".", call. = FALSE)
    }
    object <- registry_row$object_name[[1]]
    validate_identifier(object, "source view")
    invisible(lapply(fields, validate_identifier, label = "field"))

    message(
      "Checking ", round_id, " (", length(fields), " duration fields from ",
      object, ")..."
    )
    total_query <- paste0("SELECT COUNT(*) AS N_ROWS FROM ", object)
    total_result <- DBI::dbGetQuery(con, total_query)
    total_rows <- single_count(total_result, "N_ROWS")

    sql <- paste0(
      "SELECT ",
      paste(quote_identifier(fields), collapse = ", "),
      " FROM ", object,
      " WHERE ",
      paste0(quote_identifier(fields), " IS NOT NULL", collapse = " OR ")
    )
    started <- proc.time()[["elapsed"]]
    data <- DBI::dbGetQuery(con, sql)
    elapsed <- proc.time()[["elapsed"]] - started
    names(data) <- toupper(names(data))
    message(
      "  received ", format(nrow(data), big.mark = ","),
      " rows containing at least one duration value in ",
      sprintf("%.1f", elapsed), " seconds"
    )
    pull_summaries[[length(pull_summaries) + 1L]] <- data.frame(
      round_id = round_id,
      source_view = object,
      requested_field_count = length(fields),
      source_row_count = total_rows,
      rows_with_any_duration_value = nrow(data),
      elapsed_seconds = round(elapsed, 3),
      stringsAsFactors = FALSE
    )

    for (field in fields) {
      values <- get_column(data, field)
      field_occurrence <- round_occurrences[
        round_occurrences$variable == field,
        ,
        drop = FALSE
      ]
      counts <- value_counts(values, total_rows)
      counts$round_id <- round_id
      counts$source_view <- object
      counts$raw_field <- field
      counts$concept_id <- paste(unique(field_occurrence$primary_concept_id), collapse = " | ")
      counts$question_text <- paste(unique(field_occurrence$label), collapse = " | ")
      counts$field_format <- field_format(field)
      field_counts[[length(field_counts) + 1L]] <- counts[c(
        "round_id", "source_view", "raw_field", "concept_id", "question_text",
        "field_format", "raw_value", "count"
      )]
    }

    continuous <- round_occurrences[
      grepl("^LONGCOVIDB_[12]_[0-9]+$", round_occurrences$variable),
      ,
      drop = FALSE
    ]
    if (nrow(continuous)) {
      continuous$unit <- ifelse(
        grepl("^LONGCOVIDB_1_", continuous$variable), "days", "weeks"
      )
      continuous$item_number <- sub("^LONGCOVIDB_[12]_", "", continuous$variable)
      pair_keys <- unique(continuous$item_number)
      for (item_number in pair_keys) {
        item <- continuous[continuous$item_number == item_number, , drop = FALSE]
        day_fields <- unique(item$variable[item$unit == "days"])
        week_fields <- unique(item$variable[item$unit == "weeks"])
        if (length(day_fields) != 1L || length(week_fields) != 1L) {
          stop(
            "Could not identify one DAYS/WEEKS pair for ", round_id,
            " item ", item_number, ".",
            call. = FALSE
          )
        }
        day_field <- day_fields[[1]]
        week_field <- week_fields[[1]]
        days <- numeric_values(get_column(data, day_field))
        weeks <- numeric_values(get_column(data, week_field))
        concept_id <- paste(unique(item$primary_concept_id), collapse = " | ")
        question_text <- paste(unique(item$label), collapse = " | ")
        target_fields <- unique(duration_occurrences$variable[
          duration_occurrences$primary_concept_id %in% unique(item$primary_concept_id) &
            grepl("^LONGCOVIDB2_[0-9]+$", duration_occurrences$variable) &
            grepl("^react1\\.r1[0-9]$", duration_occurrences$round_id)
        ])

        pair_summaries[[length(pair_summaries) + 1L]] <- pair_summary(
          round_id, object, total_rows, item_number, concept_id, question_text,
          day_field, week_field, target_fields, days, weeks
        )
        both_value_counts[[length(both_value_counts) + 1L]] <- paired_value_counts(
          round_id, item_number, concept_id, day_field, week_field, days, weeks
        )
        binning_comparisons[[length(binning_comparisons) + 1L]] <- compare_binning(
          round_id, item_number, concept_id, day_field, week_field,
          target_fields, days, weeks
        )
      }
    }

    categorical <- round_occurrences[
      grepl("^LONGCOVIDB2_[0-9]+$", round_occurrences$variable),
      ,
      drop = FALSE
    ]
    for (field in unique(categorical$variable)) {
      values <- numeric_values(get_column(data, field))
      observed <- !is.na(values)
      categorical_summaries[[length(categorical_summaries) + 1L]] <- data.frame(
        round_id = round_id,
        source_view = object,
        raw_field = field,
        concept_id = paste(
          unique(categorical$primary_concept_id[categorical$variable == field]),
          collapse = " | "
        ),
        nonmissing_count = sum(observed),
        expected_code_1_to_7_count = sum(values %in% 1:7, na.rm = TRUE),
        administrative_negative_code_count = sum(values < 0, na.rm = TRUE),
        zero_count = sum(values == 0, na.rm = TRUE),
        unexpected_positive_code_count = sum(values > 7, na.rm = TRUE),
        noninteger_count = sum(observed & values != trunc(values)),
        minimum_value = safe_min(values[observed]),
        maximum_value = safe_max(values[observed]),
        coding_assessment = assess_categorical_codes(values),
        stringsAsFactors = FALSE
      )
    }
    rm(data)
    invisible(gc())
  }

  outputs <- list(
    "pull-summary.csv" = bind_rows(pull_summaries),
    "field-value-counts.csv" = bind_rows(field_counts),
    "day-week-completion-summary.csv" = bind_rows(pair_summaries),
    "day-week-both-value-counts.csv" = bind_rows(both_value_counts),
    "candidate-binning-comparison.csv" = bind_rows(binning_comparisons),
    "categorical-code-summary.csv" = bind_rows(categorical_summaries)
  )
  for (filename in names(outputs)) {
    utils::write.csv(
      outputs[[filename]], file.path(output, filename),
      row.names = FALSE, na = ""
    )
  }
  version <- reactextract::react_dictionary_version()
  manifest <- data.frame(
    key = c(
      "created_at", "reactextract_version", "dictionary_release",
      "dictionary_manifest_sha256", "rounds_checked", "duration_fields_checked",
      "day_week_pairs_checked", "categorical_fields_checked",
      "contains_identifiers", "contains_row_level_data", "disclosure_status"
    ),
    value = c(
      format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      as.character(utils::packageVersion("reactextract")),
      version$dictionary_release[[1]],
      version$manifest_sha256[[1]],
      length(relevant_rounds),
      length(unique(duration_occurrences$variable)),
      nrow(outputs[["day-week-completion-summary.csv"]]),
      nrow(outputs[["categorical-code-summary.csv"]]),
      "no", "no",
      "keep_inside_enclave_until_normal_disclosure_review"
    ),
    stringsAsFactors = FALSE
  )
  utils::write.csv(manifest, file.path(output, "manifest.csv"), row.names = FALSE)
  write_explanation(output)

  message("Diagnostic report written to ", normalizePath(output, mustWork = TRUE))
  message(
    "No identifiers or respondent-level rows were written. Keep the aggregate ",
    "report inside the enclave until it has passed normal disclosure review."
  )
  invisible(outputs)
}

validate_identifier <- function(value, label) {
  if (length(value) != 1L || is.na(value) ||
      !grepl("^[A-Z][A-Z0-9_.]*$", value)) {
    stop("Unsafe ", label, ": ", value, ".", call. = FALSE)
  }
  invisible(value)
}

quote_identifier <- function(value) paste0('"', value, '"')

single_count <- function(data, expected_name) {
  index <- match(toupper(expected_name), toupper(names(data)))
  if (nrow(data) != 1L || is.na(index)) {
    stop("Aggregate query did not return ", expected_name, ".", call. = FALSE)
  }
  as.numeric(data[[index]][[1]])
}

get_column <- function(data, field) {
  index <- match(toupper(field), toupper(names(data)))
  if (is.na(index)) stop("Query result omitted field ", field, ".", call. = FALSE)
  data[[index]]
}

numeric_values <- function(value) {
  suppressWarnings(as.numeric(as.character(value)))
}

field_format <- function(field) {
  if (grepl("^LONGCOVIDB_1_", field)) return("continuous_days")
  if (grepl("^LONGCOVIDB_2_", field)) return("continuous_weeks")
  if (grepl("^LONGCOVIDB2_", field)) return("categorical_or_round6_value")
  "other"
}

value_counts <- function(value, total_rows) {
  display <- as.character(value[!is.na(value)])
  counts <- if (length(display)) {
    result <- as.data.frame(table(display, useNA = "no"), stringsAsFactors = FALSE)
    names(result) <- c("raw_value", "count")
    result$count <- as.numeric(result$count)
    result
  } else {
    data.frame(raw_value = character(), count = numeric(), stringsAsFactors = FALSE)
  }
  rbind(
    counts,
    data.frame(
      raw_value = "<DATABASE NULL>",
      count = total_rows - length(display),
      stringsAsFactors = FALSE
    )
  )
}

safe_min <- function(value) if (length(value)) min(value, na.rm = TRUE) else NA_real_
safe_max <- function(value) if (length(value)) max(value, na.rm = TRUE) else NA_real_

pair_summary <- function(round_id, object, total_rows, item_number, concept_id,
                         question_text, day_field, week_field, target_fields,
                         days, weeks) {
  day_present <- !is.na(days)
  week_present <- !is.na(weeks)
  day_valid <- day_present & days >= 0
  week_valid <- week_present & weeks >= 0
  data.frame(
    round_id = round_id,
    source_view = object,
    item_number_within_round = item_number,
    concept_id = concept_id,
    question_text = question_text,
    day_field = day_field,
    week_field = week_field,
    target_categorical_fields = paste(target_fields, collapse = " | "),
    source_row_count = total_rows,
    either_field_nonmissing_count = sum(day_present | week_present),
    neither_field_nonmissing_count = total_rows - sum(day_present | week_present),
    day_field_nonmissing_count = sum(day_present),
    week_field_nonmissing_count = sum(week_present),
    both_fields_nonmissing_count = sum(day_present & week_present),
    either_valid_nonnegative_count = sum(day_valid | week_valid),
    day_only_valid_count = sum(day_valid & !week_valid),
    week_only_valid_count = sum(!day_valid & week_valid),
    both_valid_count = sum(day_valid & week_valid),
    both_positive_count = sum(day_valid & week_valid & days > 0 & weeks > 0),
    day_positive_week_zero_count = sum(day_valid & week_valid & days > 0 & weeks == 0),
    day_zero_week_positive_count = sum(day_valid & week_valid & days == 0 & weeks > 0),
    both_zero_count = sum(day_valid & week_valid & days == 0 & weeks == 0),
    day_minimum_nonnegative = safe_min(days[day_valid]),
    day_maximum_nonnegative = safe_max(days[day_valid]),
    week_minimum_nonnegative = safe_min(weeks[week_valid]),
    week_maximum_nonnegative = safe_max(weeks[week_valid]),
    day_noninteger_count = sum(day_valid & days != trunc(days)),
    week_noninteger_count = sum(week_valid & weeks != trunc(weeks)),
    stringsAsFactors = FALSE
  )
}

paired_value_counts <- function(round_id, item_number, concept_id, day_field,
                                week_field, days, weeks) {
  keep <- !is.na(days) & !is.na(weeks) & days >= 0 & weeks >= 0
  if (!any(keep)) {
    return(data.frame(
      round_id = character(), item_number_within_round = character(),
      concept_id = character(), day_field = character(), week_field = character(),
      day_value = numeric(), week_value = numeric(), count = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  counts <- aggregate(
    rep(1, sum(keep)),
    by = list(day_value = days[keep], week_value = weeks[keep]),
    FUN = sum
  )
  names(counts)[[3]] <- "count"
  counts$round_id <- round_id
  counts$item_number_within_round <- item_number
  counts$concept_id <- concept_id
  counts$day_field <- day_field
  counts$week_field <- week_field
  counts[c(
    "round_id", "item_number_within_round", "concept_id", "day_field",
    "week_field", "day_value", "week_value", "count"
  )]
}

duration_band <- function(days) {
  cut(
    days,
    breaks = c(-Inf, 28, 61, 92, 183, Inf),
    right = FALSE,
    labels = c(
      "1 Less than four weeks",
      "2 Four weeks up to two months",
      "3 Two months up to three months",
      "4 Three months up to six months",
      "5 More than six months"
    )
  )
}

compare_binning <- function(round_id, item_number, concept_id, day_field,
                            week_field, target_fields, days, weeks) {
  day_valid <- !is.na(days) & days >= 0
  week_valid <- !is.na(weeks) & weeks >= 0
  interpretations <- list(
    add_days_and_weeks = ifelse(day_valid | week_valid,
      ifelse(day_valid, days, 0) + ifelse(week_valid, weeks * 7, 0), NA_real_),
    prefer_weeks_else_days = ifelse(week_valid, weeks * 7,
      ifelse(day_valid, days, NA_real_)),
    prefer_days_else_weeks = ifelse(day_valid, days,
      ifelse(week_valid, weeks * 7, NA_real_))
  )
  rows <- lapply(names(interpretations), function(method) {
    bands <- duration_band(interpretations[[method]])
    counts <- as.data.frame(table(bands, useNA = "ifany"), stringsAsFactors = FALSE)
    names(counts) <- c("target_category", "count")
    counts$count <- as.numeric(counts$count)
    counts$interpretation <- method
    counts
  })
  result <- bind_rows(rows)
  result$round_id <- round_id
  result$item_number_within_round <- item_number
  result$concept_id <- concept_id
  result$day_field <- day_field
  result$week_field <- week_field
  result$target_categorical_fields <- paste(target_fields, collapse = " | ")
  result[c(
    "round_id", "item_number_within_round", "concept_id", "day_field",
    "week_field", "target_categorical_fields", "interpretation",
    "target_category", "count"
  )]
}

assess_categorical_codes <- function(values) {
  observed <- values[!is.na(values)]
  if (!length(observed)) return("no_values")
  positive <- observed[observed >= 0]
  if (!length(positive)) return("administrative_codes_only")
  if (all(positive %in% 1:7)) return("matches_expected_1_to_7_categories")
  if (all(positive == trunc(positive)) && max(positive) > 7) {
    return("does_not_look_like_expected_categories")
  }
  "mixed_or_unexpected_values"
}

bind_rows <- function(rows) {
  rows <- Filter(function(x) !is.null(x) && nrow(x) > 0L, rows)
  if (!length(rows)) return(data.frame())
  names_union <- unique(unlist(lapply(rows, names), use.names = FALSE))
  rows <- lapply(rows, function(row) {
    missing <- setdiff(names_union, names(row))
    for (name in missing) row[[name]] <- NA
    row[names_union]
  })
  do.call(rbind, rows)
}

write_explanation <- function(output) {
  text <- c(
    "REACT Long-COVID duration diagnostic",
    "",
    "This report answers three questions:",
    "1. Are DAYS and WEEKS companion fields populated together?",
    "2. Do the values look like components to add, or alternatives to choose between?",
    "3. Do LONGCOVIDB2 fields, especially REACT-2 round 6, use the expected 1-7 categories?",
    "",
    "Start with day-week-completion-summary.csv. High counts in both_valid_count,",
    "day_positive_week_zero_count, day_zero_week_positive_count, or both_positive_count",
    "show how the paired fields behave. day-week-both-value-counts.csv gives aggregate",
    "value combinations without identifiers.",
    "",
    "candidate-binning-comparison.csv shows how three interpretations change the final",
    "duration bands. It is diagnostic evidence, not an approved transformation.",
    "",
    "categorical-code-summary.csv shows whether each LONGCOVIDB2 field contains only",
    "the expected category codes 1-7 (apart from negative administrative codes).",
    "field-value-counts.csv contains the complete aggregate value frequencies.",
    "",
    "No identifiers or respondent-level records are written. Counts are unsuppressed",
    "and may contain small cells, so keep the folder inside the enclave until it passes",
    "the normal disclosure-review process."
  )
  writeLines(text, file.path(output, "README.txt"), useBytes = TRUE)
}

if (sys.nframe() == 0L) main()
