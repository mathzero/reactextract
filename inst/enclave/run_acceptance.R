main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1L || length(args) > 2L) {
    stop(
      "Usage: Rscript run_acceptance.R CONNECTION_SCRIPT [OUTPUT_DIRECTORY]",
      call. = FALSE
    )
  }
  connection_script <- normalizePath(args[[1]], mustWork = TRUE)
  output <- if (length(args) == 2L) args[[2]] else "reactextract-acceptance"
  dir.create(output, recursive = TRUE, showWarnings = FALSE)

  if (!requireNamespace("reactextract", quietly = TRUE) ||
      !requireNamespace("DBI", quietly = TRUE) ||
      !requireNamespace("odbc", quietly = TRUE)) {
    stop("Install reactextract, DBI, and odbc before running acceptance.", call. = FALSE)
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

  source <- reactextract::react_oracle(con)
  validation <- reactextract::react_validate_oracle(source)
  reactextract::react_write_enclave_report(
    validation,
    file.path(output, "preflight")
  )
  preflight_status <- validation$manifest$value[
    validation$manifest$key == "status"
  ]
  preflight_passed <- preflight_status %in% c("passed", "passed_with_notes")

  smoke_rounds <- c("react1.r10", "react2.r01")
  smoke_concepts <- c(
    "health.preexisting.overweight",
    "health.acute_symptoms.highest_temperature_reading",
    "infection_measurement.testing_history.previous_antibody_test_history"
  )
  smoke_status <- "skipped"
  smoke_message <- "Smoke extraction was skipped because preflight requires attention."
  if (preflight_passed) {
    smoke <- reactextract::react_extract(
      source,
      families = "all",
      rounds = smoke_rounds,
      concepts = smoke_concepts,
      progress = TRUE
    )
    count_by_round <- function(data) {
      counts <- table(factor(data$round_id, levels = smoke_rounds))
      as.integer(counts)
    }
    smoke_summary <- data.frame(
      round_id = smoke_rounds,
      observation_count = count_by_round(smoke$observations),
      raw_value_count = count_by_round(smoke$raw_values),
      harmonised_value_count = count_by_round(smoke$harmonised_values),
      cleaned_output_column_count = rep(
        length(unique(smoke$column_dictionary$output_column)),
        length(smoke_rounds)
      ),
      requested_concept_count = rep(length(smoke_concepts), length(smoke_rounds)),
      stringsAsFactors = FALSE
    )
    utils::write.csv(
      smoke_summary, file.path(output, "smoke-summary.csv"),
      row.names = FALSE, na = ""
    )
    utils::write.csv(
      smoke$issues, file.path(output, "smoke-issues.csv"),
      row.names = FALSE, na = ""
    )
    utils::write.csv(
      smoke$manifest, file.path(output, "smoke-manifest.csv"),
      row.names = FALSE, na = ""
    )
    smoke_status <- if (all(smoke_summary$observation_count > 0L) &&
        all(smoke_summary$raw_value_count > 0L) &&
        all(smoke_summary$harmonised_value_count > 0L) &&
        all(smoke_summary$cleaned_output_column_count > 0L)) "passed" else "failed"
    smoke_message <- if (smoke_status == "passed") {
      "Row-level smoke results were discarded; only aggregate diagnostics were written."
    } else {
      "One or both representative rounds returned no observations or raw values."
    }
    rm(smoke)
    invisible(gc())
  }

  overall <- if (preflight_passed && identical(smoke_status, "passed")) {
    if (identical(preflight_status, "passed_with_notes")) {
      "passed_with_notes"
    } else {
      "passed"
    }
  } else {
    "attention_required"
  }
  status <- data.frame(
    check = c("preflight", "smoke", "overall"),
    status = c(preflight_status, smoke_status, overall),
    message = c(
      "All 25 views, exact dictionary fields, keys, and crosswalk were checked.",
      smoke_message,
      "Keep this report inside the enclave unless it passes normal disclosure review."
    ),
    stringsAsFactors = FALSE
  )
  utils::write.csv(
    status, file.path(output, "acceptance-status.csv"),
    row.names = FALSE, na = ""
  )
  message("Acceptance report written to ", normalizePath(output, mustWork = TRUE))
  if (!identical(overall, "passed")) {
    message("Acceptance requires attention; inspect the report before extracting data.")
  }
  invisible(status)
}

main()
