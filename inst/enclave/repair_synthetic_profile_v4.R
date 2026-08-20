main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 3L) {
    stop(
      paste(
        "Usage: Rscript repair_synthetic_profile_v4.R CONNECTION_SCRIPT",
        "APPROVED_V2_PROFILE OUTPUT_DIRECTORY"
      ),
      call. = FALSE
    )
  }
  connection_script <- normalizePath(args[[1L]], mustWork = TRUE)
  base_path <- normalizePath(args[[2L]], mustWork = TRUE)
  output <- args[[3L]]

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

  base <- reactextract::react_read_profile(base_path)
  recovered <- reactextract:::.recover_singleton_profile_categories(base)
  if (length(recovered$recovered_occurrence_ids) != 151L ||
      length(recovered$remaining_occurrence_ids) != 17L) {
    stop(
      "Expected 151 safe local recoveries and 17 enclave re-profiles; refusing a different scope.",
      call. = FALSE
    )
  }

  dictionary <- reactextract::react_dictionary()
  remaining <- dictionary$occurrences[
    dictionary$occurrences$occurrence_id %in%
      recovered$remaining_occurrence_ids,
    , drop = FALSE
  ]
  repair_rounds <- dictionary$rounds$round_id[
    dictionary$rounds$round_id %in% remaining$round_id
  ]
  message(
    "[reactextract] Recovered 151 exact category distributions from the ",
    "approved singleton bins. Re-profiling 17 fields across ",
    length(repair_rounds), " rounds."
  )

  repair <- reactextract::react_profile_source(
    reactextract::react_oracle(con),
    rounds = repair_rounds,
    occurrence_ids = recovered$remaining_occurrence_ids,
    include_routing = FALSE,
    include_overall = FALSE,
    progress = TRUE
  )
  corrected <- reactextract::react_repair_profile(recovered$profile, repair)

  recovered_occurrences <- dictionary$occurrences[
    match(
      recovered$recovered_occurrence_ids,
      dictionary$occurrences$occurrence_id
    ),
    c("occurrence_id", "round_id", "variable"),
    drop = FALSE
  ]
  recovered_occurrences$profile_kind <- "categorical"
  recovered_occurrences$status <- "recovered_from_approved_singleton_bins"
  reprofiled_occurrences <- repair$profiled_occurrences
  reprofiled_occurrences$status <- "reprofiled_from_source"
  corrected$profiled_occurrences <- rbind(
    recovered_occurrences,
    reprofiled_occurrences[, names(recovered_occurrences), drop = FALSE]
  )

  replace_keys <- c(
    "profile_version", "package_version", "status", "disclosure_approval",
    "profile_scope", "profiled_occurrence_count",
    "locally_recovered_occurrence_count",
    "source_reprofiled_occurrence_count", "overall_distributions"
  )
  corrected$metadata <- corrected$metadata[
    !(corrected$metadata$key %in% replace_keys), , drop = FALSE
  ]
  corrected$metadata <- rbind(
    corrected$metadata,
    data.frame(
      key = replace_keys,
      value = c(
        "enclave-profile-v2-domain-corrected",
        as.character(utils::packageVersion("reactextract")),
        "requires_enclave_disclosure_review", "not_approved",
        "complete_with_safe_local_recovery_and_targeted_repair", "168",
        "151", "17", "not_available_after_targeted_repair"
      ),
      stringsAsFactors = FALSE
    )
  )
  reactextract::react_write_profile(corrected, output)
  message(
    "Corrected profile written to ", normalizePath(output, mustWork = TRUE),
    ". It still requires normal disclosure approval."
  )
  invisible(corrected)
}

main()
