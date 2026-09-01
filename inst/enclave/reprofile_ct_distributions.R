main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 3L) {
    stop(
      paste(
        "Usage: Rscript reprofile_ct_distributions.R CONNECTION_SCRIPT",
        "APPROVED_V4_PROFILE OUTPUT_DIRECTORY"
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
  base_metadata <- stats::setNames(base$metadata$value, base$metadata$key)
  if (!identical(
    unname(base_metadata[["dictionary_manifest_sha256"]]),
    "03a2fb41a02becbe292663934e6ed436a85335b93d5004118a82ea9e4460a846"
  ) || !identical(
    unname(base_metadata[["profile_version"]]),
    "enclave-profile-v2-domain-corrected"
  )) {
    stop("The input is not the approved rc9/v4 baseline profile.", call. = FALSE)
  }

  dictionary <- reactextract::react_dictionary()
  ct_specs <- dictionary$synthetic_profile_specs[
    dictionary$synthetic_profile_specs$review_state == "approved" &
      dictionary$synthetic_profile_specs$bin_spec_id == "ct_value_v2",
    , drop = FALSE
  ]
  if (nrow(ct_specs) != 50L || anyDuplicated(ct_specs$occurrence_id)) {
    stop("Expected exactly 50 approved Ct/Cp occurrences.", call. = FALSE)
  }
  ct_occurrences <- dictionary$occurrences[
    match(ct_specs$occurrence_id, dictionary$occurrences$occurrence_id),
    , drop = FALSE
  ]
  if (anyNA(ct_occurrences$occurrence_id)) {
    stop("A Ct/Cp profile specification has no source occurrence.", call. = FALSE)
  }
  repair_rounds <- dictionary$rounds$round_id[
    dictionary$rounds$round_id %in% ct_occurrences$round_id
  ]
  message(
    "[reactextract] Re-profiling 50 Ct/Cp fields across ",
    length(repair_rounds), " REACT-1 rounds."
  )

  repair <- reactextract::react_profile_source(
    reactextract::react_oracle(con),
    rounds = repair_rounds,
    occurrence_ids = ct_specs$occurrence_id,
    include_routing = FALSE,
    include_overall = FALSE,
    progress = TRUE
  )
  reactextract:::.validate_ct_profile_repair(
    repair,
    ct_specs$occurrence_id,
    repair_rounds
  )
  corrected <- reactextract::react_repair_profile(base, repair)
  corrected$profiled_occurrences$status <- "reprofiled_ct_cp_ranges"

  replace_keys <- c(
    "profile_version", "status", "disclosure_approval", "profile_scope",
    "profiled_occurrence_count", "ct_cp_occurrence_count",
    "overall_distributions"
  )
  corrected$metadata <- corrected$metadata[
    !(corrected$metadata$key %in% replace_keys),
    , drop = FALSE
  ]
  corrected$metadata <- rbind(
    corrected$metadata,
    data.frame(
      key = replace_keys,
      value = c(
        "enclave-profile-v2-ct-cp-bins",
        "requires_enclave_disclosure_review", "not_approved",
        "complete_with_targeted_ct_cp_repair", "50", "50",
        "not_available_after_targeted_repair"
      ),
      stringsAsFactors = FALSE
    )
  )
  reactextract::react_write_profile(corrected, output)
  message(
    "Ct/Cp profile written to ", normalizePath(output, mustWork = TRUE),
    ". It still requires normal disclosure approval."
  )
  invisible(corrected)
}

main()
