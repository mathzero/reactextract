main <- function() {
  # This is a historical builder: it must continue to recognise the exact v4
  # baseline even after the package's live approved-profile pin moves to v5.
  approved_v4_manifest_sha256 <-
    "fbf7bfc9453cb06a99284d0d5bc86d3bcd5fdc7f7d942561eaba2df7f99d6990"
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 3L) {
    stop(
      paste(
        "Usage: Rscript build_synthetic_profile_v5.R CONNECTION_SCRIPT",
        "APPROVED_V4_PROFILE OUTPUT_DIRECTORY"
      ),
      call. = FALSE
    )
  }
  connection_script <- normalizePath(args[[1L]], mustWork = TRUE)
  base_path <- normalizePath(args[[2L]], mustWork = TRUE)
  output <- args[[3L]]
  if (dir.exists(output) && length(list.files(
    output, all.files = TRUE, no.. = TRUE
  ))) {
    stop(
      "The output must be a new or empty directory; the approved baseline is never overwritten.",
      call. = FALSE
    )
  }

  base_manifest <- file.path(base_path, "manifest.csv")
  if (!file.exists(base_manifest) || !identical(
    reactextract:::.sha256_file(base_manifest),
    approved_v4_manifest_sha256
  )) {
    stop(
      "The input profile is not the checksum-pinned, formally approved v4 baseline.",
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

  base <- reactextract::react_read_profile(base_path)
  base_metadata <- stats::setNames(base$metadata$value, base$metadata$key)
  if (!identical(
    unname(base_metadata[["dictionary_manifest_sha256"]]),
    "03a2fb41a02becbe292663934e6ed436a85335b93d5004118a82ea9e4460a846"
  ) || !identical(
    unname(base_metadata[["profile_version"]]),
    "enclave-profile-v2-domain-corrected"
  )) {
    stop("The input is not the formally approved rc9/v4 baseline profile.", call. = FALSE)
  }

  dictionary <- reactextract::react_dictionary()
  source <- reactextract::react_oracle(con)
  changes <- reactextract:::.profile_contract_rebase_ids(base, dictionary)
  repair_ids <- changes$repair_ids
  repair_occurrences <- dictionary$occurrences[
    dictionary$occurrences$occurrence_id %in% repair_ids, , drop = FALSE
  ]
  message(
    "[reactextract] Re-profiling ", length(repair_ids),
    " fields whose public treatment changed (Ct/Cp and laboratory results)."
  )
  repair <- reactextract::react_profile_source(
    source,
    rounds = unique(repair_occurrences$round_id),
    occurrence_ids = repair_ids,
    include_routing = FALSE,
    include_overall = FALSE,
    include_dependencies = FALSE,
    progress = TRUE
  )
  if (!setequal(repair$profiled_occurrences$occurrence_id, repair_ids)) {
    stop("The targeted distribution repair did not cover every changed field.", call. = FALSE)
  }
  ct_ids <- dictionary$synthetic_profile_specs$occurrence_id[
    dictionary$synthetic_profile_specs$bin_spec_id == "ct_value_v2"
  ]
  result_ids <- unique(dictionary$synthetic_profile_overrides$occurrence_id)
  react1_rounds <- dictionary$rounds$round_id[
    dictionary$rounds$study_id == "react1"
  ]
  reactextract:::.validate_ct_profile_repair(repair, ct_ids, react1_rounds)
  reactextract:::.validate_lab_result_profile_repair(
    repair, result_ids, react1_rounds, dictionary = dictionary
  )
  corrected <- reactextract::react_repair_profile(base, repair)

  message("[reactextract] Profiling the 22 reviewed outcome-centred dependencies.")
  dependency_profile <- reactextract::react_profile_dependencies_source(
    source, rounds = "all", progress = TRUE
  )
  safe_dependencies <- reactextract::react_prepare_profile_export(dependency_profile)
  reactextract:::.validate_dependency_profile_contract(
    safe_dependencies, dictionary, rounds = "all"
  )
  corrected$outcome_counts <- safe_dependencies$outcome_counts
  corrected$dependency_counts <- safe_dependencies$dependency_counts
  corrected$dependency_specs <- safe_dependencies$dependency_specs
  corrected$profile_specs <- reactextract:::.approved_profile_specs(dictionary)
  corrected$issues <- reactextract:::.bind_rows(
    list(corrected$issues, safe_dependencies$issues), reactextract:::.empty_issues()
  )

  dependency_manifest <- dictionary$manifest[
    dictionary$manifest$file == "synthetic_dependencies.csv",
    , drop = FALSE
  ]
  if (nrow(dependency_manifest) != 1L) {
    stop("The dictionary manifest has no unique dependency-contract row.", call. = FALSE)
  }
  replace_keys <- c(
    "profile_version", "status", "disclosure_approval", "profile_scope",
    "profiled_occurrence_count", "dependency_specification_sha256",
    "dependency_specification_hash_basis", "dependency_tables",
    "lab_result_support_contract", "occurrence_specific_lab_result_handling",
    "overall_distributions"
  )
  corrected$metadata <- corrected$metadata[
    !(corrected$metadata$key %in% replace_keys), , drop = FALSE
  ]
  corrected$metadata <- rbind(
    corrected$metadata,
    data.frame(
      key = replace_keys,
      value = c(
        "enclave-profile-v5-outcome-dependencies",
        "requires_enclave_disclosure_review", "not_approved",
        "complete_with_targeted_distribution_and_dependency_upgrade",
        as.character(length(repair_ids)),
        dependency_manifest$sha256[[1L]], "canonical_dictionary_csv",
        "included_disclosure_controlled",
        paste(
          "react1_pcr_lab_result_r02_final_v1",
          "react1_pcr_lab_result_r11_result_v1",
          "react1_pcr_lab_result_r13_result_v1",
          sep = "|"
        ),
        paste(
          "r02_FINALRESULT_Rejected_missing",
          "r11_RESULT_lowercase_negative_negative",
          "r13_RESULT_ambiguous_missing",
          sep = "|"
        ),
        "not_available_after_targeted_repair"
      ),
      stringsAsFactors = FALSE
    )
  )
  reactextract::react_write_profile(corrected, output)
  message(
    "Synthetic profile v5 candidate written to ",
    normalizePath(output, mustWork = TRUE),
    ". Keep it inside the enclave until normal disclosure approval."
  )
  invisible(corrected)
}

main()
