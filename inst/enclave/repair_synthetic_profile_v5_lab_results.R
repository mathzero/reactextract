main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 3L) {
    stop(
      paste(
        "Usage: Rscript repair_synthetic_profile_v5_lab_results.R",
        "CONNECTION_SCRIPT V5_CANDIDATE OUTPUT_DIRECTORY"
      ),
      call. = FALSE
    )
  }
  connection_script <- normalizePath(args[[1L]], mustWork = TRUE)
  candidate_path <- normalizePath(args[[2L]], mustWork = TRUE)
  output <- args[[3L]]
  if (dir.exists(output) && length(list.files(
    output, all.files = TRUE, no.. = TRUE
  ))) {
    stop("The output must be a new or empty directory.", call. = FALSE)
  }

  expected_candidate_manifest <-
    "de313c74d64edd725c91aa3eb5784d82cda7da65957b7e1df707bf0e310704a2"
  candidate_manifest <- file.path(candidate_path, "manifest.csv")
  if (!file.exists(candidate_manifest) || !identical(
    reactextract:::.sha256_file(candidate_manifest),
    expected_candidate_manifest
  )) {
    stop(
      "The input is not the checksum-verified v5 candidate returned on 2026-08-28.",
      call. = FALSE
    )
  }

  candidate <- reactextract::react_read_profile(candidate_path)
  candidate_metadata <- stats::setNames(
    candidate$metadata$value, candidate$metadata$key
  )
  if (!identical(
    unname(candidate_metadata[["profile_version"]]),
    "enclave-profile-v5-outcome-dependencies"
  ) || !identical(
    unname(candidate_metadata[["dictionary_manifest_sha256"]]),
    "f8da578f8aa7827ab3c484ae964853b45aa24a053e20d0423b1e58b59e49410a"
  ) || !identical(
    unname(candidate_metadata[["disclosure_approval"]]), "not_approved"
  )) {
    stop("The input profile is not the expected unapproved rc11 v5 candidate.", call. = FALSE)
  }

  dictionary <- reactextract::react_dictionary()
  result_ids <- unique(dictionary$synthetic_profile_overrides$occurrence_id)
  result_occurrences <- dictionary$occurrences[
    dictionary$occurrences$occurrence_id %in% result_ids,
    c("occurrence_id", "round_id", "variable"),
    drop = FALSE
  ]
  result_rounds <- dictionary$rounds$round_id[
    dictionary$rounds$round_id %in% result_occurrences$round_id
  ]
  if (length(result_ids) != 37L || length(result_rounds) != 19L ||
      !all(result_occurrences$variable %in% c("RESULT", "FINALRESULT"))) {
    stop("The installed dictionary does not contain the fixed 37-field result repair.", call. = FALSE)
  }
  reviewed_occurrence_supports <- c(
    occ_3d8dfcbd9554c0afc41ef7e2 =
      "react1_pcr_lab_result_r02_final_v1",
    occ_654a0c1cc8cdb5d263af8bad =
      "react1_pcr_lab_result_r11_result_v1",
    occ_02fe3a74aee39c3150210a30 =
      "react1_pcr_lab_result_r13_result_v1"
  )
  observed_supports <- stats::setNames(
    dictionary$synthetic_profile_overrides$support_id[
      match(
        names(reviewed_occurrence_supports),
        dictionary$synthetic_profile_overrides$occurrence_id
      )
    ],
    names(reviewed_occurrence_supports)
  )
  if (!identical(observed_supports, reviewed_occurrence_supports)) {
    stop(
      "The installed dictionary does not contain the three approved occurrence-specific result supports.",
      call. = FALSE
    )
  }
  reactextract:::.validate_dependency_profile_contract(
    candidate, dictionary, rounds = "all"
  )

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

  message(
    "[reactextract] Correcting 37 RESULT/FINALRESULT distributions across ",
    "19 REACT-1 rounds. Existing Ct/Cp tables are retained."
  )
  repair <- reactextract::react_profile_source(
    reactextract::react_oracle(con),
    rounds = result_rounds,
    occurrence_ids = result_ids,
    include_routing = FALSE,
    include_overall = FALSE,
    include_dependencies = FALSE,
    progress = TRUE
  )
  reactextract:::.validate_lab_result_profile_repair(
    repair, result_ids, result_rounds, dictionary = dictionary
  )

  message(
    "[reactextract] Recalculating the REACT-1 outcome and dependency aggregates ",
    "so exact blank results remain missing rather than PCR-negative."
  )
  dependency_repair <- reactextract::react_profile_dependencies_source(
    reactextract::react_oracle(con),
    rounds = result_rounds,
    progress = TRUE
  )
  dependency_errors <- dependency_repair$issues$severity == "error"
  dependency_errors[is.na(dependency_errors)] <- FALSE
  if (!setequal(dependency_repair$round_denominators$round_id, result_rounds) ||
      !nrow(dependency_repair$outcome_counts) ||
      !nrow(dependency_repair$dependency_counts) || any(dependency_errors)) {
    stop(
      paste(
        "The corrected REACT-1 outcome/dependency aggregates were incomplete;",
        "no repair was written."
      ),
      call. = FALSE
    )
  }
  safe_dependency_repair <- reactextract::react_prepare_profile_export(
    dependency_repair
  )

  retained_profiled <- candidate$profiled_occurrences
  corrected <- reactextract::react_repair_profile(candidate, repair)
  corrected$outcome_counts <- rbind(
    candidate$outcome_counts[
      !(candidate$outcome_counts$round_id %in% result_rounds), , drop = FALSE
    ],
    safe_dependency_repair$outcome_counts
  )
  corrected$dependency_counts <- rbind(
    candidate$dependency_counts[
      !(candidate$dependency_counts$round_id %in% result_rounds), , drop = FALSE
    ],
    safe_dependency_repair$dependency_counts
  )
  corrected$dependency_specs <- dictionary$synthetic_dependencies[
    dictionary$synthetic_dependencies$review_state == "approved", , drop = FALSE
  ]
  retained_profiled <- retained_profiled[
    !(retained_profiled$occurrence_id %in% result_ids), , drop = FALSE
  ]
  repaired_profiled <- repair$profiled_occurrences
  repaired_profiled$status <- "corrected_occurrence_specific_lab_result_support"
  corrected$profiled_occurrences <- rbind(
    retained_profiled[, names(repaired_profiled), drop = FALSE],
    repaired_profiled
  )
  reactextract:::.validate_dependency_profile_contract(
    corrected, dictionary, rounds = "all"
  )

  dependency_manifest <- dictionary$manifest[
    dictionary$manifest$file == "synthetic_dependencies.csv",
    , drop = FALSE
  ]
  if (nrow(dependency_manifest) != 1L) {
    stop("The dictionary manifest has no unique dependency-contract row.", call. = FALSE)
  }
  replace_keys <- c(
    "profile_version", "package_version", "status", "disclosure_approval",
    "profile_scope", "profiled_occurrence_count",
    "lab_result_occurrence_count", "ct_cp_occurrence_count",
    "dependency_specification_sha256", "dependency_specification_hash_basis",
    "source_candidate_manifest_sha256", "lab_result_support_contract",
    "occurrence_specific_lab_result_handling", "dependency_aggregates",
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
        "enclave-profile-v5-outcome-dependencies-lab-results-corrected",
        as.character(utils::packageVersion("reactextract")),
        "requires_enclave_disclosure_review", "not_approved",
        "complete_with_targeted_lab_result_label_correction",
        as.character(nrow(corrected$profiled_occurrences)), "37", "50",
        dependency_manifest$sha256[[1L]], "canonical_dictionary_csv",
        expected_candidate_manifest,
        paste(unname(reviewed_occurrence_supports), collapse = "|"),
        paste(
          "r02_FINALRESULT_Rejected_missing",
          "r11_RESULT_lowercase_negative_negative",
          "r13_RESULT_ambiguous_missing",
          sep = "|"
        ),
        "react1_reprofiled_after_occurrence_specific_result_correction",
        "not_available_after_targeted_repair"
      ),
      stringsAsFactors = FALSE
    )
  )
  reactextract::react_write_profile(corrected, output)
  message(
    "Corrected v5 candidate written to ",
    normalizePath(output, mustWork = TRUE),
    ". It still requires normal disclosure approval."
  )
  invisible(corrected)
}

main()
