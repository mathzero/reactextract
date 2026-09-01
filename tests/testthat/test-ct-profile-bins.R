test_that("rc10 assigns all Ct and Cp occurrences to the reviewed v2 bins", {
  dictionary <- react_dictionary()
  specs <- dictionary$synthetic_profile_specs[
    dictionary$synthetic_profile_specs$bin_spec_id == "ct_value_v2",
    , drop = FALSE
  ]
  occurrences <- dictionary$occurrences[
    match(specs$occurrence_id, dictionary$occurrences$occurrence_id),
    , drop = FALSE
  ]

  expect_equal(nrow(specs), 50L)
  expect_identical(anyDuplicated(specs$occurrence_id), 0L)
  expect_true(all(specs$profile_kind == "continuous"))
  expect_setequal(
    unique(occurrences$variable),
    c(
      "CT_VALUE1", "CT_VALUE2", "EGENE_CTVALUE", "NGENE_CTVALUE",
      "EGENE_CTVALUE_SHED1", "NGENE_CTVALUE_SHED1",
      "EGENE_CTVALUE_SHED2", "NGENE_CTVALUE_SHED2",
      "INFLUENZAACPVALUE", "INFLUENZABCPVALUE"
    )
  )
  expect_false(any(dictionary$synthetic_profile_specs$bin_spec_id == "ct_value_v1"))
})

test_that("Ct decimal values respect exact open and closed boundaries", {
  bins <- react_dictionary()$safe_bins
  bins <- bins[bins$bin_spec_id == "ct_value_v2", , drop = FALSE]
  bins <- bins[match(
    c("zero", "1_10", "11_20", "21_30", "31_40", "41_50"),
    bins$bin_id
  ), , drop = FALSE]
  values <- c(
    0, 0.1, 1, 10, 10.1, 11, 20, 20.1, 30, 30.1, 40, 40.1,
    50, 50.1, NA_real_
  )

  expect_identical(
    .bin_values(values, bins, "continuous"),
    c(
      "zero", "1_10", "1_10", "1_10", "11_20", "11_20",
      "11_20", "21_30", "21_30", "31_40", "31_40", "41_50",
      "41_50", NA_character_, NA_character_
    )
  )

  bad_bins <- bins[1L, , drop = FALSE]
  bad_bins$boundary_rules <- "unknown_rule"
  expect_error(
    .bin_values(0, bad_bins, "continuous"),
    "Unsupported safe-bin boundary rule"
  )
  source <- list(
    profile = list(
      safe_bins = bad_bins,
      numeric_bin_counts = data.frame(
        occurrence_id = "occ_test", round_id = "react1.r01",
        bin_spec_id = "ct_value_v2", bin_id = bad_bins$bin_id,
        count = 100, suppressed = FALSE, stringsAsFactors = FALSE
      )
    ),
    safe_prior_fraction = 0,
    seed = 1L
  )
  occurrence <- data.frame(
    occurrence_id = "occ_test", round_id = "react1.r01",
    stringsAsFactors = FALSE
  )
  spec <- data.frame(
    profile_kind = "continuous", bin_spec_id = "ct_value_v2",
    stringsAsFactors = FALSE
  )
  expect_error(
    .synthetic_binned_value(source, occurrence, spec, 1L),
    "Unsupported safe-bin boundary rule"
  )
})

test_that("the approved v5 profile has the complete targeted repair scope", {
  archive <- system.file(
    "extdata", "synthetic-profile.tar.gz", package = "reactextract"
  )
  path <- tempfile("approved-v5-")
  dir.create(path)
  utils::untar(archive, exdir = path)
  profile <- react_read_profile(path)
  dictionary <- react_dictionary()
  expected_ct <- dictionary$synthetic_profile_specs$occurrence_id[
    dictionary$synthetic_profile_specs$bin_spec_id == "ct_value_v2"
  ]
  expected_results <- dictionary$synthetic_profile_overrides$occurrence_id
  expected <- unique(c(expected_ct, expected_results))

  expect_setequal(profile$profiled_occurrences$occurrence_id, expected)
  expect_equal(length(expected_ct), 50L)
  expect_equal(length(expected_results), 37L)
  expect_equal(length(expected), 87L)

  script <- readLines(system.file(
    "enclave", "build_synthetic_profile_v5.R", package = "reactextract"
  ), warn = FALSE)
  expect_true(any(grepl(
    "approved_v4_manifest_sha256", script, fixed = TRUE
  )))
  expect_true(any(grepl(
    "fbf7bfc9453cb06a99284d0d5bc86d3bcd5fdc7f7d942561eaba2df7f99d6990",
    script, fixed = TRUE
  )))
  expect_true(any(grepl(
    "approved baseline is never overwritten", script, fixed = TRUE
  )))
})

test_that("a partial or out-of-range Ct repair is rejected", {
  ids <- sprintf("occ_%02d", seq_len(50L))
  rounds <- sprintf("react1.r%02d", seq_len(19L))
  bins <- c("zero", "1_10", "11_20", "21_30", "31_40", "41_50")
  good <- list(
    numeric_bin_counts = expand.grid(
      occurrence_id = ids,
      bin_id = bins,
      stringsAsFactors = FALSE
    ),
    profiled_occurrences = data.frame(
      occurrence_id = ids,
      stringsAsFactors = FALSE
    ),
    round_denominators = data.frame(
      round_id = rounds,
      stringsAsFactors = FALSE
    ),
    issues = .empty_issues()
  )
  expect_invisible(.validate_ct_profile_repair(
    good, ids, rounds
  ))

  partial <- good
  partial$numeric_bin_counts <- partial$numeric_bin_counts[-1L, , drop = FALSE]
  expect_error(
    .validate_ct_profile_repair(
      partial, ids, rounds
    ),
    "did not return all six bands"
  )

  outside <- good
  outside$issues <- .issue(
    "warning", "profile", "value_outside_safe_bins",
    "Outside fixed Ct/Cp support.", "react1.r01",
    variable = "CT_VALUE1", affected_count = 10L
  )
  expect_error(
    .validate_ct_profile_repair(
      outside, ids, rounds
    ),
    "outside 0-50"
  )
})

test_that("approved v5 requires an exact rc14 dictionary match", {
  profile <- react_synthetic_profile(refresh = TRUE)
  metadata <- stats::setNames(profile$metadata$value, profile$metadata$key)
  expect_identical(
    unname(metadata[["dictionary_compatibility"]]),
    "exact"
  )
  expect_s3_class(react_synthetic(profile, n_per_round = 1L), "react_synthetic_source")

  mismatched <- profile
  mismatched$metadata$value[
    mismatched$metadata$key == "dictionary_manifest_sha256"
  ] <- "03a2fb41a02becbe292663934e6ed436a85335b93d5004118a82ea9e4460a846"
  expect_error(
    react_synthetic(mismatched, n_per_round = 1L),
    "dictionary hashes do not match"
  )
})

test_that("the enclave bundle contains the targeted Ct/Cp script", {
  path <- system.file(
    "enclave", "reprofile_ct_distributions.R", package = "reactextract"
  )
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_true(file.exists(path))
  expect_match(text, "bin_spec_id == \"ct_value_v2\"", fixed = TRUE)
  expect_match(text, "nrow(ct_specs) != 50L", fixed = TRUE)
  expect_match(text, "react_profile_source", fixed = TRUE)
  expect_match(text, "react_repair_profile", fixed = TRUE)
})

test_that("the enclave bundle contains the combined v5 profile script", {
  path <- system.file(
    "enclave", "build_synthetic_profile_v5.R", package = "reactextract"
  )
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_true(file.exists(path))
  expect_match(text, "react_profile_dependencies_source", fixed = TRUE)
  expect_match(text, "requires_enclave_disclosure_review", fixed = TRUE)
})
