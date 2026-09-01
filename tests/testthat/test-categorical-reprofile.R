test_that("rc10 retains exactly the approved categorical corrections", {
  dictionary <- react_dictionary()
  specs <- dictionary$synthetic_profile_specs
  corrected <- specs[
    specs$support_source == "inferred_public_response_domain",
    , drop = FALSE
  ]
  expect_equal(nrow(corrected), 168L)
  expect_true(all(corrected$profile_kind == "categorical"))
  expect_true(all(corrected$generation_action == "empirical"))
  expect_true(all(corrected$bin_spec_id == ""))

  occurrence <- dictionary$occurrences[
    dictionary$occurrences$round_id == "react1.r03" &
      dictionary$occurrences$variable == "AGE_GROUP",
    , drop = FALSE
  ]
  spec <- corrected[
    corrected$occurrence_id == occurrence$occurrence_id,
    , drop = FALSE
  ]
  options <- .profile_response_options(occurrence, spec, dictionary)
  expect_identical(options$return_value, as.character(1:10))
  expect_identical(
    options$display_value,
    c(
      "5 to 12", "13 to 17", "18 to 24", "25 to 34", "35 to 44",
      "45 to 54", "55 to 64", "65 to 74", "75 to 84", "85+"
    )
  )
})

test_that("approved v5 profile retains the corrected category policy", {
  archive <- system.file(
    "extdata", "synthetic-profile.tar.gz", package = "reactextract"
  )
  path <- tempfile("approved-profile-")
  dir.create(path)
  untar(archive, exdir = path)
  profile <- react_read_profile(path)
  metadata <- stats::setNames(profile$metadata$value, profile$metadata$key)
  expect_identical(unname(metadata[["dictionary_release"]]), "v1.0.0-rc14")
  expect_identical(
    unname(metadata[["profile_version"]]),
    "enclave-profile-v5-outcome-dependencies-lab-results-corrected"
  )
  corrected_ids <- react_dictionary()$synthetic_profile_specs$occurrence_id[
    react_dictionary()$synthetic_profile_specs$support_source ==
      "inferred_public_response_domain"
  ]
  expect_false(any(profile$numeric_bin_counts$occurrence_id %in% corrected_ids))
  expect_setequal(
    unique(profile$categorical_counts$occurrence_id[
      profile$categorical_counts$occurrence_id %in% corrected_ids
    ]),
    corrected_ids
  )
})

test_that("development generation uses inferred AGE_GROUP categories", {
  profile <- react_synthetic_profile(development = TRUE)
  source <- react_synthetic(
    profile = profile,
    n_per_round = c(REACT1_R03 = 100L),
    seed = 301L
  )
  result <- react_extract(
    source,
    rounds = "REACT1_R03",
    concepts = "people_households.demographics.age_band",
    progress = FALSE
  )
  values <- result$raw_data$AGE_GROUP
  expect_true(all(as.character(values[!is.na(values)]) %in% as.character(1:10)))
})

test_that("administrative-only local options do not hide an inferred domain", {
  dictionary <- react_dictionary()
  occurrence <- dictionary$occurrences[
    dictionary$occurrences$round_id == "react1.r07" &
      dictionary$occurrences$variable == "COVIDSYM2_1",
    , drop = FALSE
  ]
  spec <- dictionary$synthetic_profile_specs[
    dictionary$synthetic_profile_specs$occurrence_id == occurrence$occurrence_id,
    , drop = FALSE
  ]
  options <- .profile_response_options(occurrence, spec, dictionary)
  expect_setequal(options$return_value, c("-92", "-91", "-77", "0", "1"))
  expect_identical(
    options$display_value[options$return_value == "1"],
    "Yes - Coughing"
  )
})

test_that("approved v5 records its 50 Ct/Cp and 37 result repairs", {
  path <- tempfile("approved-v5-")
  dir.create(path)
  archive <- system.file(
    "extdata", "synthetic-profile.tar.gz", package = "reactextract"
  )
  untar(archive, exdir = path)
  profile <- react_read_profile(path)
  expect_equal(nrow(profile$profiled_occurrences), 87L)
  expect_equal(sum(profile$profiled_occurrences$profile_kind == "continuous"), 50L)
  expect_equal(sum(profile$profiled_occurrences$profile_kind == "categorical"), 37L)
  expect_true(all(
    profile$profiled_occurrences$status[
      profile$profiled_occurrences$profile_kind == "continuous"
    ] == "requested"
  ))
  expect_true(all(
    profile$profiled_occurrences$status[
      profile$profiled_occurrences$profile_kind == "categorical"
    ] == "corrected_occurrence_specific_lab_result_support"
  ))
})
