test_that("pinned dictionary verifies and exposes the fixed contract", {
  dictionary <- react_dictionary(refresh = TRUE)
  version <- react_dictionary_version()

  expect_identical(version$dictionary_release, "v1.0.0-rc14")
  expect_identical(
    version$manifest_sha256,
    "28d03054e4b284cd44a040cf473991c441184739e84a7f6235392b1142a79236"
  )
  expect_equal(nrow(dictionary$rounds), 25L)
  expect_equal(nrow(dictionary$occurrences), 15093L)
  expect_equal(nrow(dictionary$occurrence_exclusions), 14L)
  expect_equal(nrow(dictionary$concepts), 478L)
  expect_equal(nrow(dictionary$mappings), 436L)
  expect_equal(nrow(dictionary$oracle_unavailable_fields), 4L)
  expect_equal(nrow(dictionary$concept_value_lookup), 67452L)
  expect_equal(nrow(dictionary$concept_output_columns), 15087L)
  duplicates <- duplicated(
    dictionary$concept_output_columns[c("output_column", "round_id")]
  )
  expect_true(all(
    dictionary$concept_output_columns$output_strategy[duplicates] == "reviewed_group"
  ))
  expect_equal(nrow(dictionary$harmonisation_groups), 31L)
  expect_equal(nrow(dictionary$harmonisation_inputs), 543L)
  expect_equal(nrow(dictionary$instruments), 50L)
  expect_equal(nrow(dictionary$synthetic_profile_specs), 15093L)
  expect_equal(nrow(dictionary$synthetic_outcomes), 2L)
  expect_equal(nrow(dictionary$synthetic_dependencies), 22L)
  expect_equal(nrow(dictionary$synthetic_profile_overrides), 37L)
  expect_equal(nrow(dictionary$routing_rules), 1989L)
  expect_true(all(dictionary$routing_rules$review_state == "approved"))
  expect_true(all(dictionary$synthetic_profile_specs$review_state == "approved"))
  expect_true(all(dictionary$safe_bins$review_state == "approved"))
  grouped <- dictionary$concept_output_columns[
    dictionary$concept_output_columns$output_strategy == "reviewed_group",
    , drop = FALSE
  ]
  components <- dictionary$concept_output_columns[
    dictionary$concept_output_columns$output_strategy == "component",
    , drop = FALSE
  ]
  expect_true(all(grouped$output_column == grouped$concept_id))
  expect_true(all(startsWith(
    components$output_column,
    paste0(components$concept_id, "__field__")
  )))
  expect_true(all(dictionary$occurrences$review_state == "approved"))
  expect_true(all(dictionary$mappings$review_state == "approved"))
  expect_false(any(
    dictionary$occurrence_exclusions$occurrence_id %in%
      dictionary$occurrences$occurrence_id
  ))
})

test_that("shared harmonisation notes and decisions are available offline", {
  notes <- react_harmonisation_notes(open = FALSE)
  expect_true(file.exists(notes))
  expect_match(
    paste(readLines(notes, warn = FALSE), collapse = "\n"),
    "LONGCOVIDB2_21"
  )

  decisions <- react_harmonisation_decisions(
    "health.persistent_symptoms.duration.loss_or_change_smell"
  )
  expect_equal(nrow(decisions$groups), 1L)
  expect_true(all(decisions$inputs$concept_id == decisions$groups$concept_id[[1]]))
  expect_true(all(c("LONGCOVIDB2_1", "LONGCOVIDB_1_1", "LONGCOVIDB_2_1") %in%
    decisions$inputs$source_variable))
})

test_that("shared synthetic methods and review contract are available offline", {
  notes <- react_synthetic_notes(open = FALSE)
  expect_true(file.exists(notes))
  expect_match(
    paste(readLines(notes, warn = FALSE), collapse = "\n"),
    "development fallback"
  )
  expect_match(
    paste(readLines(notes, warn = FALSE), collapse = "\n"),
    "ac157927e065fe70ab951f4d8b3accece5f66c4a47b1a1d615354a4bc3c99a5a",
    fixed = TRUE
  )
  dictionary <- react_dictionary()
  expect_equal(nrow(dictionary$instruments), 50L)
  expect_equal(nrow(dictionary$occurrence_routing_status), 15093L)
})

test_that("source coding expands offline coverage without claiming harmonisation", {
  concepts <- c(
    "health.acute_symptoms.cough",
    "infection_measurement.testing_history.previous_antibody_test_history"
  )
  coding <- react_concept_coding(concepts)

  expect_setequal(unique(coding$concept_id), concepts)
  expect_true(all(c(
    "round_id", "raw_field", "raw_value", "harmonised_value",
    "mapping_status"
  ) %in% names(coding)))
  expect_true(any(coding$concept_id == concepts[[1]]))
  expect_true(any(coding$concept_id == concepts[[2]]))
  expect_true(any(coding$mapping_status == "standardized_missing"))
  expect_true(any(coding$mapping_status == "source_label"))
})

test_that("reviewed Oracle exceptions come from the pinned dictionary", {
  dictionary <- react_dictionary()
  registry <- .read_confirmed_unavailable()

  expect_identical(registry, dictionary$oracle_unavailable_fields)
  expect_setequal(
    registry$variable,
    c("PREVREACT", "PREVREACTID1", "PREVREACTID2", "PREVREACTID3")
  )
})

test_that("families have stable human-readable selectors", {
  families <- react_families()
  expect_true("health/acute-symptoms" %in% families$family)
  expect_true("infection-measurement/testing-history" %in% families$family)
  expect_true("consent-administration" %in% families$family)
})

test_that("exact raw names are never normalised together", {
  occurrences <- react_dictionary()$occurrences
  names_of_interest <- occurrences$variable[
    occurrences$variable %in% c("HEALTHA05", "HEALTHA_5", "HEALTHA_05")
  ]
  expect_setequal(unique(names_of_interest), c("HEALTHA05", "HEALTHA_5", "HEALTHA_05"))

  r1 <- occurrences[
    occurrences$round_id == "react1.r01" & occurrences$variable == "HEALTHA05",
    , drop = FALSE
  ]
  r2 <- occurrences[
    occurrences$round_id == "react2.r06" & occurrences$variable == "HEALTHA_05",
    , drop = FALSE
  ]
  expect_identical(r1$primary_concept_id, "health.preexisting.overweight")
  expect_identical(r2$primary_concept_id, "health.preexisting.stroke")
})
