test_that("pinned dictionary verifies and exposes the fixed contract", {
  dictionary <- react_dictionary(refresh = TRUE)
  version <- react_dictionary_version()

  expect_identical(version$dictionary_release, "v1.0.0-rc1")
  expect_identical(
    version$manifest_sha256,
    "7f6731181c136eaa555041a80c55077446ecfaa49f459b7fb105ba1fc4ea4ad9"
  )
  expect_equal(nrow(dictionary$rounds), 25L)
  expect_equal(nrow(dictionary$occurrences), 15093L)
  expect_equal(nrow(dictionary$occurrence_exclusions), 14L)
  expect_equal(nrow(dictionary$concepts), 478L)
  expect_equal(nrow(dictionary$mappings), 436L)
  expect_true(all(dictionary$occurrences$review_state == "approved"))
  expect_true(all(dictionary$mappings$review_state == "approved"))
  expect_false(any(
    dictionary$occurrence_exclusions$occurrence_id %in%
      dictionary$occurrences$occurrence_id
  ))
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
