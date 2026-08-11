test_that("pinned dictionary verifies and exposes the fixed contract", {
  dictionary <- react_dictionary(refresh = TRUE)
  version <- react_dictionary_version()

  expect_identical(version$dictionary_release, "v1.0.0-rc2")
  expect_identical(
    version$manifest_sha256,
    "87102e6aa90f440e079ee4f966140263a0998874af7f09264ae7e19fa8b6d29b"
  )
  expect_equal(nrow(dictionary$rounds), 25L)
  expect_equal(nrow(dictionary$occurrences), 15093L)
  expect_equal(nrow(dictionary$occurrence_exclusions), 14L)
  expect_equal(nrow(dictionary$concepts), 478L)
  expect_equal(nrow(dictionary$mappings), 436L)
  expect_equal(nrow(dictionary$oracle_unavailable_fields), 4L)
  expect_true(all(dictionary$occurrences$review_state == "approved"))
  expect_true(all(dictionary$mappings$review_state == "approved"))
  expect_false(any(
    dictionary$occurrence_exclusions$occurrence_id %in%
      dictionary$occurrences$occurrence_id
  ))
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
