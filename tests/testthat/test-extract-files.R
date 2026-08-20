test_that("file extraction returns long typed tables and safe identity links", {
  source <- react_files(preexisting_rounds(), crosswalk = fixture_crosswalk())
  result <- react_extract(
    source,
    families = "health/preexisting-conditions",
    rounds = c("REACT1_R01", "REACT2_S5_R06")
  )

  expect_equal(nrow(result$observations), 8L)
  expect_setequal(result$observations$SUBJECT_ID, paste0("s", 1:4))
  expect_true(all(result$observations$total_visits == 2L))
  expect_equal(nrow(result$raw_values), 12L)
  expect_setequal(
    unique(result$raw_values$raw_variable),
    c("HEALTHA05", "HEALTHA06", "HEALTHA_05")
  )
  expect_setequal(
    unique(result$harmonised_values$concept_id),
    c(
      "health.preexisting.overweight",
      "health.preexisting.stroke"
    )
  )
  expect_true(all(
    result$harmonised_values$source_raw_variable == "HEALTHA05" |
      result$harmonised_values$source_raw_variable == "HEALTHA06" |
      result$harmonised_values$source_raw_variable == "HEALTHA_05"
  ))
  expect_true(all(
    reactextract:::.typed_nonmissing_count(result$harmonised_values) == 1L
  ))
  expect_equal(nrow(result$data), nrow(result$observations))
  expect_equal(nrow(result$raw_data), nrow(result$observations))
  expect_true(is.logical(result$data$health.preexisting.overweight))
  expect_true(all(c("HEALTHA05", "HEALTHA06", "HEALTHA_05") %in% names(result$raw_data)))
  expect_identical(
    result$raw_data$HEALTHA05[result$raw_data$round_id == "react1.r01"],
    rep(c(0L, 1L), length.out = 4L)
  )
})

test_that("simple output retains requested rounds where a concept was not asked", {
  round <- data.frame(U_PASSCODE = c("p1", "p2"), check.names = FALSE)
  result <- react_extract(
    react_files(list(react1.r05 = round)),
    families = "health/preexisting-conditions",
    rounds = "react1.r05",
    concepts = "health.preexisting.overweight"
  )

  expect_equal(nrow(result$data), 2L)
  expect_true(all(is.na(result$data$health.preexisting.overweight)))
  expect_equal(nrow(result$raw_values), 0L)
  expect_equal(nrow(result$harmonised_values), 0L)
})

test_that("overweight pilot has one clean concept and exact raw-name columns", {
  key <- data.frame(U_PASSCODE = c("p1", "p2"), check.names = FALSE)
  rounds <- list(
    react1.r01 = data.frame(key, HEALTHA05 = c(0L, 1L), check.names = FALSE),
    react1.r10 = data.frame(key, HEALTHA_5 = c(1L, 0L), check.names = FALSE),
    react1.r19 = key,
    react2.r01 = data.frame(key, HEALTHA_05 = c(0L, 1L), check.names = FALSE),
    react2.r06 = key
  )
  result <- react_extract(
    react_files(rounds, crosswalk = fixture_crosswalk(2L)),
    families = "health/preexisting-conditions",
    concepts = "health.preexisting.overweight",
    rounds = names(rounds)
  )

  expect_equal(nrow(result$data), 10L)
  expect_true(is.logical(result$data$health.preexisting.overweight))
  expect_equal(sum(is.na(result$data$health.preexisting.overweight)), 6L)
  expect_setequal(
    setdiff(names(result$raw_data), names(reactextract:::.simple_identifiers(result$observations))),
    c("HEALTHA05", "HEALTHA_5", "HEALTHA_05")
  )
  expect_equal(nrow(result$harmonised_values), 4L)
})

test_that("literal NA text remains text while database missingness remains missing", {
  round <- data.frame(
    U_PASSCODE = c("NA", NA_character_, "p3"),
    check.names = FALSE
  )
  result <- react_extract(
    react_files(list(react1.r01 = round)),
    families = "consent-administration/survey-admin",
    rounds = "react1.r01",
    concepts = "consent_administration.survey_admin.participant_identifier"
  )
  values <- result$raw_values[result$raw_values$raw_variable == "U_PASSCODE", ]

  expect_identical(values$value_character[[1]], "NA")
  expect_false(values$source_is_missing[[1]])
  expect_true(values$source_is_missing[[2]])
  expect_true(is.na(values$value_character[[2]]))
  expect_equal(nrow(result$observations), 3L)
})

test_that("partial files retain successful variables and report failures", {
  round <- data.frame(U_PASSCODE = c("p1", "p2"), HEALTHA05 = c(0L, 1L))
  result <- react_extract(
    react_files(list(react1.r01 = round)),
    families = "health/preexisting-conditions",
    rounds = "react1.r01"
  )

  expect_equal(nrow(result$observations), 2L)
  expect_equal(nrow(result$raw_values[result$raw_values$raw_variable == "HEALTHA05", ]), 2L)
  expect_true(any(result$issues$code == "field_unavailable"))
  expect_identical(result$manifest$value[result$manifest$key == "completeness"], "partial")
})

test_that("crosswalk conflicts never multiply observations", {
  round <- data.frame(U_PASSCODE = c("p1", "p2"), HEALTHA05 = c(0L, 1L))
  crosswalk <- data.frame(
    REACT_ID = c("p1", "p1", "p2"),
    SUBJECT_ID = c("s1", "conflict", "s2")
  )
  result <- react_extract(
    react_files(list(react1.r01 = round), crosswalk = crosswalk),
    families = "health/preexisting-conditions",
    rounds = "react1.r01"
  )

  expect_equal(nrow(result$observations), 2L)
  expect_true(is.na(result$observations$SUBJECT_ID[result$observations$U_PASSCODE == "p1"]))
  expect_identical(result$observations$SUBJECT_ID[result$observations$U_PASSCODE == "p2"], "s2")
  expect_true(any(result$issues$code == "crosswalk_conflict"))
})

test_that("unrecognised recode values are retained and flagged", {
  round <- data.frame(U_PASSCODE = "p1", HEALTHA05 = -999L)
  result <- react_extract(
    react_files(list(react1.r01 = round)),
    families = "health/preexisting-conditions",
    rounds = "react1.r01",
    concepts = "health.preexisting.overweight"
  )
  value <- result$harmonised_values

  expect_identical(value$missing_reason, "unrecognised_raw_retained")
  expect_identical(value$value_character, "-999")
  expect_true(any(result$issues$code == "unrecognised_raw_retained"))
})

test_that("repeated extraction has equivalent data and manifests", {
  source <- react_files(preexisting_rounds(3), crosswalk = fixture_crosswalk(3))
  first <- react_extract(source, "health/preexisting-conditions", "react1.r01")
  second <- react_extract(source, "health/preexisting-conditions", "react1.r01")
  expect_identical(first[names(first) != "manifest"], second[names(second) != "manifest"])
  stable_manifest <- function(x) x[!startsWith(x$key, "timing_"), , drop = FALSE]
  expect_identical(stable_manifest(first$manifest), stable_manifest(second$manifest))
})
