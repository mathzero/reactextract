test_that("continuous Long COVID duration is binned to reviewed categories", {
  round <- data.frame(
    U_PASSCODE = paste0("p", 1:8),
    LONGCOVIDB_1_1 = c(0, 27, 28, 60, 61, 92, 183, 0),
    LONGCOVIDB_2_1 = c(0, 0, 0, 0, 0, 0, 0, 4),
    check.names = FALSE
  )
  result <- react_extract(
    react_files(list(react1.r05 = round)),
    families = "health/persistent-symptoms",
    rounds = "react1.r05",
    concepts = "health.persistent_symptoms.duration.loss_or_change_smell",
    output = "both"
  )
  output_column <- unique(result$harmonised_values$output_column)

  expect_identical(
    result$data[[output_column]],
    c(
      "Less than four weeks", "Less than four weeks",
      "Four weeks up to two months", "Four weeks up to two months",
      "Two months up to three months", "Three months up to six months",
      "More than six months", "Four weeks up to two months"
    )
  )
  expect_equal(nrow(result$harmonised_values), 8L)
  expect_true(all(
    result$harmonised_values$transform_id ==
      "harmonisation.long_covid.duration.01"
  ))
  expect_true(all(c("LONGCOVIDB_1_1", "LONGCOVIDB_2_1") %in%
    unique(result$raw_values$raw_variable)))
})

test_that("later categorical values take the reviewed target coding", {
  round <- data.frame(
    U_PASSCODE = paste0("p", 1:8),
    LONGCOVIDB2_1 = c(1, 2, 3, 4, 5, 6, 7, -91),
    check.names = FALSE
  )
  result <- react_extract(
    react_files(list(react2.r06 = round)),
    families = "health/persistent-symptoms",
    rounds = "react2.r06",
    concepts = "health.persistent_symptoms.duration.loss_or_change_smell",
    output = "both"
  )
  output_column <- unique(result$harmonised_values$output_column)

  expect_identical(
    result$data[[output_column]],
    c(
      "Less than four weeks", "Four weeks up to two months",
      "Two months up to three months", "Three months up to six months",
      "More than six months", "Cannot give an estimate",
      "Prefer not to say", NA_character_
    )
  )
  expect_identical(result$harmonised_values$missing_reason[[8]], "not_applicable")
})

test_that("conflicting positive DAYS and WEEKS stay unresolved", {
  round <- data.frame(
    U_PASSCODE = "p1",
    LONGCOVIDB_1_1 = 14,
    LONGCOVIDB_2_1 = 5,
    check.names = FALSE
  )
  result <- react_extract(
    react_files(list(react1.r05 = round)),
    families = "health/persistent-symptoms",
    rounds = "react1.r05",
    concepts = "health.persistent_symptoms.duration.loss_or_change_smell",
    output = "both"
  )
  output_column <- unique(result$harmonised_values$output_column)

  expect_true(is.na(result$data[[output_column]]))
  expect_identical(
    result$harmonised_values$missing_reason,
    "conflicting_duration_inputs"
  )
  expect_true(any(result$issues$code == "conflicting_duration_inputs"))
})

test_that("compound duration uses the longest reviewed symptom component", {
  round <- data.frame(
    U_PASSCODE = c("p1", "p2"),
    LONGCOVIDB_1_18 = c(7, 14),
    LONGCOVIDB_2_18 = c(0, 0),
    LONGCOVIDB_1_19 = c(70, 21),
    LONGCOVIDB_2_19 = c(0, 0),
    check.names = FALSE
  )
  result <- react_extract(
    react_files(list(react1.r05 = round)),
    families = "health/persistent-symptoms",
    rounds = "react1.r05",
    concepts = "health.persistent_symptoms.duration.chest_pain_or_tightness",
    output = "both"
  )
  output_column <- unique(result$harmonised_values$output_column)

  expect_identical(
    result$data[[output_column]],
    c("Two months up to three months", "Less than four weeks")
  )
})

test_that("shifted REACT-2 fields use the same reviewed concepts", {
  round <- data.frame(
    U_PASSCODE = c("p1", "p2"),
    LONGCOVIDB2_21 = c(1, 5),
    LONGCOVIDB2_22 = c(2, 4),
    check.names = FALSE
  )
  concepts <- c(
    "health.persistent_symptoms.duration.leg_swelling",
    "health.persistent_symptoms.duration.difficulty_sleeping"
  )
  result <- react_extract(
    react_files(list(react2.r06 = round)),
    families = "health/persistent-symptoms",
    rounds = "react2.r06",
    concepts = concepts,
    output = "both"
  )

  expect_setequal(unique(result$harmonised_values$concept_id), concepts)
  leg <- result$harmonised_values[
    result$harmonised_values$concept_id == concepts[[1]], , drop = FALSE
  ]
  sleep <- result$harmonised_values[
    result$harmonised_values$concept_id == concepts[[2]], , drop = FALSE
  ]
  expect_true(all(leg$source_raw_variable == "LONGCOVIDB2_21"))
  expect_true(all(sleep$source_raw_variable == "LONGCOVIDB2_22"))
  expect_identical(leg$value_character, c("Less than four weeks", "More than six months"))
  expect_identical(sleep$value_character, c("Four weeks up to two months", "Three months up to six months"))
})

test_that("grouped outputs use topic-prefixed concepts and retain source provenance", {
  decisions <- react_harmonisation_decisions()
  by_decision <- split(decisions$inputs, decisions$inputs$decision_id)
  for (i in seq_len(nrow(decisions$groups))) {
    group <- decisions$groups[i, , drop = FALSE]
    source_names <- unique(by_decision[[group$decision_id]]$source_variable)
    expect_identical(group$output_column, group$concept_id)
    expect_true(startsWith(group$output_column, "health.persistent_symptoms."))
    expect_true(length(source_names) >= 1L)
  }
})
