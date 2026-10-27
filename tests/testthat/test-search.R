test_that("react_search finds concepts by name, raw field and wording", {
  smoking <- react_search("smok")
  expect_s3_class(smoking, "react_search_result")
  expect_true("behaviours_mobility.behaviours.current_cigarette_smoking" %in% smoking$concept_id)
  current <- smoking[smoking$concept_id ==
    "behaviours_mobility.behaviours.current_cigarette_smoking", ]
  expect_match(current$fields, "SMOKENOW", fixed = TRUE)
  expect_match(current$rounds, "REACT-1 1-19", fixed = TRUE)
  expect_identical(current$family, "behaviours-mobility/behaviours")
  expect_true(all(smoking$concept_id %in% react_dictionary()$concepts$concept_id))

  age <- react_search("u_age")
  expect_identical(age$concept_id, "people_households.demographics.age")
  expect_match(age$matched_in, "field", fixed = TRUE)

  wording <- react_search("smoke cigarettes at all nowadays")
  expect_true("behaviours_mobility.behaviours.current_cigarette_smoking" %in% wording$concept_id)
})

test_that("react_search ranks concept-name matches first", {
  hits <- react_search("vaccin", study = "react2")
  expect_true(startsWith(hits$matched_in[[1L]], "concept"))
  ethnic <- react_search("ethnic")
  expect_match(
    ethnic$question[ethnic$concept_id == "people_households.demographics.ethnic_group"],
    "ethnic group", ignore.case = TRUE
  )
})

test_that("react_search filters by study and supports regular expressions", {
  expect_equal(nrow(react_search("NEWRESULT", study = "react1")), 0L)
  react2 <- react_search("NEWRESULT", study = "react2")
  expect_true("infection_measurement.testing.antibody_test_result" %in% react2$concept_id)
  expect_false(grepl("REACT-1", react2$rounds[[1L]], fixed = TRUE))

  regex <- react_search("^IMD_DECILE$", regex = TRUE)
  expect_identical(regex$concept_id, "people_households.housing.area_deprivation_decile")
  expect_equal(nrow(react_search("^IMD_DECILE$")), 0L)
})

test_that("react_search prints readably and validates its input", {
  expect_output(print(react_search("u_age")), "people_households.demographics.age")
  expect_output(print(react_search("covid"), n = 2), "Showing 2 of")
  expect_output(print(react_search("zzzz-no-match")), "No REACT concepts match")
  expect_error(react_search(""), "`pattern` must be one non-empty string")
  expect_error(react_search("age", regex = NA), "`regex` must be TRUE or FALSE")
})
