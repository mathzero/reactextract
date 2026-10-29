test_that("react_short_names drops topic prefixes and keeps identifiers", {
  result <- react_extract(
    react_synthetic(n_per_round = c(REACT1_R10 = 15L), seed = 21L),
    families = "people-households/demographics",
    rounds = "REACT1_R10",
    progress = FALSE
  )
  short <- react_short_names(result)
  expect_identical(short, react_short_names(result$data))
  expect_identical(unname(as.list(short)), unname(as.list(result$data)))
  expect_identical(names(short)[1:4], c("observation_id", "study_id", "round_id", "survey_id"))
  expect_true(all(c("gender", "age_U_AGE", "age_band_AGE_GROUP") %in% names(short)))
  expect_false(any(grepl("people_households.", names(short), fixed = TRUE)))
  expect_false(anyDuplicated(names(short)) > 0L)
})

test_that("react_short_names keeps original names when shortening would clash", {
  data <- data.frame(
    observation_id = "a",
    health.acute_symptoms.cough = 1,
    cough = 2,
    people_households.demographics.gender = "Female",
    estbinres = 0L,
    check.names = FALSE
  )
  expect_identical(
    names(react_short_names(data)),
    c("observation_id", "health.acute_symptoms.cough", "cough", "gender", "estbinres")
  )
  expect_error(react_short_names(1:3), "`x` must be")
})
