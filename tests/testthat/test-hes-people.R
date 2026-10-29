test_that("people are standardised for synthetic HES", {
  p <- .hes_people(data.frame(
    SUBJECT_ID = c("a", "b", "b", "c", "d"),
    sex = c("Male", "F", "F", "2", NA),
    age = c(30, 40, 40, 50, 60),
    ethnicity = c("Pakistani", "Prefer not to say", "Prefer not to say", "N", "Unknown text"),
    stringsAsFactors = FALSE
  ))
  expect_identical(p$SUBJECT_ID, c("a", "b", "c", "d"))
  expect_identical(p$sex, c("1", "2", "2", NA))
  expect_identical(p$ethnicity, c("J", "Z", "N", NA))
  expect_true(all(p$age_date == as.Date("2021-04-01")))
  expect_error(.hes_people(data.frame(SUBJECT_ID = 1, age = 3)), "sex")
})

test_that("synthetic HES joins to synthetic participants on SUBJECT_ID", {
  source <- react_synthetic(n_per_round = c(REACT1_R08 = 60L, REACT2_S5_R05 = 60L), seed = 2L)
  result <- react_extract(source, families = "people-households/demographics", progress = FALSE)
  hes <- react_hes(source, result)
  expect_true(all(hes$NHSE_HES_ADMITTED_PATIENTS$SUBJECT_ID %in% result$data$SUBJECT_ID))
  expect_identical(hes, react_hes(source, result, seed = 2L))

  # without the demographics family, age and sex come from the same source
  lab <- react_extract(source, families = "infection-measurement/laboratory", progress = FALSE)
  expect_identical(react_hes(source, lab), hes)

  kept <- react_hes(source, result, columns = c("ADMIDATE", "DIAG_4_01", "APPTDATE"))
  expect_named(kept$NHSE_HES_ADMITTED_PATIENTS, c("SUBJECT_ID", "ADMIDATE", "DIAG_4_01"))
  expect_named(kept$NHSE_HES_OUTPATIENTS, c("SUBJECT_ID", "APPTDATE", "DIAG_4_01"))
})

test_that("an Oracle source queries the linked tables in batches of SUBJECT_IDs", {
  calls <- character()
  query <- function(connection, sql) {
    calls <<- c(calls, sql)
    ids <- as.numeric(strsplit(sub("^.*IN \\((.*)\\)$", "\\1", sql), ", ", fixed = TRUE)[[1]])
    data.frame(SUBJECT_ID = ids[1:2], ADMIDATE = as.POSIXct("2020-01-01", tz = "UTC"))
  }
  source <- .new_oracle_source(
    connection = structure(list(), class = "fake_connection"),
    registry = react_dictionary()$source_registry,
    batch_size = 200L,
    query_fn = query
  )
  hes <- react_hes(source, data.frame(SUBJECT_ID = c(1:2500, 7L)), tables = "apc",
                   columns = c("DIAG_4_01", "ADMIDATE"))
  expect_length(calls, 3L)
  expect_match(calls[1], "^SELECT SUBJECT_ID, ADMIDATE, DIAG_4_01 FROM NHSE_HES_ADMITTED_PATIENTS WHERE SUBJECT_ID IN \\(1, 2, ")
  expect_named(hes, "NHSE_HES_ADMITTED_PATIENTS")
  expect_identical(nrow(hes$NHSE_HES_ADMITTED_PATIENTS), 6L)

  calls <- character()
  react_hes(source, 1:3)
  expect_match(calls, "^SELECT \\* FROM NHSE_HES_(ADMITTED_PATIENTS|OUTPATIENTS) WHERE SUBJECT_ID IN \\(1, 2, 3\\)$")

  expect_error(react_hes(source, c("1; DROP TABLE x")), "whole numbers")
  expect_error(react_hes(source, 1:3, columns = "NOT_A_COLUMN"), "Unknown HES column")
})

test_that("round-file sources cannot supply HES", {
  source <- react_files(list(REACT1_R01 = data.frame(U_PASSCODE = "p1")))
  expect_error(react_hes(source, 1), "not round files")
})
