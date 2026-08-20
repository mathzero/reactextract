test_that("RDS writer preserves every output mode", {
  source <- react_files(list(
    react1.r01 = data.frame(U_PASSCODE = "p1", HEALTHA05 = 1L)
  ))
  for (mode in c("wide", "long", "both")) {
    result <- react_extract(
      source,
      families = "health/preexisting-conditions",
      rounds = "react1.r01",
      concepts = "health.preexisting.overweight",
      output = mode
    )
    path <- withr::local_tempfile(fileext = ".rds")
    expect_silent(react_write(result, path, "rds"))
    expect_identical(readRDS(path), result)
  }
})

test_that("Parquet writer creates only tables present in each output mode", {
  skip_if_not_installed("arrow")
  source <- react_files(list(
    react1.r01 = data.frame(U_PASSCODE = "p1", HEALTHA05 = 1L)
  ))
  expected <- list(
    wide = c("data", "raw_data", "observations", "column_dictionary", "issues", "manifest"),
    long = c("observations", "raw_values", "harmonised_values", "column_dictionary", "issues", "manifest"),
    both = c("data", "raw_data", "observations", "raw_values", "harmonised_values", "column_dictionary", "issues", "manifest")
  )

  for (mode in names(expected)) {
    result <- react_extract(
      source,
      concepts = "health.preexisting.overweight",
      rounds = "react1.r01",
      output = mode,
      progress = FALSE
    )
    path <- withr::local_tempdir()
    expect_silent(react_write(result, path, "parquet"))
    written <- sub("[.]parquet$", "", list.files(path, pattern = "[.]parquet$"))
    expect_setequal(written, expected[[mode]])
  }
})

test_that("writer rejects incomplete output representations", {
  source <- react_files(list(
    react1.r01 = data.frame(U_PASSCODE = "p1", HEALTHA05 = 1L)
  ))
  result <- react_extract(
    source,
    concepts = "health.preexisting.overweight",
    rounds = "react1.r01",
    output = "both",
    progress = FALSE
  )
  result$raw_data <- NULL
  expect_error(
    react_write(result, withr::local_tempfile(fileext = ".rds"), "rds"),
    "returned by `react_extract"
  )
})
