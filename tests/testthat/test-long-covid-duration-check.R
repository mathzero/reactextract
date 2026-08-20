load_duration_check <- function() {
  environment <- new.env(parent = globalenv())
  path <- testthat::test_path(
    "..", "..", "inst", "enclave", "check_long_covid_durations.R"
  )
  if (!file.exists(path)) {
    path <- system.file(
      "enclave", "check_long_covid_durations.R", package = "reactextract"
    )
  }
  sys.source(path, envir = environment)
  environment
}

test_that("duration boundaries reproduce the reviewed categories", {
  environment <- load_duration_check()
  expect_identical(
    as.character(environment$duration_band(c(0, 27, 28, 60, 61, 91, 92, 182, 183))),
    c(
      "1 Less than four weeks", "1 Less than four weeks",
      "2 Four weeks up to two months", "2 Four weeks up to two months",
      "3 Two months up to three months", "3 Two months up to three months",
      "4 Three months up to six months", "4 Three months up to six months",
      "5 More than six months"
    )
  )
})

test_that("pair summaries distinguish joint and alternative completion", {
  environment <- load_duration_check()
  summary <- environment$pair_summary(
    "react1.r05", "VIEW", 7, "1", "concept", "question",
    "LONGCOVIDB_1_1", "LONGCOVIDB_2_1", "LONGCOVIDB2_1",
    c(14, 0, NA, 3, NA, -91),
    c(0, 4, 8, 2, NA, -91)
  )

  expect_equal(summary$source_row_count, 7)
  expect_equal(summary$either_field_nonmissing_count, 5)
  expect_equal(summary$neither_field_nonmissing_count, 2)
  expect_equal(summary$both_fields_nonmissing_count, 4)
  expect_equal(summary$both_valid_count, 3)
  expect_equal(summary$both_positive_count, 1)
  expect_equal(summary$day_positive_week_zero_count, 1)
  expect_equal(summary$day_zero_week_positive_count, 1)
})

test_that("categorical assessment identifies continuous-looking values", {
  environment <- load_duration_check()
  expect_identical(
    environment$assess_categorical_codes(c(-91, 1, 2, 7, NA)),
    "matches_expected_1_to_7_categories"
  )
  expect_identical(
    environment$assess_categorical_codes(c(-91, 12, 30, NA)),
    "does_not_look_like_expected_categories"
  )
})

test_that("field frequencies include database nulls from the whole round", {
  environment <- load_duration_check()
  counts <- environment$value_counts(c(1, 1, NA), total_rows = 10)
  expect_equal(counts$count[counts$raw_value == "1"], 2)
  expect_equal(counts$count[counts$raw_value == "<DATABASE NULL>"], 8)

  empty_counts <- environment$value_counts(c(NA, NA), total_rows = 10)
  expect_equal(nrow(empty_counts), 1)
  expect_equal(empty_counts$count, 10)
})
