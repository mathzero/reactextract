derived_guide_path <- function() {
  path <- testthat::test_path("..", "..", "inst", "DERIVED_VARIABLES.md")
  if (!file.exists(path)) {
    path <- system.file("DERIVED_VARIABLES.md", package = "reactextract")
  }
  path
}

derived_guide_chunks <- function() {
  lines <- readLines(derived_guide_path(), warn = FALSE)
  chunks <- list()
  in_chunk <- FALSE
  current <- character()

  for (line in lines) {
    if (!in_chunk && identical(line, "```r")) {
      in_chunk <- TRUE
      current <- character()
    } else if (in_chunk && identical(line, "```")) {
      chunks[[length(chunks) + 1L]] <- paste(current, collapse = "\n")
      in_chunk <- FALSE
    } else if (in_chunk) {
      current <- c(current, line)
    }
  }

  chunks
}

test_that("all R snippets in the derived-variable guide parse", {
  path <- derived_guide_path()
  expect_true(file.exists(path))
  chunks <- derived_guide_chunks()
  expect_gte(length(chunks), 6L)
  expect_silent(lapply(
    chunks,
    function(chunk) parse(text = chunk, keep.source = FALSE)
  ))
})

test_that("estbinres derivation preserves historical round rules", {
  fixture <- data.frame(
    survey_id = c(
      rep("REACT1_R01", 3), rep("REACT1_R02", 4),
      rep("REACT1_R05", 2), rep("REACT1_R08", 3),
      rep("REACT1_R11", 2), rep("REACT1_R13", 2),
      "REACT2_S5_R01"
    ),
    RESULT = c(
      rep("Detected", 7), NA, NA,
      "Detected", "Detected", "Not Detected",
      "negative", "Negative", "ambiguous", "Detected", NA
    ),
    LAB = c("Other", "Eurofin", NA, rep(NA_character_, 14)),
    CT_VALUE1 = c(NA, 36, 36, 40, 36, 37, 36, rep(NA_real_, 10)),
    CT_VALUE2 = c(NA, 0, 0, 40, 0, 0, NA, rep(NA_real_, 10)),
    FINALRESULT = c(
      rep(NA_character_, 3), "Rejected", rep(NA_character_, 3),
      "Detected", "Void", rep(NA_character_, 8)
    ),
    NGENE_CTVALUE = c(
      rep(NA_real_, 9), 40, 36, 37, 0, 0, 25, 25, NA
    ),
    EGENE_CTVALUE = c(
      rep(NA_real_, 9), 40, 0, 0, 0, 0, 25, 25, NA
    ),
    stringsAsFactors = FALSE
  )

  expect_identical(
    react_derive_estbinres(fixture),
    c(1L, 1L, NA_integer_, 1L, 1L, 0L, NA_integer_,
      1L, NA_integer_, 1L, 1L, 0L,
      0L, NA_integer_, NA_integer_, 1L, NA_integer_)
  )
})

test_that("simple derived-variable functions run on mixed rounds", {
  fixture <- data.frame(
    survey_id = c("REACT1_R01", "REACT1_R14", "REACT1_R19", "REACT2_S5_R01"),
    IMD_DECILE = c(1, 10, -92, 3),
    U_AGE = c(12, 65, 85, 44),
    NADULTS = c(2, 1, 2, 2),
    NADULTS1 = c(NA, 1, NA, NA),
    NCHILD = c(1, 2, 0, 2),
    NCHILD1 = c(NA, 2, NA, NA),
    WEIGHT_KG = c(70, 70, -92, 80),
    WEIGHT_S = c(NA, NA, 11, NA),
    WEIGHT_P = c(NA, NA, 0, NA),
    HEIGHT_CM = c(175, 175, -92, 180),
    HEIGHT_FEET = c(NA, NA, 5, NA),
    HEIGHT_INCHES = c(NA, NA, 10, NA)
  )

  expect_identical(
    react_derive_imd_quintile(fixture),
    c(1L, 5L, NA_integer_, NA_integer_)
  )
  expect_identical(
    as.character(react_derive_age_group(fixture)),
    c("5-12", "65+", "65+", NA_character_)
  )
  expect_warning(
    household <- react_derive_household_size(fixture),
    "both routed household-count alternatives",
    fixed = TRUE
  )
  expect_identical(household, c(3L, 6L, 2L, NA_integer_))

  bmi <- react_derive_bmi(fixture)
  expect_true(is.na(bmi[[1L]]))
  expect_equal(bmi[[2L]], 22.86)
  expect_equal(bmi[[3L]], 22.14)
  expect_true(is.na(bmi[[4L]]))
  expect_identical(
    as.character(react_bmi_category(bmi, fixture$U_AGE)),
    c(NA_character_, "Normal weight", "Normal weight", NA_character_)
  )
})

test_that("IgG positivity follows the reviewed NEWRESULT coding", {
  fixture <- data.frame(
    survey_id = c(rep("REACT2_S5_R05", 8), "REACT1_R08"),
    NEWRESULT = c(0, 1, 2, 3, 4, 7, -77, NA, 2)
  )
  expect_identical(
    react_derive_igg(fixture),
    c(0L, 0L, 1L, 1L, NA, NA, NA, NA, NA)
  )
  expect_identical(react_derive_igg(fixture[9, , drop = FALSE]), NA_integer_)
  expect_error(
    react_derive_igg(data.frame(survey_id = "REACT2_S5_R01")),
    "needs raw column NEWRESULT"
  )
})

test_that("derived-variable functions accept an extraction result", {
  result <- react_extract(
    react_synthetic(n_per_round = c(REACT1_R17 = 300L, REACT2_S5_R05 = 300L), seed = 11L),
    families = c(
      "infection-measurement/laboratory", "infection-measurement/testing",
      "people-households/demographics"
    ),
    rounds = c("REACT1_R17", "REACT2_S5_R05"),
    progress = FALSE
  )
  react1 <- result$raw_data$survey_id == "REACT1_R17"

  estbinres <- react_derive_estbinres(result)
  expect_identical(estbinres, react_derive_estbinres(result$raw_data))
  expect_length(estbinres, nrow(result$data))
  expect_true(all(is.na(estbinres[!react1])))
  expect_true(all(estbinres[react1] %in% c(0L, 1L, NA)))
  expect_true(any(estbinres[react1] %in% 0L))

  igg <- react_derive_igg(result)
  expect_true(all(is.na(igg[react1])))
  expect_true(any(igg[!react1] %in% 1L))

  expect_false(anyNA(react_derive_age_group(result)[react1]))
  expect_error(react_derive_estbinres(list(a = 1)), "raw_data")
})
