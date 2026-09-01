test_that("REACT-1 PCR outcomes use exact occurrence-specific result states", {
  dictionary <- .test_rc14_lab_dictionary()

  round_2 <- data.frame(
    RESULT = "Detected", FINALRESULT = "Rejected",
    CT_VALUE1 = 25, CT_VALUE2 = 26, stringsAsFactors = FALSE
  )
  expect_identical(
    .dependency_react1_outcome(round_2, "react1.r02", dictionary),
    "positive"
  )

  round_11 <- data.frame(
    RESULT = c("negative", "Negative", "Detected"),
    NGENE_CTVALUE = c(0, 0, 25), EGENE_CTVALUE = c(0, 0, 26),
    stringsAsFactors = FALSE
  )
  expect_identical(
    .dependency_react1_outcome(round_11, "react1.r11", dictionary),
    c("negative", "missing", "positive")
  )

  round_13 <- data.frame(
    RESULT = c("ambiguous", "Detected", "negative"),
    NGENE_CTVALUE = c(25, 25, 0), EGENE_CTVALUE = c(26, 26, 0),
    stringsAsFactors = FALSE
  )
  expect_identical(
    .dependency_react1_outcome(round_13, "react1.r13", dictionary),
    c("missing", "positive", "missing")
  )
  expect_true(.dependency_lab_result_unknown(
    "negative", "react1.r13", "RESULT", dictionary
  ))
})

test_that("synthetic PCR assignment changes only the round's outcome field", {
  dictionary <- .test_rc14_lab_dictionary()
  source <- list(seed = 504L)

  round_2 <- data.frame(
    RESULT = c("Not Detected", "Detected"),
    FINALRESULT = c("Rejected", "Rejected"),
    CT_VALUE1 = c(0, 25), CT_VALUE2 = c(0, 26),
    stringsAsFactors = FALSE
  )
  assigned_2 <- .set_dependency_outcome(
    round_2, c("positive", "negative"), "react1.r02",
    "react1_pcr_positive", source, dictionary
  )
  expect_identical(assigned_2$FINALRESULT, round_2$FINALRESULT)
  expect_identical(assigned_2$RESULT, c("Detected", "Not Detected"))

  round_5 <- data.frame(
    RESULT = c("companion-a", "companion-b"),
    FINALRESULT = c("Not Detected", "Detected"),
    stringsAsFactors = FALSE
  )
  assigned_5 <- .set_dependency_outcome(
    round_5, c("positive", "negative"), "react1.r05",
    "react1_pcr_positive", source, dictionary
  )
  expect_identical(assigned_5$RESULT, round_5$RESULT)
  expect_identical(assigned_5$FINALRESULT, c("Detected", "Not Detected"))
})

test_that("synthetic outcomes retain released exact raw representations", {
  dictionary <- .test_rc14_lab_dictionary()
  source <- list(seed = 505L)

  round_11 <- data.frame(
    RESULT = rep(c("Not Detected", "negative"), 500L),
    NGENE_CTVALUE = 0, EGENE_CTVALUE = 0,
    stringsAsFactors = FALSE
  )
  assigned_11 <- .set_dependency_outcome(
    round_11, rep("negative", nrow(round_11)), "react1.r11",
    "react1_pcr_positive", source, dictionary
  )
  expect_setequal(unique(assigned_11$RESULT), c("Not Detected", "negative"))

  round_13 <- data.frame(
    RESULT = rep(c("Void", "ambiguous"), 500L),
    NGENE_CTVALUE = NA_real_, EGENE_CTVALUE = NA_real_,
    stringsAsFactors = FALSE
  )
  assigned_13 <- .set_dependency_outcome(
    round_13, rep("missing", nrow(round_13)), "react1.r13",
    "react1_pcr_positive", source, dictionary
  )
  expect_setequal(unique(assigned_13$RESULT), c("Void", "ambiguous"))
})
