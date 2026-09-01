test_that("the supported R-version contract has no upper bound", {
  expect_false(.reactextract_supports_r("4.3.3"))
  expect_true(.reactextract_supports_r("4.4.0"))
  expect_true(.reactextract_supports_r("4.4.9"))
  expect_true(.reactextract_supports_r("4.5.1"))
  expect_true(.reactextract_supports_r("4.6.0"))
  expect_true(.reactextract_supports_r("99.0.0"))
})

test_that("DESCRIPTION declares only the supported minimum R version", {
  depends <- utils::packageDescription("reactextract", fields = "Depends")

  expect_match(depends, "R \\(>= 4\\.4\\.0\\)")
  expect_false(grepl("R \\([^)]*<", depends))
})
