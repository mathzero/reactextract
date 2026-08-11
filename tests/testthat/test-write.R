test_that("RDS writer preserves the long result", {
  result <- react_extract(
    react_files(list(
      react1.r01 = data.frame(U_PASSCODE = "p1", HEALTHA05 = 1L)
    )),
    families = "health/preexisting-conditions",
    rounds = "react1.r01",
    concepts = "health.preexisting.overweight"
  )
  path <- withr::local_tempfile(fileext = ".rds")
  expect_silent(react_write(result, path, "rds"))
  expect_identical(readRDS(path), result)
})
