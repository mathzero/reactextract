test_that("bundled CSV files are read completely in a C locale", {
  skip_if_not_installed("withr")
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  lines <- c("id,label", "1,plain", "2,Can’t tell – en dash", "3,after")
  con <- file(path, open = "wb")
  writeLines(enc2utf8(lines), con, useBytes = TRUE)
  close(con)

  read <- withr::with_locale(c(LC_CTYPE = "C"), .read_literal_csv(path))

  expect_identical(read$id, c("1", "2", "3"))
  expect_identical(read$label[[2]], "Can’t tell – en dash")
})
