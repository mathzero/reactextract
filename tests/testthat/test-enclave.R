preflight_source <- function(fail_field = "", conflicting_links = 0, fail_view = FALSE) {
  registry <- react_dictionary()$source_registry
  queries <- new.env(parent = emptyenv())
  queries$sql <- character()
  query <- function(connection, sql) {
    queries$sql <- c(queries$sql, sql)
    if (fail_view && grepl(
      'SELECT "U_PASSCODE" FROM REACT1_R01_V WHERE 1 = 0',
      sql,
      fixed = TRUE
    )) {
      stop("simulated inaccessible view")
    }
    if (grepl("N_CONFLICTING_PASSCODES", sql, fixed = TRUE)) {
      return(data.frame(N_CONFLICTING_PASSCODES = conflicting_links))
    }
    if (grepl("N_NONMISSING_PASSCODES", sql, fixed = TRUE)) {
      return(data.frame(
        N_ROWS = 100,
        N_NONMISSING_PASSCODES = 100,
        N_DISTINCT_PASSCODES = 100,
        N_NONMISSING_SUBJECTS = 100
      ))
    }
    if (grepl("N_NONMISSING_KEYS", sql, fixed = TRUE)) {
      return(data.frame(
        N_ROWS = 50,
        N_NONMISSING_KEYS = 50,
        N_DISTINCT_KEYS = 50
      ))
    }
    failed_match <- length(fail_field) > 0L && any(vapply(
      fail_field[nzchar(fail_field)],
      function(field) grepl(paste0('"', field, '"'), sql, fixed = TRUE),
      logical(1)
    ))
    if (failed_match) {
      stop("simulated unavailable field")
    }
    if (grepl("WHERE 1 = 0", sql, fixed = TRUE)) {
      return(data.frame())
    }
    stop("The preflight attempted an unexpected respondent-data query.")
  }
  list(
    source = reactextract:::.new_oracle_source(
      structure(list(), class = "fake_connection"), registry, 20L, query
    ),
    queries = queries
  )
}

test_that("Oracle preflight checks fields with zero-row queries", {
  fixture <- preflight_source()
  result <- react_validate_oracle(
    fixture$source,
    families = "health/preexisting-conditions",
    rounds = "react1.r01"
  )

  expect_identical(result$rounds$status, "passed")
  expect_true(all(result$fields$status == "available"))
  expect_identical(result$crosswalk$status, "passed")
  expect_identical(result$manifest$value[result$manifest$key == "status"], "passed")
  field_queries <- fixture$queries$sql[
    !grepl("COUNT", fixture$queries$sql, fixed = TRUE)
  ]
  expect_true(all(grepl("WHERE 1 = 0", field_queries, fixed = TRUE)))
})

test_that("Oracle preflight isolates unavailable fields without fetching values", {
  fixture <- preflight_source("HEALTHA05")
  result <- react_validate_oracle(
    fixture$source,
    families = "health/preexisting-conditions",
    rounds = "react1.r01"
  )

  failed <- result$fields[result$fields$status == "unavailable", , drop = FALSE]
  expect_identical(failed$variable, "HEALTHA05")
  expect_identical(result$rounds$unavailable_field_count, 1L)
  expect_identical(result$rounds$status, "failed")
  expect_true(any(result$issues$code == "fields_unavailable"))
  expect_identical(
    result$manifest$value[result$manifest$key == "status"],
    "attention_required"
  )
})

test_that("an inaccessible view fails once without recursive field queries", {
  fixture <- preflight_source(fail_view = TRUE)
  result <- react_validate_oracle(
    fixture$source,
    families = "health/preexisting-conditions",
    rounds = "react1.r01"
  )

  round_queries <- fixture$queries$sql[
    grepl("REACT1_R01_V", fixture$queries$sql, fixed = TRUE)
  ]
  expect_length(round_queries, 1L)
  expect_identical(result$rounds$status, "failed")
  expect_true(any(result$issues$code == "view_or_key_unavailable"))
})

test_that("confirmed absent round 19 fields pass with visible notes", {
  confirmed <- c("PREVREACT", "PREVREACTID1", "PREVREACTID2", "PREVREACTID3")
  fixture <- preflight_source(fail_field = confirmed)
  result <- react_validate_oracle(
    fixture$source,
    families = "all",
    rounds = "react1.r19"
  )

  noted <- result$fields[result$fields$status == "confirmed_unavailable", ]
  expect_setequal(noted$variable, confirmed)
  expect_equal(result$rounds$confirmed_unavailable_field_count, 4L)
  expect_identical(result$rounds$status, "passed_with_notes")
  expect_identical(
    result$manifest$value[result$manifest$key == "status"],
    "passed_with_notes"
  )
  expect_true(any(result$issues$code == "fields_confirmed_unavailable"))
})

test_that("crosswalk conflicts are aggregate, visible, and non-multiplying", {
  fixture <- preflight_source(conflicting_links = 2)
  result <- react_validate_oracle(
    fixture$source,
    families = "health/preexisting-conditions",
    rounds = "react1.r01"
  )

  expect_identical(result$crosswalk$status, "warning")
  expect_equal(result$crosswalk$conflicting_passcode_count, 2)
  expect_true(any(result$issues$code == "crosswalk_conflict"))
})

test_that("enclave report writer emits metadata-only tables", {
  fixture <- preflight_source()
  result <- react_validate_oracle(
    fixture$source,
    families = "health/preexisting-conditions",
    rounds = "react1.r01"
  )
  path <- withr::local_tempdir()
  react_write_enclave_report(result, path)

  expect_setequal(
    basename(list.files(path)),
    paste0(c("rounds", "fields", "crosswalk", "issues", "manifest"), ".csv")
  )
  contents <- paste(
    unlist(lapply(list.files(path, full.names = TRUE), readLines), use.names = FALSE),
    collapse = "\n"
  )
  expect_false(grepl("U_PASSCODE,SUBJECT_ID", contents, fixed = TRUE))
})
