test_that("Oracle recovery aligns every successful field by the unique key", {
  dictionary <- react_dictionary()
  registry <- dictionary$source_registry
  registry_row <- registry[registry$round_id == "react1.r01", , drop = FALSE]
  query <- function(connection, sql) {
    selected <- sub("^SELECT ", "", sub(" FROM .*$", "", sql))
    fields <- gsub('"', "", strsplit(selected, ", ", fixed = TRUE)[[1]], fixed = TRUE)
    if ("BAD" %in% fields) stop("simulated corrupt field")
    if (identical(fields, "U_PASSCODE")) {
      return(data.frame(U_PASSCODE = c("p1", "p2"), check.names = FALSE))
    }
    data.frame(
      U_PASSCODE = c("p2", "p1"),
      GOOD = c(2L, 1L),
      check.names = FALSE
    )[fields]
  }
  source <- reactextract:::.new_oracle_source(
    connection = structure(list(), class = "fake_connection"),
    registry = registry,
    batch_size = 2L,
    query_fn = query
  )
  fetched <- reactextract:::.read_oracle_round(
    source,
    registry_row,
    c("GOOD", "BAD")
  )

  expect_identical(fetched$data$U_PASSCODE, c("p1", "p2"))
  expect_identical(fetched$data$GOOD, c(1L, 2L))
  expect_false("BAD" %in% names(fetched$data))
  expect_true(any(fetched$issues$variable == "BAD"))
})

test_that("unsafe Oracle key sets are not combined", {
  registry <- react_dictionary()$source_registry
  registry_row <- registry[registry$round_id == "react1.r01", , drop = FALSE]
  query <- function(connection, sql) {
    data.frame(U_PASSCODE = c("p1", "p1"), check.names = FALSE)
  }
  source <- reactextract:::.new_oracle_source(
    structure(list(), class = "fake_connection"), registry, 10L, query
  )
  fetched <- reactextract:::.read_oracle_round(source, registry_row, "GOOD")

  expect_null(fetched$data)
  expect_true(any(fetched$issues$code == "observation_key_not_unique"))
})

test_that("Oracle rounds with no selected value field retain their passcodes", {
  registry <- react_dictionary()$source_registry
  registry_row <- registry[registry$round_id == "react1.r19", , drop = FALSE]
  query <- function(connection, sql) {
    data.frame(U_PASSCODE = c("p1", "p2"), check.names = FALSE)
  }
  source <- reactextract:::.new_oracle_source(
    structure(list(), class = "fake_connection"), registry, 10L, query
  )
  fetched <- reactextract:::.read_oracle_round(
    source,
    registry_row,
    character()
  )

  expect_identical(names(fetched$data), "U_PASSCODE")
  expect_identical(fetched$data$U_PASSCODE, c("p1", "p2"))
  expect_equal(nrow(fetched$issues), 0L)
})
