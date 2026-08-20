test_that("wide, long, and both modes contain equivalent values", {
  source <- react_files(preexisting_rounds(), crosswalk = fixture_crosswalk())
  arguments <- list(
    source = source,
    families = "health/preexisting-conditions",
    rounds = c("REACT1_R01", "REACT2_S5_R06"),
    concepts = c(
      "health.preexisting.overweight",
      "health.preexisting.stroke"
    ),
    progress = FALSE
  )
  wide <- do.call(react_extract, c(arguments, list(output = "wide")))
  long <- do.call(react_extract, c(arguments, list(output = "long")))
  both <- do.call(react_extract, c(arguments, list(output = "both")))

  expect_identical(
    names(wide),
    c("data", "raw_data", "observations", "column_dictionary", "issues", "manifest")
  )
  expect_identical(
    names(long),
    c(
      "observations", "raw_values", "harmonised_values",
      "column_dictionary", "issues", "manifest"
    )
  )
  expect_identical(
    names(both),
    c(
      "data", "raw_data", "observations", "raw_values",
      "harmonised_values", "column_dictionary", "issues", "manifest"
    )
  )
  expect_identical(wide$data, both$data)
  expect_identical(wide$raw_data, both$raw_data)
  expect_identical(long$raw_values, both$raw_values)
  expect_identical(long$harmonised_values, both$harmonised_values)
  expect_identical(wide$observations, both$observations)
  expect_identical(long$observations, both$observations)

  manifest_value <- function(result, key) {
    result$manifest$value[result$manifest$key == key]
  }
  expect_identical(manifest_value(wide, "output_mode"), "wide")
  expect_identical(manifest_value(long, "output_mode"), "long")
  expect_identical(manifest_value(both, "output_mode"), "both")
  for (key in c("observation_count", "raw_value_count", "harmonised_value_count")) {
    expect_identical(manifest_value(wide, key), manifest_value(both, key))
    expect_identical(manifest_value(long, key), manifest_value(both, key))
  }
})

test_that("wide results explain how to request profiling details", {
  result <- react_extract(
    react_files(list(
      react1.r01 = data.frame(U_PASSCODE = "p1", HEALTHA05 = 1L)
    )),
    concepts = "health.preexisting.overweight",
    rounds = "react1.r01",
    progress = FALSE
  )
  expect_error(
    react_profile(result),
    'output = "long"',
    fixed = TRUE
  )
  expect_error(
    react_extract(
      react_files(list(react1.r01 = data.frame(U_PASSCODE = "p1"))),
      output = "unsupported"
    ),
    "arg"
  )
})

test_that("Oracle-fixture output modes share the same wide values", {
  dictionary <- react_dictionary()
  registry <- dictionary$source_registry
  crosswalk_object <- registry$crosswalk_object[[1L]]
  query <- function(connection, sql) {
    selected <- sub("^SELECT ", "", sub(" FROM .*$", "", sql))
    fields <- gsub('"', "", strsplit(selected, ", ", fixed = TRUE)[[1L]], fixed = TRUE)
    if (grepl(crosswalk_object, sql, fixed = TRUE)) {
      data <- data.frame(
        REACT_ID = c("p1", "p2"),
        SUBJECT_ID = c("s1", "s2"),
        check.names = FALSE
      )
    } else {
      data <- data.frame(
        U_PASSCODE = c("p1", "p2"),
        HEALTHA05 = c(0L, 1L),
        check.names = FALSE
      )
    }
    data[fields]
  }
  source <- reactextract:::.new_oracle_source(
    structure(list(), class = "fake_connection"),
    registry,
    20L,
    query
  )
  arguments <- list(
    source = source,
    concepts = "health.preexisting.overweight",
    rounds = "react1.r01",
    progress = FALSE
  )
  wide <- do.call(react_extract, c(arguments, list(output = "wide")))
  both <- do.call(react_extract, c(arguments, list(output = "both")))

  expect_identical(wide$data, both$data)
  expect_identical(wide$raw_data, both$raw_data)
  expect_identical(wide$data$SUBJECT_ID, c("s1", "s2"))
})

test_that("wide part binding preserves types and uses character for conflicts", {
  first <- data.frame(
    observation_id = "one",
    integer_value = 1L,
    date_value = as.Date("2020-01-01"),
    mixed_value = 1L,
    check.names = FALSE
  )
  second <- data.frame(
    observation_id = "two",
    integer_value = 2L,
    date_value = as.Date("2020-01-02"),
    mixed_value = "two",
    check.names = FALSE
  )
  bound <- reactextract:::.bind_simple_parts(
    list(first, second),
    c("observation_id", "integer_value", "date_value", "mixed_value", "absent")
  )

  expect_identical(bound$integer_value, c(1L, 2L))
  expect_s3_class(bound$date_value, "Date")
  expect_identical(bound$mixed_value, c("1", "two"))
  expect_type(bound$absent, "logical")
  expect_true(all(is.na(bound$absent)))
  expect_identical(names(bound), c(
    "observation_id", "integer_value", "date_value", "mixed_value", "absent"
  ))
})

test_that("all-field wide output avoids most retained long-table memory", {
  rounds <- react_dictionary()$rounds$round_id
  source <- react_synthetic(
    n_per_round = stats::setNames(rep(5L, length(rounds)), rounds),
    seed = 404L
  )
  both <- react_extract(
    source,
    families = "all",
    rounds = "all",
    progress = FALSE,
    output = "both"
  )
  compact <- both[c(
    "data", "raw_data", "observations", "column_dictionary", "issues", "manifest"
  )]

  expect_lt(as.numeric(object.size(compact)), 0.35 * as.numeric(object.size(both)))
  expect_equal(nrow(compact$data), 125L)
  expect_equal(nrow(compact$raw_data), 125L)
})
