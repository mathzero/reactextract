test_that("large multi-round extraction stays correct and reports useful progress", {
  participants_per_round <- 50000L
  passcodes <- sprintf("p%06d", seq_len(participants_per_round))
  subjects <- sprintf("s%06d", seq_len(participants_per_round))
  make_round <- function(variable, offset = 0L) {
    values <- rep(c(0L, 1L), length.out = participants_per_round)
    values <- c(values[(offset + 1L):length(values)], values[seq_len(offset)])
    data.frame(
      U_PASSCODE = passcodes,
      SUBJECT_ID = subjects,
      stats::setNames(list(values), variable),
      check.names = FALSE
    )
  }
  rounds <- list(
    react1.r01 = make_round("HEALTHA05"),
    react1.r02 = make_round("HEALTHA_5", 1L),
    react2.r01 = make_round("HEALTHA_05")
  )

  elapsed <- system.time({
    progress_output <- capture.output(
      result <- react_extract(
        react_files(rounds),
        families = "health/preexisting-conditions",
        rounds = names(rounds),
        concepts = "health.preexisting.overweight",
        progress = TRUE
      ),
      type = "message"
    )
  })[["elapsed"]]

  expect_equal(nrow(result$observations), participants_per_round * 3L)
  expect_equal(nrow(result$harmonised_values), participants_per_round * 3L)
  expect_equal(nrow(result$data), participants_per_round * 3L)
  expect_true(is.logical(result$data$health.preexisting.overweight))
  expect_true(all(result$observations$total_visits == 3L))
  expect_identical(
    result$observations$visit_number[seq_len(participants_per_round)],
    rep.int(1L, participants_per_round)
  )
  expect_setequal(
    setdiff(names(result$raw_data), names(.simple_identifiers(result$observations))),
    c("HEALTHA05", "HEALTHA_5", "HEALTHA_05")
  )

  progress_text <- paste(progress_output, collapse = "\n")
  expect_match(progress_text, "Round 1/3 react1.r01", fixed = TRUE)
  expect_match(progress_text, "requesting 1 fields", fixed = TRUE)
  expect_match(progress_text, "received 50,000 records", fixed = TRUE)
  for (stage in c(
    "combining rounds", "assigning visits", "harmonising",
    "creating cleaned table", "creating raw table", "validation"
  )) {
    expect_match(progress_text, paste0("Stage: ", stage), fixed = TRUE)
    expect_match(progress_text, paste0("Stage complete: ", stage), fixed = TRUE)
  }

  timing_keys <- c(
    "timing_loading_dictionary_and_selecting_fields_seconds",
    "timing_combining_rounds_seconds",
    "timing_assigning_visits_seconds",
    "timing_harmonising_seconds",
    "timing_creating_cleaned_table_seconds",
    "timing_creating_raw_table_seconds",
    "timing_validation_seconds",
    "timing_total_seconds"
  )
  expect_true(all(timing_keys %in% result$manifest$key))
  expect_true(all(as.numeric(result$manifest$value[
    match(timing_keys, result$manifest$key)
  ]) >= 0))

  # This generous ceiling catches accidental quadratic regressions while
  # remaining stable on slower CI and enclave desktops.
  expect_lt(unname(elapsed), 30)
})

test_that("acute symptoms and testing history receive source-preserving cleaned output", {
  rounds <- list(
    react1.r10 = data.frame(
      U_PASSCODE = c("p1", "p2"),
      SUBJECT_ID = c("s1", "s2"),
      TEMPC = c(1L, 2L),
      TEMPC01 = c(NA_real_, 38.0),
      TEMPC02 = c(37.3, NA_real_),
      ABPREV1 = c(1L, 3L),
      check.names = FALSE
    ),
    react2.r01 = data.frame(
      U_PASSCODE = c("p1", "p2"),
      SUBJECT_ID = c("s1", "s2"),
      NEWQA = c(1L, -92L),
      check.names = FALSE
    )
  )
  concepts <- c(
    "health.acute_symptoms.highest_temperature_reading",
    "infection_measurement.testing_history.previous_antibody_test_history"
  )

  result <- react_extract(
    react_files(rounds),
    families = "all",
    rounds = names(rounds),
    concepts = concepts
  )

  expect_equal(nrow(result$observations), 4L)
  expect_setequal(
    setdiff(names(result$raw_data), names(.simple_identifiers(result$observations))),
    c("TEMPC", "TEMPC01", "TEMPC02", "ABPREV1", "NEWQA")
  )
  expect_equal(nrow(result$raw_values), 10L)
  expect_equal(nrow(result$harmonised_values), 10L)
  expect_true(all(result$harmonised_values$transform_id == "source-preserving-coding"))
  temperature_columns <- paste0(
    concepts[[1]], "__field__", c("TEMPC", "TEMPC01", "TEMPC02")
  )
  antibody_column <- concepts[[2]]
  expect_true(all(c(temperature_columns, antibody_column) %in% names(result$data)))
  expect_identical(
    result$data[[temperature_columns[[1]]]][result$data$round_id == "react1.r10"],
    c("?C", "?F")
  )
  expect_identical(
    result$data[[antibody_column]][result$data$round_id == "react1.r10"],
    c("Yes, just once", "No")
  )
  expect_setequal(result$column_dictionary$output_strategy, c("concept", "component"))
  expect_setequal(unique(react_concept_coding(concepts)$concept_id), concepts)
})

test_that("every reviewed feature topic produces cleaned output", {
  dictionary <- react_dictionary()
  occurrences <- dictionary$occurrences
  plan <- dictionary$concept_output_columns
  occurrences <- occurrences[
    occurrences$occurrence_id %in% plan$occurrence_id,
    ,
    drop = FALSE
  ]
  topic_ids <- unique(occurrences$primary_taxonomy_id)
  representatives <- occurrences[
    match(topic_ids, occurrences$primary_taxonomy_id),
    ,
    drop = FALSE
  ]
  rounds <- split(representatives, representatives$round_id)
  round_data <- lapply(rounds, function(rows) {
    values <- list(U_PASSCODE = "p1")
    for (i in seq_len(nrow(rows))) {
      lookup <- dictionary$concept_value_lookup[
        dictionary$concept_value_lookup$concept_id == rows$primary_concept_id[[i]] &
          dictionary$concept_value_lookup$round_id == rows$round_id[[i]] &
          dictionary$concept_value_lookup$raw_field == rows$variable[[i]] &
          dictionary$concept_value_lookup$mapping_status %in%
            c("source_label", "inferred_binary"),
        ,
        drop = FALSE
      ]
      value <- if (nrow(lookup) > 0L) {
        lookup$raw_value[[1]]
      } else if (rows$data_type[[i]] == "NUMBER") {
        1
      } else {
        "example"
      }
      values[[rows$variable[[i]]]] <- value
    }
    as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
  })

  result <- react_extract(
    react_files(round_data),
    families = "all",
    rounds = names(round_data),
    concepts = unique(representatives$primary_concept_id)
  )

  expect_setequal(unique(result$raw_values$taxonomy_id), topic_ids)
  expect_setequal(
    unique(result$harmonised_values$source_occurrence_id),
    unique(result$raw_values$occurrence_id)
  )
  expect_true(all(unique(result$harmonised_values$output_column) %in% names(result$data)))
  expect_true(all(result$harmonised_values$transform_id %in%
    c(
      "source-preserving-coding",
      dictionary$mappings$transform_id,
      dictionary$harmonisation_groups$decision_id
    )))
  expect_equal(anyDuplicated(names(result$data)), 0L)
})
