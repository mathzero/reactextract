test_that("the generic result support preserves exact labels and missingness", {
  dictionary <- react_dictionary()
  support <- dictionary$synthetic_public_supports[
    dictionary$synthetic_public_supports$support_id ==
      "react1_pcr_lab_result_v1",
    , drop = FALSE
  ]
  support <- support[order(as.integer(support$sort_order)), , drop = FALSE]

  expect_identical(
    support$raw_value,
    c("Detected", "Not Detected", "Void", " ")
  )
  expect_identical(support$label[seq_len(3L)], support$raw_value[seq_len(3L)])
  expect_match(support$label[[4L]], "missing", ignore.case = TRUE)

  occurrence_id <- dictionary$synthetic_profile_overrides$occurrence_id[
    dictionary$synthetic_profile_overrides$support_id ==
      "react1_pcr_lab_result_v1"
  ][[1L]]
  occurrence <- dictionary$occurrences[
    dictionary$occurrences$occurrence_id == occurrence_id, , drop = FALSE
  ]
  spec <- .approved_profile_specs(dictionary)
  spec <- spec[spec$occurrence_id == occurrence_id, , drop = FALSE]
  profiled <- .profile_exact_occurrence(
    c("Detected", "Not Detected", "Void", " ", "-91", NA_character_),
    occurrence, spec, dictionary
  )
  expect_identical(profiled$categorical_counts$value, support$raw_value)
  expect_equal(profiled$categorical_counts$count, rep(1L, 4L))
  expect_equal(
    profiled$missingness$count[profiled$missingness$status == "coded: "], 1L
  )
  expect_true(all(c("coded:Void", "coded: ", "coded:-91") %in%
    profiled$missingness$status))
  expect_false("coded:Not Detected" %in% profiled$missingness$status)
  expect_equal(nrow(profiled$issues), 0L)
})

test_that("laboratory-result repair validation follows each exact support", {
  dictionary <- .test_rc14_lab_dictionary()
  ids <- unique(dictionary$synthetic_profile_overrides$occurrence_id)
  occurrences <- dictionary$occurrences[
    match(ids, dictionary$occurrences$occurrence_id), , drop = FALSE
  ]
  rounds <- unique(occurrences$round_id)
  support <- stats::setNames(lapply(
    ids, .lab_result_support, dictionary = dictionary
  ), ids)
  categorical <- .bind_rows(lapply(ids, function(id) {
    rows <- support[[id]]
    data.frame(
      occurrence_id = id,
      round_id = occurrences$round_id[occurrences$occurrence_id == id],
      value = rows$raw_value,
      count = 5L,
      stringsAsFactors = FALSE
    )
  }), data.frame())
  missingness <- .bind_rows(lapply(ids, function(id) {
    rows <- support[[id]]
    coded <- .coded_missing_domain(data.frame(
      return_value = rows$raw_value,
      display_value = rows$label,
      outcome_state = rows$outcome_state,
      stringsAsFactors = FALSE
    ))$return_value
    data.frame(
      occurrence_id = id,
      round_id = occurrences$round_id[occurrences$occurrence_id == id],
      status = c("database_missing", paste0("coded:", coded)),
      count = c(40L - 5L * nrow(rows), rep(5L, length(coded))),
      stringsAsFactors = FALSE
    )
  }), data.frame())
  good <- list(
    categorical_counts = categorical,
    profiled_occurrences = occurrences[c(
      "occurrence_id", "round_id", "variable"
    )],
    round_denominators = data.frame(
      round_id = rounds, count = rep(40L, length(rounds)),
      stringsAsFactors = FALSE
    ),
    missingness = missingness,
    issues = .empty_issues()
  )
  expect_length(ids, 37L)
  expect_length(rounds, 19L)
  expect_invisible(.validate_lab_result_profile_repair(
    good, ids, rounds, dictionary
  ))

  wrong_case <- good
  wrong_case$categorical_counts$value[
    wrong_case$categorical_counts$value == "Not Detected"
  ] <- "Not detected"
  expect_error(
    .validate_lab_result_profile_repair(wrong_case, ids, rounds, dictionary),
    "exact approved support"
  )

  unsupported <- good
  unsupported$issues <- .issue(
    "warning", "profile", "unrecognised_profile_code",
    "Outside fixed support.", "react1.r01",
    variable = "RESULT", affected_count = 100L
  )
  expect_error(
    .validate_lab_result_profile_repair(unsupported, ids, rounds, dictionary),
    "react1.r01/RESULT (100 records)", fixed = TRUE
  )

  no_blank_missingness <- good
  no_blank_missingness$missingness <- no_blank_missingness$missingness[
    no_blank_missingness$missingness$status != "coded: ", , drop = FALSE
  ]
  expect_error(
    .validate_lab_result_profile_repair(no_blank_missingness, ids, rounds, dictionary),
    "coded missing"
  )

  bad_total <- good
  bad_total$categorical_counts$count[[1L]] <- 4L
  expect_error(
    .validate_lab_result_profile_repair(bad_total, ids, rounds, dictionary),
    "did not reconcile"
  )
})

test_that("occurrence-specific result values never leak into other rounds", {
  dictionary <- .test_rc14_lab_dictionary()
  ids <- .lab_result_occurrence_support_ids()

  expect_identical(
    .lab_result_value_state("Rejected", names(ids)[[1L]], dictionary),
    "missing"
  )
  expect_identical(
    .lab_result_value_state("negative", names(ids)[[2L]], dictionary),
    "negative"
  )
  expect_identical(
    .lab_result_value_state("ambiguous", names(ids)[[3L]], dictionary),
    "missing"
  )
  expect_identical(
    .lab_result_value_state("negative", names(ids)[[3L]], dictionary),
    "unknown"
  )
  expect_identical(
    .lab_result_value_state("ambiguous", names(ids)[[2L]], dictionary),
    "unknown"
  )
})

test_that("exact occurrence profiling retains reviewed representations", {
  dictionary <- .test_rc14_lab_dictionary()
  specs <- .approved_profile_specs(dictionary)
  cases <- list(
    c("occ_3d8dfcbd9554c0afc41ef7e2", "Rejected", "coded:Rejected"),
    c("occ_654a0c1cc8cdb5d263af8bad", "negative", NA_character_),
    c("occ_02fe3a74aee39c3150210a30", "ambiguous", "coded:ambiguous")
  )
  for (case in cases) {
    occurrence <- dictionary$occurrences[
      dictionary$occurrences$occurrence_id == case[[1L]], , drop = FALSE
    ]
    spec <- specs[specs$occurrence_id == case[[1L]], , drop = FALSE]
    profiled <- .profile_exact_occurrence(
      c(case[[2L]], NA_character_), occurrence, spec, dictionary
    )
    expect_equal(nrow(profiled$issues), 0L)
    expect_true(case[[2L]] %in% profiled$categorical_counts$value)
    support <- .lab_result_support(case[[1L]], dictionary)
    coded_values <- sub(
      "^coded:", "",
      profiled$missingness$status[startsWith(
        profiled$missingness$status, "coded:"
      )]
    )
    expect_setequal(
      coded_values,
      support$raw_value[support$outcome_state == "missing"]
    )
    expect_false("negative" %in% coded_values)
    if (!is.na(case[[3L]])) {
      expect_true(case[[3L]] %in% profiled$missingness$status)
    }
  }
})

test_that("rc11 profiles require all 37 result distributions when rebased", {
  profile <- .development_synthetic_profile()
  set_metadata <- function(key, value) {
    index <- match(key, profile$metadata$key)
    if (is.na(index)) {
      profile$metadata <<- rbind(
        profile$metadata,
        data.frame(key = key, value = value, stringsAsFactors = FALSE)
      )
    } else {
      profile$metadata$value[[index]] <<- value
    }
  }
  set_metadata(
    "dictionary_manifest_sha256",
    "f8da578f8aa7827ab3c484ae964853b45aa24a053e20d0423b1e58b59e49410a"
  )
  set_metadata(
    "routing_specification_sha256",
    "d0a0b467e3690e24da457227664db8c255afc5aa90790887be00a3e6658de3f0"
  )
  changes <- .profile_contract_rebase_ids(profile, react_dictionary())

  expect_setequal(
    changes$changed_ids,
    react_dictionary()$synthetic_profile_overrides$occurrence_id
  )
  expect_setequal(changes$repair_ids, changes$changed_ids)
})

test_that("the enclave correction recalculates protected dependency tables", {
  path <- system.file(
    "enclave", "repair_synthetic_profile_v5_lab_results.R",
    package = "reactextract"
  )
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_true(file.exists(path))
  expect_match(
    text,
    "de313c74d64edd725c91aa3eb5784d82cda7da65957b7e1df707bf0e310704a2",
    fixed = TRUE
  )
  expect_match(text, ".validate_lab_result_profile_repair", fixed = TRUE)
  expect_match(text, "react_profile_dependencies_source", fixed = TRUE)
  expect_match(text, "safe_dependency_repair", fixed = TRUE)
  expect_match(text, "outcome_counts", fixed = TRUE)
  expect_match(text, "dependency_counts", fixed = TRUE)
  expect_match(text, "canonical_dictionary_csv", fixed = TRUE)
  expect_match(text, "requires_enclave_disclosure_review", fixed = TRUE)
})
