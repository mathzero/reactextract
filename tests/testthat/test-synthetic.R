test_that("synthetic extraction defaults to the compact wide result contract", {
  source <- react_synthetic(
    n_per_round = c(REACT1_R01 = 20L, REACT2_S5_R01 = 20L),
    seed = 41L
  )
  result <- react_extract(
    source,
    concepts = "health.preexisting.overweight",
    rounds = c("REACT1_R01", "REACT2_S5_R01"),
    progress = FALSE
  )

  expect_identical(
    names(result),
    c(
      "data", "raw_data", "observations", "column_dictionary", "issues", "manifest"
    )
  )
  expect_s3_class(result, "react_extract_result")
  expect_false(any(c("raw_values", "harmonised_values") %in% names(result)))
  expect_equal(nrow(result$observations), 40L)
  expect_true(all(result$observations$visit_number == 1L))
  expect_true(all(result$observations$total_visits == 1L))
  expect_equal(length(unique(result$observations$SUBJECT_ID)), 40L)
  expect_true(all(startsWith(result$observations$SUBJECT_ID, "SYN-SUBJECT-")))
  expect_identical(
    result$manifest$value[result$manifest$key == "synthetic_profile_status"],
    "approved_for_release"
  )
  expect_identical(
    result$manifest$value[result$manifest$key == "output_mode"],
    "wide"
  )
  expect_identical(
    result$manifest$value[result$manifest$key == "long_tables_included"],
    "false"
  )
  printed <- capture.output(print(result))
  expect_match(paste(printed, collapse = "\n"), "Output: wide", fixed = TRUE)
})

test_that("the default profile is the formally approved v5 release", {
  approved <- react_synthetic_profile(refresh = TRUE)
  approved_metadata <- stats::setNames(
    approved$metadata$value, approved$metadata$key
  )
  expect_identical(unname(approved_metadata[["status"]]), "approved_for_release")
  expect_identical(
    unname(approved_metadata[["profile_release"]]),
    "react-synthetic-profile-v5"
  )
  expect_identical(
    unname(approved_metadata[["profile_version"]]),
    "enclave-profile-v5-outcome-dependencies-lab-results-corrected"
  )
  expect_identical(unname(approved_metadata[["dictionary_compatibility"]]), "exact")
  expect_equal(nrow(approved$outcome_counts), 81L)
  expect_equal(nrow(approved$dependency_specs), 22L)
  expect_gt(nrow(approved$dependency_counts), 0L)

  development <- react_synthetic_profile(development = TRUE)
  development_metadata <- stats::setNames(
    development$metadata$value, development$metadata$key
  )
  expect_identical(unname(development_metadata[["status"]]), "development_only")
})

test_that("default sample size is 25,000 and every round can be generated", {
  default_source <- react_synthetic()
  expect_equal(sum(default_source$n_per_round), 25000L)
  expect_equal(length(default_source$n_per_round), 25L)

  rounds <- react_dictionary()$rounds$round_id
  small_source <- react_synthetic(
    n_per_round = stats::setNames(rep(2L, length(rounds)), rounds),
    seed = 4L
  )
  result <- react_extract(
    small_source,
    concepts = "health.preexisting.overweight",
    rounds = "all",
    progress = FALSE
  )
  expect_equal(nrow(result$observations), 50L)
  expect_setequal(unique(result$observations$round_id), rounds)
})

test_that("a complete small all-field extraction covers the 25-round contract", {
  dictionary <- react_dictionary()
  counts <- stats::setNames(rep(1L, nrow(dictionary$rounds)), dictionary$rounds$round_id)
  result <- react_extract(
    react_synthetic(
      n_per_round = counts,
      seed = 202L
    ),
    families = "all",
    rounds = "all",
    progress = FALSE
  )

  expect_equal(nrow(result$observations), 25L)
  expect_equal(length(unique(result$observations$SUBJECT_ID)), 25L)
  expect_false(any(c("raw_values", "harmonised_values") %in% names(result)))
  expect_equal(
    as.integer(result$manifest$value[result$manifest$key == "raw_value_count"]),
    nrow(dictionary$occurrences)
  )
  expect_true(all(c("data", "raw_data", "manifest") %in% names(result)))
})

test_that("synthetic streams are deterministic and selector independent", {
  narrow <- react_extract(
    react_synthetic(
      n_per_round = c(REACT1_R01 = 30L), seed = 12L
    ),
    concepts = "health.preexisting.overweight",
    rounds = "REACT1_R01",
    progress = FALSE
  )
  broad <- react_extract(
    react_synthetic(
      n_per_round = c(REACT1_R01 = 30L), seed = 12L
    ),
    families = "health/preexisting-conditions",
    rounds = "REACT1_R01",
    progress = FALSE
  )
  changed <- react_extract(
    react_synthetic(
      n_per_round = c(REACT1_R01 = 30L), seed = 13L
    ),
    concepts = "health.preexisting.overweight",
    rounds = "REACT1_R01",
    progress = FALSE
  )

  expect_identical(
    narrow$raw_data$HEALTHA05,
    broad$raw_data$HEALTHA05
  )
  expect_false(identical(
    narrow$raw_data$HEALTHA05,
    changed$raw_data$HEALTHA05
  ))
})

test_that("approved multi-level gates are applied in questionnaire order", {
  old_dictionary <- .reactextract_env$dictionary
  on.exit(assign("dictionary", old_dictionary, envir = .reactextract_env), add = TRUE)
  dictionary <- react_dictionary()
  round_occurrences <- dictionary$occurrences[
    dictionary$occurrences$round_id == "react1.r01" &
      dictionary$occurrences$variable %in% c("HEALTHA05", "HEALTHA06", "HEALTHA07"),
    , drop = FALSE
  ]
  id <- stats::setNames(round_occurrences$occurrence_id, round_occurrences$variable)
  instrument <- dictionary$instruments$instrument_id[
    dictionary$instruments$round_id == "REACT1_R01"
  ][[1L]]
  dictionary$routing_rules <- data.frame(
    routing_rule_id = c("child_gate", "parent_gate"),
    instrument_id = instrument,
    rule_type = "gate",
    rule_label = c("child", "parent"),
    combine_clauses = "any",
    false_value_kind = "database_missing",
    false_value = "",
    review_state = "approved",
    reviewed_by = "mathzero",
    review_date = "2026-08-14",
    evidence_locator = "test fixture",
    note = "test fixture",
    stringsAsFactors = FALSE
  )
  dictionary$routing_conditions <- data.frame(
    routing_rule_id = c("child_gate", "parent_gate"),
    clause_id = c("child_gate.1", "parent_gate.1"),
    condition_order = "1",
    parent_occurrence_id = c(id[["HEALTHA06"]], id[["HEALTHA05"]]),
    operator = "equals",
    comparison_values_json = "[1]",
    stringsAsFactors = FALSE
  )
  dictionary$routing_targets <- data.frame(
    routing_rule_id = c("child_gate", "parent_gate"),
    target_occurrence_id = c(id[["HEALTHA07"]], id[["HEALTHA06"]]),
    target_order = "1",
    stringsAsFactors = FALSE
  )
  .reactextract_env$dictionary <- dictionary

  result <- react_extract(
    react_synthetic(
      profile = react_synthetic_profile(development = TRUE),
      n_per_round = c(REACT1_R01 = 200L), seed = 24L
    ),
    families = "health/preexisting-conditions",
    rounds = "REACT1_R01",
    progress = FALSE
  )
  raw <- result$raw_data
  expect_true(all(is.na(raw$HEALTHA06[is.na(raw$HEALTHA05) | raw$HEALTHA05 != 1])))
  expect_true(all(is.na(raw$HEALTHA07[is.na(raw$HEALTHA06) | raw$HEALTHA06 != 1])))
})

test_that("selected checkbox gates and alternative routes are evaluated correctly", {
  expect_identical(
    .condition_true(c(1, 0, NA, TRUE, FALSE), "selected_any", "[]"),
    c(TRUE, FALSE, FALSE, TRUE, FALSE)
  )

  data <- data.frame(parent_a = c(1, 0, 0), parent_b = c(0, 1, 0), child = 7)
  occurrence_variable <- c(a = "parent_a", b = "parent_b", child = "child")
  rules <- data.frame(
    routing_rule_id = c("route_a", "route_b"), rule_type = "gate",
    false_value_kind = "database_missing", false_value = "",
    stringsAsFactors = FALSE
  )
  conditions <- data.frame(
    routing_rule_id = c("route_a", "route_b"),
    clause_id = c("route_a.1", "route_b.1"), condition_order = "1",
    parent_occurrence_id = c("a", "b"), operator = "equals",
    comparison_values_json = "[1]", stringsAsFactors = FALSE
  )
  targets <- data.frame(
    routing_rule_id = c("route_a", "route_b"),
    target_occurrence_id = "child", target_order = "1",
    stringsAsFactors = FALSE
  )
  routed <- .apply_synthetic_gate_rules(
    data, rules, conditions, targets, occurrence_variable
  )
  expect_equal(routed$child, c(7, 7, NA))
})

test_that("profiling tolerates mixed date representations without retaining them", {
  bins <- data.frame(
    bin_id = c("year_2020", "year_2021"),
    lower = c("2020-01-01", "2021-01-01"),
    upper = c("2020-12-31", "2021-12-31"),
    stringsAsFactors = FALSE
  )
  values <- c("2020-03-02", "31/12/2020", "15-Jan-2021", "not a date", NA)
  expect_identical(
    .bin_values(values, bins, "date"),
    c("year_2020", "year_2020", "year_2021", NA_character_, NA_character_)
  )
})

test_that("profile source uses reviewed dispositions and exports no row data", {
  old_dictionary <- .reactextract_env$dictionary
  on.exit(assign("dictionary", old_dictionary, envir = .reactextract_env), add = TRUE)
  dictionary <- react_dictionary()
  dictionary$synthetic_profile_specs$review_state <- "candidate"
  occurrence <- dictionary$occurrences[
    dictionary$occurrences$round_id == "react1.r01" &
      dictionary$occurrences$variable == "HEALTHA05",
    , drop = FALSE
  ]
  dictionary$synthetic_profile_specs$review_state[
    dictionary$synthetic_profile_specs$occurrence_id == occurrence$occurrence_id
  ] <- "approved"
  dictionary$safe_bins$review_state <- "approved"
  .reactextract_env$dictionary <- dictionary

  round <- data.frame(
    U_PASSCODE = paste0("real-key-", seq_len(25L)),
    HEALTHA05 = c(rep(0L, 13L), rep(1L, 11L), NA_integer_),
    check.names = FALSE
  )
  profile <- react_profile_source(
    react_files(list(react1.r01 = round)),
    rounds = "REACT1_R01",
    progress = FALSE,
    batch_size = 1L
  )
  expect_equal(profile$round_denominators$count, 25L)
  expect_gt(nrow(profile$categorical_counts), 0L)
  expect_false(any(grepl("real-key-", unlist(profile), fixed = TRUE)))

  safe <- react_prepare_profile_export(profile)
  expect_identical(
    safe$metadata$value[safe$metadata$key == "status"],
    "requires_enclave_disclosure_review"
  )
  expect_true("suppressed" %in% names(safe$categorical_counts))

  output <- tempfile("react-profile-")
  react_write_profile(safe, output)
  reread <- react_read_profile(output)
  expect_true(all(.profile_v2_required %in% names(reread)))
  expect_error(
    react_write_profile(safe, output),
    "new or empty directory"
  )
  writeLines("not part of the profile", file.path(output, "unexpected.txt"))
  expect_error(
    react_read_profile(output),
    "not covered by its manifest"
  )
})

test_that("profile source refuses unreviewed profiling dispositions", {
  old_dictionary <- .reactextract_env$dictionary
  on.exit(assign("dictionary", old_dictionary, envir = .reactextract_env), add = TRUE)
  dictionary <- react_dictionary()
  dictionary$synthetic_profile_specs$review_state <- "candidate"
  .reactextract_env$dictionary <- dictionary
  expect_error(
    react_profile_source(
      react_files(list(react1.r01 = data.frame(U_PASSCODE = "p1"))),
      rounds = "REACT1_R01",
      progress = FALSE
    ),
    "no approved profiling dispositions"
  )
})

test_that("administrative missing codes never enter numeric bins", {
  old_dictionary <- .reactextract_env$dictionary
  on.exit(assign("dictionary", old_dictionary, envir = .reactextract_env), add = TRUE)
  dictionary <- react_dictionary()
  candidate <- merge(
    dictionary$occurrences[
      dictionary$occurrences$round_id == "react1.r01" &
        dictionary$occurrences$data_type == "NUMBER", , drop = FALSE
    ],
    dictionary$synthetic_profile_specs[
      dictionary$synthetic_profile_specs$profile_kind == "integer", , drop = FALSE
    ],
    by = "occurrence_id"
  )
  candidate <- candidate[nzchar(candidate$bin_spec_id), , drop = FALSE]
  occurrence <- candidate[1L, , drop = FALSE]
  dictionary$synthetic_profile_specs$review_state <- "candidate"
  dictionary$synthetic_profile_specs$review_state[
    dictionary$synthetic_profile_specs$occurrence_id == occurrence$occurrence_id
  ] <- "approved"
  dictionary$response_options <- dictionary$response_options[
    dictionary$response_options$occurrence_id != occurrence$occurrence_id,
    , drop = FALSE
  ]
  .reactextract_env$dictionary <- dictionary

  values <- c(-91, -92, -77, -66, -99, -555, 20, NA)
  round <- data.frame(U_PASSCODE = paste0("key-", seq_along(values)))
  round[[occurrence$variable]] <- values
  profile <- react_profile_source(
    react_files(list(react1.r01 = round)), rounds = "REACT1_R01",
    progress = FALSE, include_routing = FALSE, include_overall = FALSE
  )
  coded <- profile$missingness[startsWith(profile$missingness$status, "coded:"), ]
  expect_setequal(
    coded$status,
    paste0("coded:", c(-91, -92, -77, -66, -99, -555))
  )
  expect_true(all(coded$count == 1L))
  expect_equal(sum(profile$numeric_bin_counts$count), 1L)
  expect_false(any(profile$issues$code == "value_outside_safe_bins"))
})

test_that("administrative Date offsets never enter calendar bins", {
  occurrence <- data.frame(
    occurrence_id = "occ_date", round_id = "react1.r01",
    variable = "TEST_DATE", data_type = "DATE", stringsAsFactors = FALSE
  )
  spec <- data.frame(
    occurrence_id = "occ_date", round_id = "react1.r01",
    profile_kind = "date", bin_spec_id = "calendar_month_v1",
    stringsAsFactors = FALSE
  )
  dictionary <- react_dictionary()
  dictionary$response_options <- dictionary$response_options[0, , drop = FALSE]
  dictionary$safe_bins <- data.frame(
    bin_spec_id = "calendar_month_v1", bin_id = "2021-01", value_type = "date",
    lower = "2021-01-01", upper = "2021-01-31", boundary_rules = "inclusive",
    sampling_rule = "uniform_date", review_state = "approved", note = "test",
    stringsAsFactors = FALSE
  )
  value <- as.Date(c(-91, -92, -77, -66, -99, -555, 18642, NA), origin = "1970-01-01")
  profiled <- .profile_exact_occurrence(value, occurrence, spec, dictionary)
  coded <- profiled$missingness[startsWith(profiled$missingness$status, "coded:"), ]
  expect_setequal(
    coded$status,
    paste0("coded:", c(-91, -92, -77, -66, -99, -555))
  )
  expect_true(all(coded$count == 1L))
  expect_equal(sum(profiled$numeric_bin_counts$count), 1L)
  expect_false(any(profiled$issues$code == "value_outside_safe_bins"))
})

test_that("unsupported values become explicit synthetic missingness", {
  occurrence <- data.frame(
    occurrence_id = "occ_numeric", round_id = "react1.r01",
    variable = "TEST_NUMBER", data_type = "NUMBER", stringsAsFactors = FALSE
  )
  spec <- data.frame(
    occurrence_id = "occ_numeric", round_id = "react1.r01",
    profile_kind = "integer", bin_spec_id = "test_bins",
    stringsAsFactors = FALSE
  )
  dictionary <- react_dictionary()
  dictionary$response_options <- dictionary$response_options[0, , drop = FALSE]
  dictionary$safe_bins <- data.frame(
    bin_spec_id = "test_bins", bin_id = "valid", value_type = "integer",
    lower = "0", upper = "10", boundary_rules = "inclusive",
    sampling_rule = "uniform_integer", review_state = "approved", note = "test",
    stringsAsFactors = FALSE
  )
  profiled <- .profile_exact_occurrence(
    c(1, 2, 50, 51, NA), occurrence, spec, dictionary
  )
  unsupported <- profiled$missingness[
    profiled$missingness$status == "outside_safe_support", , drop = FALSE
  ]
  expect_equal(unsupported$count, 2L)
  expect_equal(profiled$issues$affected_count, 2L)
})

test_that("issue counts receive suppression and rounding", {
  issues <- rbind(
    .issue(
      "warning", "profile", "value_outside_safe_bins", "test",
      "react1.r01", variable = "A", affected_count = 13L
    ),
    .issue(
      "warning", "profile", "unrecognised_profile_code", "test",
      "react1.r01", variable = "B", affected_count = 3L
    )
  )
  safe <- .sdc_issue_table(issues, react_sdc_policy())
  expect_equal(nrow(safe), 1L)
  expect_identical(safe$affected_count, "15")
})

test_that("profile policy rebasing identifies every changed safe occurrence", {
  dictionary <- react_dictionary()
  specs <- .approved_profile_specs(dictionary)
  candidate <- which(
    specs$profile_kind == "integer" &
      specs$generation_action == "empirical" &
      nzchar(specs$bin_spec_id)
  )[[1L]]
  profile <- list(
    metadata = data.frame(
      key = c("routing_specification_sha256", "dictionary_manifest_sha256"),
      value = c(
        .routing_specification_sha256(dictionary),
        react_dictionary_version()$manifest_sha256
      ),
      stringsAsFactors = FALSE
    ),
    round_denominators = data.frame(
      round_id = dictionary$rounds$round_id,
      stringsAsFactors = FALSE
    ),
    profile_specs = specs,
    safe_bins = dictionary$safe_bins[
      dictionary$safe_bins$review_state == "approved", , drop = FALSE
    ]
  )
  profile$profile_specs$profile_kind[[candidate]] <- "categorical"
  changes <- .profile_contract_rebase_ids(profile, dictionary)
  expected <- specs$occurrence_id[[candidate]]
  expect_identical(changes$changed_ids, expected)
  expect_identical(changes$repair_ids, expected)

  profile$metadata$value <- "changed-routing-hash"
  expect_error(
    .profile_contract_rebase_ids(profile, dictionary),
    "routing contract changed"
  )

  profile$metadata$value <- c(
    "d0a0b467e3690e24da457227664db8c255afc5aa90790887be00a3e6658de3f0",
    "03a2fb41a02becbe292663934e6ed436a85335b93d5004118a82ea9e4460a846"
  )
  expect_identical(
    .profile_contract_rebase_ids(profile, dictionary)$changed_ids,
    expected
  )

  profile$metadata$value <- c(
    "70ba0bd048725b3763205a633988dcfee4789c275a9aab8d2659b72f7d9ecd83",
    "03a2fb41a02becbe292663934e6ed436a85335b93d5004118a82ea9e4460a846"
  )
  expect_identical(
    .profile_contract_rebase_ids(profile, dictionary)$changed_ids,
    expected
  )

  profile$metadata$value <- c(
    "d0a0b467e3690e24da457227664db8c255afc5aa90790887be00a3e6658de3f0",
    react_dictionary_version()$manifest_sha256
  )
  expect_identical(
    .profile_contract_rebase_ids(profile, dictionary)$changed_ids,
    expected
  )

  profile$metadata$value <- c(
    "70ba0bd048725b3763205a633988dcfee4789c275a9aab8d2659b72f7d9ecd83",
    react_dictionary_version()$manifest_sha256
  )
  expect_identical(
    .profile_contract_rebase_ids(profile, dictionary)$changed_ids,
    expected
  )
})

test_that("free-text source profiling retains presence but never respondent text", {
  old_dictionary <- .reactextract_env$dictionary
  on.exit(assign("dictionary", old_dictionary, envir = .reactextract_env), add = TRUE)
  dictionary <- react_dictionary()
  specs <- dictionary$synthetic_profile_specs
  occurrence_id <- specs$occurrence_id[
    specs$round_id == "react1.r01" & specs$profile_kind == "free_text"
  ][[1L]]
  occurrence <- dictionary$occurrences[
    dictionary$occurrences$occurrence_id == occurrence_id, , drop = FALSE
  ]
  dictionary$synthetic_profile_specs$review_state <- "candidate"
  dictionary$synthetic_profile_specs$review_state[
    dictionary$synthetic_profile_specs$occurrence_id == occurrence_id
  ] <- "approved"
  .reactextract_env$dictionary <- dictionary
  secret <- "DO NOT RETAIN THIS RESPONDENT TEXT"
  round <- data.frame(U_PASSCODE = paste0("key-", 1:4), check.names = FALSE)
  round[[occurrence$variable]] <- c(secret, "another answer", NA, "")
  profile <- react_profile_source(
    react_files(list(react1.r01 = round)), rounds = "REACT1_R01",
    progress = FALSE, include_routing = FALSE, include_overall = FALSE
  )
  expect_equal(
    profile$text_presence$count[profile$text_presence$status == "present"], 2L
  )
  expect_false(any(grepl(secret, unlist(profile), fixed = TRUE)))
})

test_that("missing-state generation preserves coded and database missingness", {
  occurrence <- data.frame(
    occurrence_id = "occ_test", round_id = "react1.r01",
    variable = "TEST", data_type = "NUMBER", stringsAsFactors = FALSE
  )
  source <- list(
    seed = 91L,
    profile = list(
      missingness = data.frame(
        occurrence_id = "occ_test", round_id = "react1.r01",
        status = c("database_missing", "coded:-91", "outside_safe_support"),
        count = c(100, 200, 150),
        stringsAsFactors = FALSE
      ),
      round_denominators = data.frame(
        round_id = "react1.r01", count = 1000, stringsAsFactors = FALSE
      )
    )
  )
  states <- .synthetic_missing_states(source, occurrence, 10000L)
  expect_lt(abs(sum(states == "database_missing") / 10000 - 0.1), 0.02)
  expect_lt(abs(sum(states == "coded:-91") / 10000 - 0.2), 0.02)
  expect_lt(abs(sum(states == "outside_safe_support") / 10000 - 0.15), 0.02)
  values <- .apply_synthetic_missing_states(rep(5, length(states)), states, occurrence)
  expect_true(all(is.na(values[states == "database_missing"])))
  expect_true(all(is.na(values[states == "outside_safe_support"])))
  expect_true(all(values[states == "coded:-91"] == -91))
})

test_that("complete profiling creates separately controlled all-round tables", {
  old_dictionary <- .reactextract_env$dictionary
  on.exit(assign("dictionary", old_dictionary, envir = .reactextract_env), add = TRUE)
  dictionary <- react_dictionary()
  joined <- merge(
    dictionary$occurrences, dictionary$synthetic_profile_specs,
    by = c("occurrence_id", "round_id")
  )
  joined <- joined[
    joined$profile_kind == "integer" & joined$bin_spec_id == "age_years_v1",
    , drop = FALSE
  ]
  compatible <- split(seq_len(nrow(joined)), paste(joined$variable, joined$bin_spec_id))
  compatible <- compatible[vapply(compatible, length, integer(1L)) >= 2L]
  selected <- joined[compatible[[1L]][1:2], , drop = FALSE]
  dictionary$synthetic_profile_specs$review_state <- "candidate"
  dictionary$synthetic_profile_specs$review_state[
    dictionary$synthetic_profile_specs$occurrence_id %in% selected$occurrence_id
  ] <- "approved"
  .reactextract_env$dictionary <- dictionary
  round_files <- lapply(seq_len(nrow(selected)), function(index) {
    data <- data.frame(U_PASSCODE = paste0("key-", 1:3), check.names = FALSE)
    data[[selected$variable[[index]]]] <- c(20, 30, NA)
    data
  })
  names(round_files) <- selected$round_id
  profile <- react_profile_source(
    react_files(round_files), rounds = selected$round_id,
    progress = FALSE, include_routing = FALSE, include_overall = TRUE
  )
  expect_equal(nrow(profile$distribution_groups), 1L)
  expect_equal(profile$distribution_groups$round_count, 2L)
  expect_equal(sum(profile$overall_numeric_bin_counts$count), 4L)
  safe <- react_prepare_profile_export(profile)
  expect_true(all(safe$overall_numeric_bin_counts$suppressed))
})

test_that("targeted repair replaces only requested occurrence aggregates", {
  old_dictionary <- .reactextract_env$dictionary
  on.exit(assign("dictionary", old_dictionary, envir = .reactextract_env), add = TRUE)
  dictionary <- react_dictionary()
  candidate <- merge(
    dictionary$occurrences[
      dictionary$occurrences$round_id == "react1.r01" &
        dictionary$occurrences$data_type == "NUMBER", , drop = FALSE
    ],
    dictionary$synthetic_profile_specs[
      dictionary$synthetic_profile_specs$profile_kind == "integer", , drop = FALSE
    ],
    by = "occurrence_id"
  )
  candidate <- candidate[nzchar(candidate$bin_spec_id), , drop = FALSE]
  occurrence <- candidate[1L, , drop = FALSE]
  dictionary$synthetic_profile_specs$review_state <- "candidate"
  dictionary$synthetic_profile_specs$review_state[
    dictionary$synthetic_profile_specs$occurrence_id == occurrence$occurrence_id
  ] <- "approved"
  .reactextract_env$dictionary <- dictionary

  make_round <- function(values) {
    data <- data.frame(U_PASSCODE = paste0("key-", seq_along(values)))
    data[[occurrence$variable]] <- values
    data
  }
  base <- react_profile_source(
    react_files(list(react1.r01 = make_round(c(rep(20, 20), -91)))),
    rounds = "REACT1_R01", progress = FALSE,
    include_routing = FALSE, include_overall = TRUE
  )
  safe_base <- react_prepare_profile_export(base)
  repair <- react_profile_source(
    react_files(list(react1.r01 = make_round(c(rep(30, 20), -92)))),
    rounds = "REACT1_R01", occurrence_ids = occurrence$occurrence_id,
    progress = FALSE, include_routing = FALSE, include_overall = FALSE
  )
  result <- react_repair_profile(safe_base, repair)
  expect_true("coded:-92" %in% result$missingness$status)
  expect_false("coded:-91" %in% result$missingness$status)
  expect_false("overall_numeric_bin_counts" %in% names(result))
  metadata <- stats::setNames(result$metadata$value, result$metadata$key)
  expect_identical(unname(metadata[["profile_scope"]]), "complete_with_targeted_repair")
  expect_identical(unname(metadata[["disclosure_approval"]]), "not_approved")
})
