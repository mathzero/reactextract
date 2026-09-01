test_that("the v5 contract is fixed, reviewed and fully resolvable", {
  dictionary <- react_dictionary()
  expect_equal(nrow(dictionary$synthetic_outcomes), 2L)
  expect_equal(nrow(dictionary$synthetic_dependencies), 22L)
  expect_equal(nrow(dictionary$synthetic_profile_overrides), 37L)
  expect_true(all(dictionary$synthetic_dependencies$review_state == "approved"))
  expect_setequal(
    dictionary$synthetic_outcomes$outcome_id,
    c("react1_pcr_positive", "react2_igg_positive")
  )
  expanded <- .dependency_specs(dictionary, dictionary$rounds$round_id)
  expect_equal(length(unique(expanded$dependency_id)), 22L)
  expect_equal(length(unique(expanded$round_id)), 25L)
  expect_true(all(vapply(seq_len(nrow(expanded)), function(index) {
    nrow(.dependency_occurrences(expanded[index, , drop = FALSE], dictionary)) > 0L
  }, logical(1L))))
  dependency_occurrences <- .bind_rows(lapply(seq_len(nrow(expanded)), function(index) {
    .dependency_occurrences(expanded[index, , drop = FALSE], dictionary)
  }), dictionary$occurrences[0, , drop = FALSE])
  expect_true(all(dependency_occurrences$data_type == "NUMBER"))
  expect_false(any(grepl("OTH|OTHER|TXT", dependency_occurrences$variable)))
  profile_specs <- .approved_profile_specs(dictionary)
  overridden <- profile_specs[
    profile_specs$occurrence_id %in% dictionary$synthetic_profile_overrides$occurrence_id,
    , drop = FALSE
  ]
  expect_equal(nrow(overridden), 37L)
  expect_true(all(overridden$profile_kind == "categorical"))
  expect_true(all(startsWith(overridden$support_source, "public_support:")))

  development <- .development_synthetic_profile()
  expect_no_error(.validate_dependency_profile_contract(
    development, dictionary, rounds = "all"
  ))
  incomplete <- development
  incomplete$dependency_counts <- incomplete$dependency_counts[-1L, , drop = FALSE]
  expect_error(
    .validate_dependency_profile_contract(incomplete, dictionary, rounds = "all"),
    "incomplete"
  )
  zero_denominator <- development
  zero_denominator$round_denominators$count[[1L]] <- 0
  expect_error(
    .validate_dependency_profile_contract(
      zero_denominator, dictionary, rounds = "all"
    ),
    "positive source denominator"
  )
})

test_that("two-dimensional complementary suppression protects rows and columns", {
  table <- expand.grid(
    dependency_id = "dep.test", round_id = "react1.r01",
    outcome_id = "outcome", predictor_id = "predictor",
    outcome_level = c("negative", "positive"),
    predictor_level = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )
  table$count <- c(5, 100, 100, 100, 100, 100)
  protected <- .sdc_dependency_table(table, react_sdc_policy())
  for (level in unique(protected$outcome_level)) {
    expect_false(sum(protected$suppressed[protected$outcome_level == level]) == 1L)
  }
  for (level in unique(protected$predictor_level)) {
    expect_false(sum(protected$suppressed[protected$predictor_level == level]) == 1L)
  }
  expect_true(all(is.na(protected$count[protected$suppressed])))
  expect_true(all(protected$count[!protected$suppressed] %% 5 == 0))
})

test_that("suppressed outcome margins protect every matching joint cell", {
  profile <- .development_synthetic_profile()
  profile$outcome_counts <- data.frame(
    outcome_id = "react1_pcr_positive", round_id = "react1.r01",
    outcome_level = c("negative", "positive", "missing"),
    count = c(100, 5, 120), suppressed = FALSE,
    stringsAsFactors = FALSE
  )
  profile$dependency_counts <- expand.grid(
    dependency_id = "dep.react1.age", round_id = "react1.r01",
    outcome_id = "react1_pcr_positive", predictor_id = "age_band",
    outcome_level = c("negative", "positive", "missing"),
    predictor_level = c("18_24", "25_34", "35_44"),
    stringsAsFactors = FALSE
  )
  profile$dependency_counts$count <- c(
    30, 2, 40, 35, 2, 40, 35, 1, 40
  )
  profile$dependency_counts$suppressed <- FALSE

  protected <- react_prepare_profile_export(profile)
  protected_levels <- protected$outcome_counts$outcome_level[
    protected$outcome_counts$suppressed
  ]
  expect_true("positive" %in% protected_levels)
  for (level in protected_levels) {
    rows <- protected$dependency_counts$outcome_level == level
    expect_true(all(protected$dependency_counts$suppressed[rows]), info = level)
    expect_true(all(is.na(protected$dependency_counts$count[rows])), info = level)
  }
})

test_that("dependency source profiling returns only complete aggregate grids", {
  dictionary <- react_dictionary()
  round_id <- "react1.r01"
  specs <- .dependency_specs(dictionary, round_id)
  fields <- unique(c(
    .dependency_outcome_variables(round_id, "react1_pcr_positive"),
    unlist(lapply(seq_len(nrow(specs)), function(index) {
      .dependency_occurrences(specs[index, , drop = FALSE], dictionary)$variable
    }), use.names = FALSE)
  ))
  values <- stats::setNames(rep(list(rep(1, 20L)), length(fields)), fields)
  values$RESULT <- rep(c("Detected", "Not Detected"), each = 10L)
  values$FINALRESULT <- values$RESULT
  values$LAB <- "Eurofin"
  values$CT_VALUE1 <- c(rep(25, 10L), rep(0, 10L))
  values$CT_VALUE2 <- c(rep(26, 10L), rep(0, 10L))
  frame <- data.frame(U_PASSCODE = sprintf("fixture-%02d", 1:20), values,
                      check.names = FALSE)
  profile <- react_profile_dependencies_source(
    react_files(list(react1.r01 = frame)), rounds = round_id, progress = FALSE
  )
  expect_setequal(profile$outcome_counts$outcome_level,
                  c("negative", "positive", "missing"))
  expect_equal(length(unique(profile$dependency_counts$dependency_id)), nrow(specs))
  expect_false(any(grepl("PASSCODE|SUBJECT", names(profile), ignore.case = TRUE)))
  safe <- react_prepare_profile_export(profile)
  expect_true("suppressed" %in% names(safe$dependency_counts))
})

test_that("exact blank laboratory results are PCR missing, never negative", {
  values <- data.frame(
    RESULT = c(
      "Detected", "Not Detected", "Void", " ", NA_character_,
      "not detected", "Pending review"
    ),
    LAB = rep("Eurofin", 7L),
    CT_VALUE1 = c(25, 0, NA, NA, NA, 0, 0),
    CT_VALUE2 = c(26, 0, NA, NA, NA, 0, 0),
    stringsAsFactors = FALSE
  )
  expect_identical(
    .dependency_react1_outcome(values, "react1.r01"),
    c(
      "positive", "negative", "missing", "missing", "missing",
      "missing", "missing"
    )
  )
  expect_true(.dependency_lab_result_missing(" "))
  expect_false(.dependency_lab_result_missing("Not Detected"))
  expect_true(.dependency_lab_result_unknown("not detected"))
  expect_true(.dependency_lab_result_unknown("Pending review"))
  expect_false(.dependency_lab_result_unknown("Detected"))
  expect_false(.dependency_lab_result_unknown("Not Detected"))
})

test_that("dependency profiling flags unknown PCR labels and fails closed", {
  dictionary <- react_dictionary()
  round_id <- "react1.r11"
  specs <- .dependency_specs(dictionary, round_id)
  fields <- unique(c(
    .dependency_outcome_variables(round_id, "react1_pcr_positive"),
    unlist(lapply(seq_len(nrow(specs)), function(index) {
      .dependency_occurrences(specs[index, , drop = FALSE], dictionary)$variable
    }), use.names = FALSE)
  ))
  values <- stats::setNames(rep(list(rep(1, 20L)), length(fields)), fields)
  values$RESULT <- c(
    rep("Detected", 5L), rep("Not Detected", 10L),
    rep("Pending review", 3L), "Void", " "
  )
  values$NGENE_CTVALUE <- c(rep(25, 5L), rep(0, 15L))
  values$EGENE_CTVALUE <- c(rep(26, 5L), rep(0, 15L))
  frame <- data.frame(
    U_PASSCODE = sprintf("fixture-%02d", 1:20), values,
    check.names = FALSE
  )
  profile <- react_profile_dependencies_source(
    react_files(list(react1.r11 = frame)), rounds = round_id,
    progress = FALSE
  )
  counts <- stats::setNames(
    profile$outcome_counts$count, profile$outcome_counts$outcome_level
  )
  expect_equal(unname(counts[c("positive", "negative", "missing")]),
               c(5L, 10L, 5L))
  issue <- profile$issues[
    profile$issues$code == "unrecognised_lab_result", , drop = FALSE
  ]
  expect_equal(nrow(issue), 1L)
  expect_equal(issue$affected_count, 3L)
})

test_that("REACT-2 round 1 profiles and checks both antibody result fields", {
  dictionary <- react_dictionary()
  round_id <- "react2.r01"
  specs <- .dependency_specs(dictionary, round_id)
  fields <- unique(c(
    .dependency_outcome_variables(round_id, "react2_igg_positive"),
    unlist(lapply(seq_len(nrow(specs)), function(index) {
      .dependency_occurrences(specs[index, , drop = FALSE], dictionary)$variable
    }), use.names = FALSE)
  ))
  values <- stats::setNames(rep(list(rep(1, 20L)), length(fields)), fields)
  values$NEWRESULT <- rep(c(0, 2), each = 10L)
  values$NEWRESULT_2 <- ifelse(values$NEWRESULT == 0, 1, 2)
  values$NEWRESULT_2[[1L]] <- 2
  frame <- data.frame(
    U_PASSCODE = sprintf("fixture-%02d", 1:20), values,
    check.names = FALSE
  )
  profile <- react_profile_dependencies_source(
    react_files(list(react2.r01 = frame)), rounds = round_id, progress = FALSE
  )
  expect_true(all(c("NEWRESULT", "NEWRESULT_2") %in% fields))
  issue <- profile$issues[
    profile$issues$code == "react2_outcome_companion_conflict", , drop = FALSE
  ]
  expect_equal(nrow(issue), 1L)
  expect_equal(issue$affected_count, 1L)
  safe <- react_prepare_profile_export(profile)
  protected <- safe$issues[
    safe$issues$code == "react2_outcome_companion_conflict", , drop = FALSE
  ]
  expect_equal(nrow(protected), 0L)
  expect_identical(
    .dependency_react2_outcome(
      data.frame(NEWRESULT_2 = 1:4), "react2.r01"
    ),
    c("negative", "positive", "non_evaluable", "non_evaluable")
  )
})

test_that("v5 generation makes laboratory outcome fields coherent", {
  source <- react_synthetic(
    react_synthetic_profile(development = TRUE),
    n_per_round = c(REACT1_R01 = 500L, REACT2_S5_R01 = 500L),
    seed = 501L
  )
  result <- react_extract(
    source, rounds = c("REACT1_R01", "REACT2_S5_R01"),
    families = "all", progress = FALSE
  )
  expect_identical(
    result$manifest$value[result$manifest$key == "synthetic_dependency_model"],
    "outcome_centred_v5"
  )

  react1 <- result$raw_data[result$raw_data$round_id == "react1.r01", , drop = FALSE]
  expect_true(all(react1$CT_VALUE1[react1$RESULT == "Not Detected"] == 0))
  expect_true(all(react1$CT_VALUE1[react1$RESULT == "Detected"] > 0))
  expect_true(all(is.na(react1$CT_VALUE1[react1$RESULT == "Void"])))
  expect_true(any(react1$RESULT == "Detected"))

  react2 <- result$raw_data[result$raw_data$round_id == "react2.r01", , drop = FALSE]
  expect_true(all(react2$NEWRESULT_2[react2$NEWRESULT %in% c(0, 1)] == 1))
  expect_true(all(react2$NEWRESULT_2[react2$NEWRESULT %in% c(2, 3)] == 2))
  expect_true(all(react2$NEWRESULT_2[react2$NEWRESULT %in% 4] == 3))
  expect_true(all(react2$NEWRESULT_2[react2$NEWRESULT %in% c(5, 6, 7)] == 4))
  age_bounds <- list(`1` = 18:24, `2` = 25:34, `3` = 35:44, `4` = 45:54,
                     `5` = 55:64, `6` = 65:74, `7` = 75:120)
  for (code in names(age_bounds)) {
    ages <- react2$U_AGE[react2$AGE_GROUP %in% as.numeric(code)]
    expect_true(all(ages %in% age_bounds[[code]]))
  }

  forced_noncompletion <- .set_dependency_outcome(
    data.frame(NEWRESULT = 7, NEWRESULT_2 = 4, ABATTEMPT = NA_real_,
               ABCOMP = NA_real_),
    "non_evaluable", "react2.r01", "react2_igg_positive", source
  )
  expect_equal(forced_noncompletion$NEWRESULT, 7)
  expect_equal(forced_noncompletion$ABATTEMPT, 1)
  expect_equal(forced_noncompletion$ABCOMP, 3)

  raw_missing <- rep(c(NA_character_, "Void", " "), each = 100L)
  preserved_missing <- .set_dependency_outcome(
    data.frame(RESULT = raw_missing, stringsAsFactors = FALSE),
    rep("missing", length(raw_missing)), "react1.r01",
    "react1_pcr_positive", source
  )$RESULT
  expect_true(all(is.na(preserved_missing) |
    preserved_missing %in% c("Void", " ")))
  expect_true(any(is.na(preserved_missing)))
  expect_true(any(preserved_missing == " ", na.rm = TRUE))
})

test_that("v5 conditional streams retain a reviewed outcome association", {
  result <- react_extract(
    react_synthetic(
      react_synthetic_profile(development = TRUE),
      n_per_round = c(REACT1_R01 = 4000L), seed = 911L
    ),
    rounds = "REACT1_R01", families = "all", progress = FALSE
  )
  raw <- result$raw_data
  positive <- raw$RESULT == "Detected"
  second_age_band <- raw$U_AGE >= 13 & raw$U_AGE <= 17
  expect_gt(
    mean(second_age_band[positive], na.rm = TRUE),
    mean(second_age_band[!positive & raw$RESULT == "Not Detected"], na.rm = TRUE)
  )
})

test_that("the approved v5 profile enables dependencies by default", {
  result <- react_extract(
    react_synthetic(n_per_round = c(REACT1_R01 = 20L), seed = 1L),
    concepts = "health.preexisting.overweight",
    rounds = "REACT1_R01", progress = FALSE
  )
  expect_identical(
    result$manifest$value[result$manifest$key == "synthetic_dependency_model"],
    "outcome_centred_v5"
  )
  expect_identical(
    result$manifest$value[
      result$manifest$key == "synthetic_dependency_pair_count"
    ],
    "22"
  )
})

test_that("approved v5 contains exact result supports and complete outcome tables", {
  dictionary <- react_dictionary()
  profile <- react_synthetic_profile(refresh = TRUE)

  decisions <- list(
    c("react1.r02", "FINALRESULT", "Rejected", "missing"),
    c("react1.r11", "RESULT", "negative", "negative"),
    c("react1.r13", "RESULT", "ambiguous", "missing")
  )
  for (decision in decisions) {
    occurrence_id <- .lab_result_occurrence_id(
      decision[[1L]], decision[[2L]], dictionary
    )
    support <- .lab_result_support(occurrence_id, dictionary)
    expect_identical(
      support$outcome_state[support$raw_value == decision[[3L]]],
      decision[[4L]],
      info = paste(decision, collapse = "/")
    )
    released <- profile$categorical_counts[
      profile$categorical_counts$occurrence_id == occurrence_id &
        profile$categorical_counts$value == decision[[3L]],
      , drop = FALSE
    ]
    expect_equal(nrow(released), 1L, info = paste(decision, collapse = "/"))
  }

  expected_outcomes <- 19L * 3L + 6L * 4L
  expect_equal(nrow(profile$outcome_counts), expected_outcomes)
  expect_setequal(unique(profile$outcome_counts$round_id), dictionary$rounds$round_id)
  expect_equal(nrow(profile$dependency_specs), 22L)
  expect_no_error(.validate_dependency_profile_contract(
    profile, dictionary, rounds = "all"
  ))
})

test_that("administrative and unknown codes never index dependency labels", {
  dictionary <- react_dictionary()
  spec_for <- function(dependency_id, round_id) {
    rows <- .dependency_specs(dictionary, round_id)
    rows[rows$dependency_id == dependency_id, , drop = FALSE]
  }

  contact <- spec_for("dep.react1.contact", "react1.r02")
  expect_identical(
    .dependency_predictor_state(
      data.frame(COVIDCON = c(-92, -91, -77, 1, 2, 3, 99)),
      contact, dictionary
    ),
    c("missing", "missing", "missing", "confirmed", "suspected_only",
      "none", "missing")
  )

  cases <- list(
    list("dep.react1.covid_history", "react1.r02", "COVIDA", 1,
         "test_confirmed"),
    list("dep.react1.economic_activity", "react1.r02", "EMPL", 6,
         "retired"),
    list("dep.react1.region", "react1.r02", "REGION", 7, "london"),
    list("dep.react1.imd", "react1.r02", "IMD_DECILE", 9,
         "q5_least_deprived")
  )
  for (case in cases) {
    spec <- spec_for(case[[1L]], case[[2L]])
    values <- stats::setNames(
      list(c(-92, -91, -77, -66, -99, -555, case[[4L]], 999)),
      case[[3L]]
    )
    state <- .dependency_predictor_state(
      as.data.frame(values, check.names = FALSE), spec, dictionary
    )
    expect_true(all(state[1:6] == "missing"), info = case[[1L]])
    expect_identical(state[[7L]], case[[5L]], info = case[[1L]])
    expect_identical(state[[8L]], "missing", info = case[[1L]])
  }
})

test_that("round-specific vaccination and ethnicity codes are interpreted exactly", {
  dictionary <- react_dictionary()
  spec_for <- function(dependency_id, round_id) {
    rows <- .dependency_specs(dictionary, round_id)
    rows[rows$dependency_id == dependency_id, , drop = FALSE]
  }

  vaccination_13 <- spec_for("dep.react1.vaccination", "react1.r13")
  expect_identical(
    .dependency_predictor_state(
      data.frame(VACCINE3 = c(1, 2, 3, 4, -92), VACCINE3SYM = NA_real_),
      vaccination_13, dictionary
    ),
    c("yes", "no", "uncertain_trial_or_unknown",
      "uncertain_trial_or_unknown", "missing")
  )
  vaccination_12 <- spec_for("dep.react1.vaccination", "react1.r12")
  expect_identical(
    .dependency_predictor_state(
      data.frame(VACCINE3 = 4, VACCINE3SYM = NA_real_),
      vaccination_12, dictionary
    ),
    "missing"
  )

  ethnicity_06 <- spec_for("dep.react1.ethnicity", "react1.r06")
  ethnicity_07 <- spec_for("dep.react1.ethnicity", "react1.r07")
  ethnicity_19 <- spec_for("dep.react1.ethnicity", "react1.r19")
  expect_identical(
    .dependency_predictor_state(data.frame(ETHNIC = 20), ethnicity_06, dictionary),
    "withheld_or_missing"
  )
  expect_identical(
    .dependency_predictor_state(data.frame(ETHNIC = 20), ethnicity_07, dictionary),
    "other"
  )
  expect_identical(
    .dependency_predictor_state(data.frame(ETHNIC = 20), ethnicity_19, dictionary),
    "other"
  )
})

test_that("scalar dependency setters emit only support valid for that round", {
  dictionary <- react_dictionary()
  source <- list(seed = 715L)
  spec_for <- function(dependency_id, round_id) {
    rows <- .dependency_specs(dictionary, round_id)
    rows[rows$dependency_id == dependency_id, , drop = FALSE]
  }

  gender_cases <- list(
    react1.r01 = c(1, 2, 0, NA),
    react1.r02 = c(1, 2, NA, NA),
    react1.r04 = c(1, 2, 9, NA),
    react2.r01 = c(1, 2, NA, NA)
  )
  for (round_id in names(gender_cases)) {
    dependency_id <- if (startsWith(round_id, "react1")) {
      "dep.react1.gender"
    } else {
      "dep.react2.gender"
    }
    spec <- spec_for(dependency_id, round_id)
    generated <- .set_dependency_predictor(
      data.frame(U_GENDER = rep(999, 4L)),
      c("male", "female", "not_specified", "missing"),
      spec, dictionary, source
    )
    expect_equal(generated$U_GENDER, gender_cases[[round_id]], info = round_id)
    expect_true("not_specified" %in% .dependency_predictor_levels(spec, dictionary))
    if (round_id %in% c("react1.r02", "react2.r01")) {
      expect_false("not_specified" %in% .dependency_encodable_levels(spec, dictionary))
    }
  }

  ethnicity <- spec_for("dep.react1.ethnicity", "react1.r02")
  levels <- c("white", "mixed", "asian", "black", "other",
              "withheld_or_missing")
  generated <- .set_dependency_predictor(
    data.frame(ETHNIC = rep(999, length(levels))), levels,
    ethnicity, dictionary, source
  )
  support <- .dependency_public_support(
    .dependency_occurrences(ethnicity, dictionary), dictionary
  )
  expect_true(all(as.character(generated$ETHNIC) %in% support))
  expect_identical(
    .dependency_predictor_state(generated, ethnicity, dictionary), levels
  )

  age <- spec_for("dep.react1.age", "react1.r01")
  generated_age <- .set_dependency_predictor(
    data.frame(U_AGE = c(2, -92, 100)),
    c("missing", "18_24", "85_plus"), age, dictionary, source
  )
  expect_true(is.na(generated_age$U_AGE[[1L]]))
  expect_true(generated_age$U_AGE[[2L]] %in% 18:24)
  expect_true(generated_age$U_AGE[[3L]] %in% 85:120)
})

test_that("missing checkbox states remain missing after dependency assignment", {
  dictionary <- react_dictionary()
  source <- list(seed = 716L)
  spec_for <- function(dependency_id, round_id) {
    rows <- .dependency_specs(dictionary, round_id)
    rows[rows$dependency_id == dependency_id, , drop = FALSE]
  }
  cases <- list(
    list("dep.react1.contact", "missing"),
    list("dep.react1.symptoms", "missing")
  )
  for (case in cases) {
    spec <- spec_for(case[[1L]], "react1.r03")
    occurrences <- .dependency_occurrences(spec, dictionary)
    frame <- as.data.frame(
      stats::setNames(rep(list(1), nrow(occurrences)), occurrences$variable),
      check.names = FALSE
    )
    generated <- .set_dependency_predictor(
      frame, case[[2L]], spec, dictionary, source
    )
    expect_true(
      all(vapply(generated, function(value) all(is.na(value)), logical(1L))),
      info = case[[1L]]
    )
    expect_identical(
      .dependency_predictor_state(generated, spec, dictionary), case[[2L]],
      info = case[[1L]]
    )
  }
})

test_that("key-worker negative and unknown options retain their meaning", {
  dictionary <- react_dictionary()
  spec <- .dependency_specs(dictionary, "react1.r02")
  spec <- spec[spec$dependency_id == "dep.react1.key_worker", , drop = FALSE]
  occurrences <- .dependency_occurrences(spec, dictionary)
  blank <- as.data.frame(
    stats::setNames(rep(list(0), nrow(occurrences)), occurrences$variable),
    check.names = FALSE
  )

  unknown <- blank
  unknown$WORKTYP1_7 <- 1
  expect_identical(
    .dependency_predictor_state(unknown, spec, dictionary),
    "unknown_or_missing"
  )
  none <- blank
  none$WORKTYP2_9 <- 1
  expect_identical(.dependency_predictor_state(none, spec, dictionary), "none")

  source <- list(seed = 718L)
  generated <- .set_dependency_predictor(
    blank, "unknown_or_missing", spec, dictionary, source
  )
  expect_equal(generated$WORKTYP1_7, 1)
  expect_identical(
    .dependency_predictor_state(generated, spec, dictionary),
    "unknown_or_missing"
  )
})

test_that("all reviewed round dependencies finish in their sampled or missing state", {
  dictionary <- react_dictionary()
  source <- react_synthetic(
    react_synthetic_profile(development = TRUE),
    n_per_round = 12L, seed = 717L
  )
  specs <- .dependency_specs(dictionary, dictionary$rounds$round_id)
  registry <- .validate_registry(dictionary$source_registry)

  for (round_id in dictionary$rounds$round_id) {
    registry_row <- registry[
      registry$round_id == round_id, , drop = FALSE
    ]
    occurrences <- dictionary$occurrences[
      dictionary$occurrences$round_id == round_id, , drop = FALSE
    ]
    generated <- .read_synthetic_round(
      source, registry_row, occurrences, dictionary
    )$data
    local <- specs[specs$round_id == round_id, , drop = FALSE]
    for (outcome_id in unique(local$outcome_id)) {
      expected_outcome <- .sample_dependency_outcome(
        source, round_id, outcome_id, nrow(generated)
      )
      expect_identical(
        .dependency_outcome_state(generated, round_id, outcome_id),
        expected_outcome, info = paste(round_id, outcome_id)
      )
      outcome_specs <- local[local$outcome_id == outcome_id, , drop = FALSE]
      for (index in seq_len(nrow(outcome_specs))) {
        spec <- outcome_specs[index, , drop = FALSE]
        expected <- .sample_dependency_predictor(
          source, round_id, spec, expected_outcome, nrow(generated),
          dictionary = dictionary
        )
        observed <- .dependency_predictor_state(generated, spec, dictionary)
        expect_identical(
          observed, expected,
          info = paste(round_id, spec$dependency_id[[1L]])
        )
      }
    }
  }
})
