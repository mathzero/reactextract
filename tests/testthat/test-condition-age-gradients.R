condition_rounds <- c("REACT1_R08", "REACT1_R13", "REACT2_S5_R05")

condition_result <- local({
  result <- NULL
  function() {
    if (is.null(result)) {
      result <<- react_extract(
        react_synthetic(n_per_round = stats::setNames(rep(20000L, 3L), condition_rounds), seed = 2026L),
        families = c("health/preexisting-conditions", "people-households/demographics"),
        progress = FALSE
      )
    }
    result
  }
})

condition_fields <- function(round_id) {
  occurrences <- react_dictionary()$occurrences
  block <- .condition_block(occurrences[occurrences$round_id == round_id, , drop = FALSE])
  stats::setNames(block$variable, block$condition)
}

test_that("the condition age gradients are pinned public figures", {
  gradients <- .condition_gradients()
  expect_identical(gradients$release, "condition-age-gradients-v1")
  expect_true(all(nzchar(gradients$sources$licence)))
  expect_true(all(gradients$figures$source_id %in% gradients$sources$source_id))
  path <- system.file("extdata", "condition-age-gradients-v1-provenance.csv", package = "reactextract")
  provenance <- .read_literal_csv(path)
  expect_identical(provenance$sha256, .condition_gradients_sha256)
  expect_identical(provenance$data_origin, "public_statistics_only")

  broken <- gradients
  broken$figures <- broken$figures[-2L, , drop = FALSE]
  expect_error(.validate_condition_gradients(broken), "contiguous")
})

test_that("prevalence lookup extends the youngest band and falls back across sex", {
  figures <- data.frame(
    condition = "x", sex = c("male", "male", "female", "female"),
    age_lower = c(16L, 50L, 16L, 50L), age_upper = c(49L, NA, 49L, NA),
    prevalence_percent = c(2, 10, 4, 20), source_id = "s",
    stringsAsFactors = FALSE
  )
  expect_equal(
    .condition_prevalence(figures, "x", c(5, 30, 80, 30, 80), c("male", "male", "male", "female", NA)),
    c(2, 2, 10, 4, 15)
  )
  figures$prevalence_percent[[1L]] <- 0
  expect_equal(.condition_prevalence(figures, "x", 20, "male"), .condition_prevalence_floor)
})

test_that("calibration keeps the published odds ratios, each target and none of these", {
  offsets <- cbind(a = c(-3, -1, 1), b = c(0, 0, 0), c = c(-2, 0, 2))
  rownames(offsets) <- c("x", "y", "z")
  weights <- c(50, 30, 20)
  targets <- c(0.10, 0.20, 0.30)
  averaged <- function(fit) {
    nodes <- .condition_normal_nodes()
    vapply(1:3, function(k) rowMeans(stats::plogis(outer(fit$logits[, k], fit$shared_sd * nodes, `+`))), numeric(3))
  }
  check <- function(fit) {
    cell <- averaged(fit)
    # Published odds ratios between cells, averaged over the shared term
    expect_equal(stats::qlogis(cell) - offsets, matrix(stats::qlogis(cell[1L, ]) - offsets[1L, ], 3, 3, byrow = TRUE),
                 tolerance = 1e-7, ignore_attr = TRUE)
    expect_equal(colSums(cell * weights / sum(weights)), targets, tolerance = 1e-8, ignore_attr = TRUE)
  }
  none <- function(fit) {
    nodes <- .condition_normal_nodes()
    clear <- Reduce(`*`, lapply(1:3, function(k) 1 - stats::plogis(outer(fit$logits[, k], fit$shared_sd * nodes, `+`))))
    sum(weights / sum(weights) * rowMeans(clear))
  }

  independent <- .calibrate_condition_block(offsets, weights, targets)
  expect_equal(independent$shared_sd, 0)
  check(independent)

  shared <- .calibrate_condition_block(offsets, weights, targets, none_target = 0.6)
  expect_gt(shared$shared_sd, 0)
  check(shared)
  expect_equal(none(shared), 0.6, tolerance = 1e-5)
  expect_equal(shared$none_scale, 1)

  # Enough people already report nothing: some tick no box at all
  scaled <- .calibrate_condition_block(offsets, weights, targets, none_target = 0.4)
  expect_equal(scaled$shared_sd, 0)
  expect_equal(scaled$none_scale * scaled$all_clear, 0.4, tolerance = 1e-8)
})

test_that("synthetic conditions follow the published age gradient", {
  result <- condition_result()
  raw <- result$raw_data
  figures <- .condition_gradients()$figures
  for (survey in condition_rounds) {
    round_id <- react_dictionary()$rounds$round_id[react_dictionary()$rounds$survey_id == survey]
    fields <- condition_fields(round_id)
    keep <- raw$survey_id == survey
    age <- raw$U_AGE[keep]
    for (condition in intersect(c("diabetes", "hypertension", "heart_disease"), unique(figures$condition))) {
      value <- raw[[fields[[condition]]]][keep]
      answered <- value %in% c(0, 1) & !is.na(age)
      young <- answered & age >= 18 & age < 45
      old <- answered & age >= 65
      expect_gt(mean(value[old] == 1), 3 * mean(value[young] == 1),
                label = paste(survey, condition, "65+ versus 18-44"))
    }
    # Sex follows the sources where they give it
    gender <- raw$U_GENDER[keep]
    share <- function(condition, rows) mean(raw[[fields[[condition]]]][keep][rows] == 1)
    answered <- raw[[fields[["none_of_these"]]]][keep] %in% c(0, 1) & !is.na(age)
    young <- answered & age >= 18 & age < 35
    older <- answered & age >= 55
    expect_gt(share("anxiety", young & gender %in% 2), 1.5 * share("anxiety", young & gender %in% 1),
              label = paste(survey, "anxiety, women versus men aged 18-34"))
    expect_gt(share("diabetes", older & gender %in% 1), 1.2 * share("diabetes", older & gender %in% 2),
              label = paste(survey, "diabetes, men versus women aged 55+"))

    # Across adult age bands, the log-odds of the common conditions move one
    # for one with the published log-odds
    source <- react_synthetic(n_per_round = 10L)
    occurrences <- react_dictionary()$occurrences
    block <- .condition_block(occurrences[occurrences$round_id == round_id, , drop = FALSE])
    graded <- .condition_gradients()$conditions
    graded <- graded$condition[graded$gradient != "none"]
    for (index in which(block$condition %in% graded)) {
      if (.condition_probability_one(source, block[index, , drop = FALSE]) < 0.03) next
      condition <- block$condition[[index]]
      value <- raw[[block$variable[[index]]]][keep]
      answered <- value %in% c(0, 1) & !is.na(age) & age >= 18
      band <- cut(age[answered], c(17, 34, 44, 54, 64, 74, Inf))
      size <- tapply(value[answered], band, length)
      observed <- stats::qlogis((tapply(value[answered] == 1, band, sum) + 0.5) / (size + 1))
      published <- stats::qlogis(tapply(
        .condition_prevalence(figures, condition, age[answered], NA), band, mean
      ) / 100)
      if (stats::sd(published) < 0.3) next
      slope <- stats::coef(stats::lm(observed ~ published, weights = size))[[2L]]
      expect_gt(slope, 0.75, label = paste(survey, condition, "log-odds slope"))
      expect_lt(slope, 1.25, label = paste(survey, condition, "log-odds slope"))
    }
  }
})

test_that("each round keeps the profile prevalence of every condition", {
  result <- condition_result()
  raw <- result$raw_data
  source <- react_synthetic(n_per_round = 10L)
  occurrences <- react_dictionary()$occurrences
  for (survey in condition_rounds) {
    round_id <- react_dictionary()$rounds$round_id[react_dictionary()$rounds$survey_id == survey]
    block <- .condition_block(occurrences[occurrences$round_id == round_id, , drop = FALSE])
    keep <- raw$survey_id == survey
    for (index in seq_len(nrow(block))) {
      target <- .condition_probability_one(source, block[index, , drop = FALSE])
      if (is.na(target)) next
      value <- raw[[block$variable[[index]]]][keep]
      answered <- value %in% c(0, 1)
      tolerance <- 4 * sqrt(target * (1 - target) / sum(answered))
      expect_lt(abs(mean(value[answered] == 1) - target), tolerance,
                label = paste(survey, block$condition[[index]], "prevalence"))
    }
  }
  calibration <- result$manifest$value[result$manifest$key == "synthetic_condition_calibration"]
  expect_match(calibration, "react1.r08:conditions=16", fixed = TRUE)
  expect_false(any(result$issues$code == "condition_none_below_profile"))
})

test_that("none of these is consistent and unanswered boxes stay missing", {
  result <- condition_result()
  raw <- result$raw_data
  for (survey in condition_rounds) {
    round_id <- react_dictionary()$rounds$round_id[react_dictionary()$rounds$survey_id == survey]
    fields <- condition_fields(round_id)
    keep <- raw$survey_id == survey
    none <- raw[[fields[["none_of_these"]]]][keep]
    answered <- none %in% c(0, 1)
    if (startsWith(survey, "REACT1")) expect_gt(sum(!answered), 0)
    conditions <- setdiff(names(fields), c("none_of_these", "overweight"))
    any_condition <- rep(FALSE, sum(keep))
    for (condition in conditions) {
      value <- raw[[fields[[condition]]]][keep]
      # Not answered: the same missing value or code as "none of these"
      expect_identical(value[!answered], none[!answered], label = paste(survey, condition))
      expect_true(all(value[answered] %in% c(0, 1)))
      any_condition <- any_condition | (answered & value == 1)
    }
    expect_true(all(none[answered & any_condition] == 0))
    expect_true(all(none[answered & !any_condition] == 1))
  }
})

test_that("conditions do not depend on which families or concepts are extracted", {
  source <- react_synthetic(n_per_round = c(REACT1_R08 = 400L, REACT2_S5_R05 = 300L), seed = 7L)
  narrow <- react_extract(source, concepts = "health.preexisting.diabetes", progress = FALSE)
  family <- react_extract(source, families = "health/preexisting-conditions", progress = FALSE)
  both <- react_extract(
    source, families = c("health/preexisting-conditions", "people-households/demographics"),
    progress = FALSE
  )
  demographics <- react_extract(source, families = "people-households/demographics", progress = FALSE)
  again <- react_extract(
    react_synthetic(n_per_round = c(REACT1_R08 = 400L, REACT2_S5_R05 = 300L), seed = 7L),
    concepts = "health.preexisting.diabetes", progress = FALSE
  )

  diabetes <- "health.preexisting.diabetes"
  expect_identical(narrow$data$SUBJECT_ID, both$data$SUBJECT_ID)
  expect_identical(narrow$data[[diabetes]], family$data[[diabetes]])
  expect_identical(narrow$data[[diabetes]], both$data[[diabetes]])
  expect_identical(narrow$data[[diabetes]], again$data[[diabetes]])
  expect_identical(
    family$data[["health.preexisting.none_of_these"]],
    both$data[["health.preexisting.none_of_these"]]
  )
  age <- "people_households.demographics.age__field__U_AGE"
  expect_identical(both$data[[age]], demographics$data[[age]])
  model <- function(result) result$manifest$value[result$manifest$key == "synthetic_condition_model"]
  expect_identical(model(demographics), "not_applied")
  expect_identical(model(narrow), "public_age_sex_gradients_calibrated_to_profile")
  expect_identical(
    both$data[["people_households.demographics.gender"]],
    demographics$data[["people_households.demographics.gender"]]
  )

  changed <- react_extract(
    react_synthetic(n_per_round = c(REACT1_R08 = 400L, REACT2_S5_R05 = 300L), seed = 8L),
    concepts = "health.preexisting.diabetes", progress = FALSE
  )
  expect_false(identical(narrow$data[[diabetes]], changed$data[[diabetes]]))
})

test_that("without none of these, each condition is calibrated on its own answers", {
  source <- react_synthetic(n_per_round = c(REACT1_R08 = 4000L), seed = 3L)
  dictionary <- react_dictionary()
  occurrences <- dictionary$occurrences[dictionary$occurrences$round_id == "react1.r08", , drop = FALSE]
  block <- .condition_block(occurrences)
  context <- occurrences[occurrences$variable %in% c("U_AGE", "U_GENDER"), , drop = FALSE]
  used <- rbind(block[block$condition %in% c("diabetes", "asthma"), names(occurrences)], context)
  data <- data.frame(SUBJECT_ID = seq_len(4000L))
  for (index in seq_len(nrow(used))) {
    data[[used$variable[[index]]]] <- .generate_synthetic_value(source, used[index, , drop = FALSE], 4000L)$value
  }
  before <- data
  after <- .apply_condition_age_gradients(data, used, source, dictionary)
  expect_identical(after$data$U_AGE, before$U_AGE)
  for (variable in block$variable[block$condition %in% c("diabetes", "asthma")]) {
    answered <- before[[variable]] %in% c(0, 1)
    expect_identical(after$data[[variable]][!answered], before[[variable]][!answered])
    expect_true(all(after$data[[variable]][answered] %in% c(0, 1)))
  }
  expect_match(after$summary, "none_target=NA", fixed = TRUE)
})
