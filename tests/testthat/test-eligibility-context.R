test_that("candidate/partial/unknown contracts cannot enter generation", {
  f <- eligibility_fixture(); d <- f$dictionary; x <- f$contract
  expect_silent(.validate_eligibility_context(x, d))
  x$rules$review_state <- "candidate"
  expect_error(.validate_eligibility_context(x, d), "approval")
  x <- f$contract; x$conditions$context_key[1] <- "unknown_administration"
  expect_error(.validate_eligibility_context(x, d), "Unknown")
  x <- f$contract; x$conditions$parent_occurrence_id[1] <- "p"
  expect_error(.validate_eligibility_context(x, d), "ambiguous")
  x <- f$contract; x$conditions$parent_occurrence_id[2] <- "missing_parent"
  expect_error(.validate_eligibility_context(x, d), "missing or belongs")
  x <- f$contract; x$conditions$operator[1] <- "R_expression"
  expect_error(.validate_eligibility_context(x, d), "Unsupported")
  x <- f$contract; x$conditions$comparison_values_json[1] <- "system('oops')"
  expect_error(.validate_eligibility_context(x, d), "numeric JSON")
  x <- f$contract; x$conditions$comparison_values_json[1] <- "[]"
  expect_error(.validate_eligibility_context(x, d), "requires one")
  x <- f$contract; x$rules$round_id <- "another.r01"
  expect_error(.validate_eligibility_context(x, d), "another round")
})

test_that("complete AND/OR gates retain boundaries and cannot be bypassed", {
  f <- eligibility_fixture(); o <- f$dictionary$occurrences
  age <- c(12, 13, 16, 17, 18, 54, 55, NA, 18, 18, 13)
  data <- data.frame(P = c(rep(1, 8), 2, -77, 1), CONFIRM = c(1, 1, 2, 1, 2, 2, -77, 1, 1, 1, -91),
    X = 1, Y = as.Date("2020-01-01"))
  z <- .apply_eligibility_context(data, o, f$dictionary, list(age = age), f$contract)
  expected <- c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  expect_identical(!is.na(z$X), expected)
  expect_identical(!is.na(z$Y), expected)
  expect_s3_class(z$Y, "Date")
  expect_identical(z$P, data$P)
  expect_identical(z$CONFIRM, data$CONFIRM)
  expect_error(.apply_eligibility_context(data, o, f$dictionary, list(), f$contract), "age context")
  expect_error(.apply_eligibility_context(data[, -2], o, f$dictionary, list(age = age), f$contract), "parent unavailable")
})

test_that("household adult branch does not require a parent confirmation", {
  f <- eligibility_fixture(); x <- f$contract
  x$conditions <- data.frame(routing_rule_id = "new", clause_id = c("adult", "child", "child", rep("parent", 3)),
    condition_order = c(1, 1, 2, 1:3), context_key = c("age", "age", "age", "age", "age", ""),
    parent_occurrence_id = c(rep("", 5), "confirm"), operator = c("gte", "gte", "lt", "gte", "lt", "equals"),
    comparison_values_json = c("[18]", "[5]", "[13]", "[13]", "[18]", "[1]"))
  data <- data.frame(P = 1, CONFIRM = c(NA, NA, 1, 2, -77, NA), X = 1, Y = 1)
  z <- .apply_eligibility_context(data, f$dictionary$occurrences, f$dictionary,
    list(age = c(18, 12, 13, 17, 16, NA)), x)
  expect_identical(!is.na(z$X), c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
})

test_that("selection closes over context and ordinary ancestors", {
  f <- eligibility_fixture(); o <- f$dictionary$occurrences
  x <- .eligibility_required_occurrences(f$dictionary, o[o$variable == "Y", ], f$contract)
  expect_setequal(x$variable, c("Y", "X", "P", "CONFIRM", "U_AGE"))
  expect_identical(x, .eligibility_required_occurrences(f$dictionary, o, f$contract))
})

test_that("cycles are rejected instead of leaving unprocessed fields", {
  f <- eligibility_fixture()
  f$contract$conditions$parent_occurrence_id[2] <- "y"
  expect_error(.apply_eligibility_context(data.frame(P = 1, CONFIRM = 1, X = 1, Y = 1),
    f$dictionary$occurrences, f$dictionary, list(age = 18), f$contract), "cycle")
})

test_that("the installed preview uses only pinned approved context rules", {
  source <- react_synthetic(n_per_round = 1L)
  expect_equal(nrow(source$profile$eligibility$rules), 197L)
  expect_true(all(source$profile$eligibility$rules$review_state == "approved"))
  expect_equal(length(unique(source$profile$eligibility$targets$target_occurrence_id)), 772L)
  broken <- source$profile; broken$eligibility <- NULL
  expect_error(react_synthetic(broken), "eligibility inputs")
  broken <- source$profile; broken$eligibility$conditions$comparison_values_json[1] <- "[99]"
  expect_error(react_synthetic(broken), "eligibility inputs")
  expect_null(react_synthetic_profile(version = "v5")$eligibility)
  expect_identical(.synthetic_profile_metadata(source$profile)[["profile_release"]],
    "react-synthetic-profile-v6-preview")
})

test_that("approved real-round gates survive generation, shared states and narrow selection", {
  s <- react_synthetic(n_per_round = 30L, seed = 607L)
  d <- react_dictionary(); contract <- s$profile$eligibility
  for (round in c("react1.r02", "react1.r04", "react1.r07", "react1.r08", "react1.r10", "react1.r14", "react1.r19")) {
    o <- d$occurrences[d$occurrences$round_id == round, , drop = FALSE]
    reg <- d$source_registry[d$source_registry$round_id == round, , drop = FALSE]
    z <- .read_synthetic_round(s, reg, o, d)
    context <- .participation_context(s, round, 30L, d)
    selected <- o[o$occurrence_id %in% contract$targets$target_occurrence_id, , drop = FALSE]
    variables <- stats::setNames(o$variable, o$occurrence_id)
    for (i in seq_len(nrow(selected))) {
      target <- selected$occurrence_id[i]; v <- selected$variable[i]
      ids <- contract$targets$routing_rule_id[contract$targets$target_occurrence_id == target]
      eligible <- rep(FALSE, 30L)
      for (id in ids) eligible <- eligible | .eligibility_context_true(z$data, id, contract$conditions, variables, context)
      blocked <- !eligible & !context$nonresponse
      expect_true(all(is.na(z$data[[v]][blocked])), info = paste(round, v))
      expect_true(all(z$missing_reasons[[v]][blocked] == "structural_skip_questionnaire"), info = paste(round, v))
    }
    one <- selected[1, , drop = FALSE]
    needed <- .synthetic_generation_occurrences(d, one, round, s)
    narrow <- .read_synthetic_round(s, reg, needed, d)
    expect_identical(narrow$data[[one$variable]], z$data[[one$variable]], info = round)
    expect_identical(narrow$missing_reasons[[one$variable]], z$missing_reasons[[one$variable]], info = round)
  }
})
