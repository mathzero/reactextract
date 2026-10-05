compact_rule_fixture <- function() {
  path <- tempfile(); dir.create(path)
  utils::untar(system.file("extdata", "synthetic-rules.tar.gz", package="reactextract"), exdir=path)
  names <- c("rules", "predicates", "bindings", "targets", "approvals", "manifest")
  stats::setNames(lapply(names, function(n) .read_literal_csv(file.path(path,paste0(n,".csv")))),names)
}

test_that("conditions are reused without losing exact bindings or approvals", {
  x <- compact_rule_fixture(); e <- .expand_synthetic_rules(x)
  expect_equal(nrow(x$rules),306L)
  expect_lt(length(unique(x$predicates$pattern_id)),nrow(x$rules)/3)
  new <- x$rules$decision_id=="response-options-v2-2026-09-25"
  expect_equal(sum(new),77L)
  expect_equal(length(unique(x$rules$pattern_id[new])),3L)
  expect_equal(nrow(e$response_options$rules),109L)
  expect_true(all(e$response_options$rules$review_state=="approved"))
  d <- react_dictionary()
  held <- d$occurrences$occurrence_id[d$occurrences$round_id=="react1.r15" &
    d$occurrences$variable %in% c("QISOLLEAVEREASON_1","QISOLLEAVEREASON_2","QISOLLEAVEREASON_12")]
  expect_false(any(held %in% e$response_options$targets$target_occurrence_id))
  bad <- x; bad$bindings <- bad$bindings[-1,]
  expect_error(.expand_synthetic_rules(bad),"binding")
  bad <- x; bad$predicates$input[1] <- "arbitrary_R"
  expect_error(.expand_synthetic_rules(bad),"inputs")
  bad <- x; bad$rules$pattern_id[1] <- "unknown"
  expect_error(.expand_synthetic_rules(bad),"references")
  bad <- x; bad$approvals$review_state[1] <- "candidate"
  expect_error(.validate_eligibility_context(.expand_synthetic_rules(bad)$eligibility,d),"approval")
  bad <- e$response_options; bad$rules$round_id[1] <- "react2.r01"
  expect_error(.validate_response_options(bad,d),"another round")
})

test_that("option restrictions propagate through downstream questions", {
  f <- eligibility_fixture(); o <- f$dictionary$occurrences
  option <- list(rules=data.frame(routing_rule_id="option",round_id="fixture.r01",rule_type="option_restriction",
    selected_code="1",review_state="approved",reviewed_by="mathzero",review_date="2000-01-01"),
    conditions=data.frame(routing_rule_id="option",clause_id="1",condition_order="1",context_key="age",
      parent_occurrence_id="",operator="gte",comparison_values_json="[18]"),
    targets=data.frame(routing_rule_id="option",target_occurrence_id="p"))
  data <- data.frame(P=1,CONFIRM=1,X=1,Y=1)
  z <- .apply_eligibility_context(data,o,f$dictionary,list(age=16),f$contract,TRUE,option)
  expect_true(all(is.na(z$data[c("P","X","Y")])) )
  expect_identical(z$option_skipped$P,TRUE)
  expect_identical(z$skipped$X,TRUE)
  expect_identical(z$skipped$Y,TRUE)
  z <- .apply_eligibility_context(data,o,f$dictionary,list(age=18),f$contract,TRUE,option)
  expect_identical(z$data,data)
})

test_that("all new restricted fields are reproducible in narrow requests", {
  s <- react_synthetic(n_per_round=10L,seed=903L); d <- react_dictionary()
  rules <- s$profile$response_options$rules
  ids <- rules$routing_rule_id[rules$decision_id=="response-options-v2-2026-09-25"]
  targets <- s$profile$response_options$targets
  selected <- d$occurrences[d$occurrences$occurrence_id %in% targets$target_occurrence_id[targets$routing_rule_id %in% ids],]
  checked <- 0L
  for (round in unique(selected$round_id)) {
    o <- d$occurrences[d$occurrences$round_id==round,]
    one <- selected[selected$round_id==round,]
    registry <- d$source_registry[d$source_registry$round_id==round,]
    all <- .read_synthetic_round(s,registry,o,d)
    narrow <- .read_synthetic_round(s,registry,.synthetic_generation_occurrences(d,one,round,s),d)
    for (v in one$variable) {
      expect_identical(narrow$data[[v]],all$data[[v]],info=paste(round,v))
      expect_identical(narrow$missing_reasons[[v]],all$missing_reasons[[v]],info=paste(round,v))
      checked <- checked+1L
    }
  }
  expect_equal(checked,77L)
})
