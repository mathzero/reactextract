#!/usr/bin/env Rscript

# Full-size check using only fictional participants and approved aggregates.
suppressPackageStartupMessages(library(reactextract))
started <- proc.time()[["elapsed"]]
source <- react_synthetic(n_per_round = 1000L, seed = 601L)
stopifnot(!is.null(source$profile$participation))
result <- react_extract(source, output = "wide", progress = TRUE)
stopifnot(
  nrow(result$data) == 25000L,
  nrow(result$raw_data) == 25000L,
  !any(c("raw_values", "harmonised_values") %in% names(result)),
  length(unique(result$observations$SUBJECT_ID)) == 25000L,
  all(result$observations$visit_number == 1L),
  all(result$observations$total_visits == 1L)
)

policy <- source$profile$participation$occurrences
checked <- 0L
eligibility_checked <- 0L
dictionary <- react_dictionary()
contract <- source$profile$eligibility
for (round in unique(result$raw_data$round_id)) {
  rows <- result$raw_data[result$raw_data$round_id == round, , drop = FALSE]
  context <- reactextract:::.participation_context(source, round, nrow(rows), dictionary)
  shared <- context$nonresponse
  if (startsWith(round, "react1.")) stopifnot(identical(shared, !is.na(rows$SFREPORTFIG) & rows$SFREPORTFIG == -77))
  governed <- policy$variable[policy$round_id == round &
    policy$stage == "individual" & policy$governed == "TRUE"]
  for (field in intersect(governed, names(rows))) {
    type <- policy$data_type[policy$round_id == round & policy$variable == field]
    valid <- if (type == "DATE" || startsWith(type, "TIMESTAMP")) {
      all(is.na(rows[[field]][shared]))
    } else {
      all(as.character(rows[[field]][shared]) == "-77")
    }
    if (!isTRUE(valid)) stop("Shared non-response mismatch: ", round, "/", field)
    checked <- checked + 1L
  }
  adult_only <- policy$variable[policy$round_id == round & policy$minimum_age == "18"]
  ineligible <- is.na(rows$U_AGE) | rows$U_AGE < 18
  for (field in intersect(adult_only, names(rows))) {
    values <- suppressWarnings(as.numeric(rows[[field]][ineligible]))
    if (any(!is.na(values) & values >= 0))
      stop("Adult-only answer in ineligible observation: ", round, "/", field)
  }
  occurrences <- dictionary$occurrences[dictionary$occurrences$round_id == round, , drop = FALSE]
  variables <- stats::setNames(occurrences$variable, occurrences$occurrence_id)
  targets <- intersect(occurrences$occurrence_id, contract$targets$target_occurrence_id)
  for (target in targets) {
    rules <- contract$targets$routing_rule_id[contract$targets$target_occurrence_id == target]
    eligible <- rep(FALSE, nrow(rows))
    for (rule in rules) eligible <- eligible | reactextract:::.eligibility_context_true(
      rows, rule, contract$conditions, variables, list(age = rows$U_AGE))
    values <- rows[[variables[[target]]]][!eligible & !shared]
    if (!all(is.na(values))) stop("Ineligible questionnaire answer: ", round, "/", variables[[target]])
    eligibility_checked <- eligibility_checked + 1L
  }
}
print(result)
manifest <- stats::setNames(result$manifest$value, result$manifest$key)
stopifnot(manifest[["synthetic_participation_assumed_rounds"]] == paste0("react2.r0",1:6,collapse="|"),
  manifest[["synthetic_participation_unestimated_rounds"]] == "")
options_checked <- 0L
for (round in unique(source$profile$response_options$rules$round_id)) {
  rows <- result$raw_data[result$raw_data$round_id == round, , drop = FALSE]
  occurrences <- dictionary$occurrences[dictionary$occurrences$round_id == round, , drop = FALSE]
  contract <- source$profile$response_options
  for (id in intersect(occurrences$occurrence_id, contract$targets$target_occurrence_id)) {
    field <- occurrences$variable[match(id, occurrences$occurrence_id)]
    eligible <- reactextract:::.response_option_eligible(rows, id, occurrences, list(age=rows$U_AGE), contract)
    values <- rows[[field]][!eligible]
    if (any(!is.na(values) & values != -77)) stop("Unavailable option populated: ",round,"/",field)
    options_checked <- options_checked+1L
  }
}
stopifnot(options_checked == 109L)
message("PASS: all 109 vaccination, work/travel/contact-option restrictions.")
message("PASS: shared non-response in ", checked,
  " governed field-rounds; no substantive under-18 smoking/vaping answers.")
stopifnot(eligibility_checked == 772L)
message("PASS: approved eligibility across ", eligibility_checked, " field-rounds.")
message(sprintf("Elapsed: %.1f seconds", proc.time()[["elapsed"]] - started))
