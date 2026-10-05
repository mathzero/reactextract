# Option-specific validation and sampling masks; shared comparisons live in eligibility_context.R.
.validate_response_options <- function(contract, dictionary) {
  .validate_rule_conditions(contract, dictionary)
  r <- contract$rules; cc <- contract$conditions; tt <- contract$targets
  if (!"selected_code" %in% names(r) || any(r$rule_type != "option_restriction" | r$selected_code != "1"))
    stop("Response-option restrictions require recorded approval and selected code 1.", call. = FALSE)
  if (anyDuplicated(tt$target_occurrence_id) || anyDuplicated(tt$routing_rule_id))
    stop("Invalid response-option references.", call. = FALSE)
  # This release handles binary checkbox fields only, never whole categorical questions.
  for (id in tt$target_occurrence_id) {
    codes <- dictionary$response_options$return_value[dictionary$response_options$occurrence_id == id]
    substantive <- suppressWarnings(as.numeric(codes)); substantive <- substantive[!is.na(substantive) & substantive >= 0]
    if (!1 %in% substantive || any(!substantive %in% c(0, 1)))
      stop("Response-option target is not a verified binary checkbox.", call. = FALSE)
  }
  if (any(cc$parent_occurrence_id %in% tt$target_occurrence_id))
    stop("Option-to-option dependencies are not supported by this release.", call. = FALSE)
  invisible(TRUE)
}

.response_option_eligible <- function(data, occurrence_id, occurrences, context, contract) {
  rules <- contract$targets$routing_rule_id[contract$targets$target_occurrence_id == occurrence_id]
  eligible <- rep(TRUE, nrow(data))
  variables <- stats::setNames(occurrences$variable, occurrences$occurrence_id)
  for (rule in rules) eligible <- eligible & .eligibility_context_true(data, rule, contract$conditions, variables, context)
  eligible
}

.apply_response_options <- function(data, occurrences, context, contract, reasons) {
  targets <- intersect(occurrences$occurrence_id, contract$targets$target_occurrence_id)
  for (id in targets) {
    v <- occurrences$variable[match(id, occurrences$occurrence_id)]
    if (!v %in% names(data)) next
    unavailable <- !.response_option_eligible(data, id, occurrences, context, contract)
    data[[v]][unavailable] <- NA
    if (is.null(reasons[[v]])) reasons[[v]] <- rep("", nrow(data))
    # If the whole question was unasked, retain that more general explanation.
    broader_skip <- reasons[[v]] %in% c("structural_skip_questionnaire", "structural_skip_age", "survey_nonresponse")
    reasons[[v]][unavailable & !broader_skip] <- "structural_skip_option"
  }
  list(data = data, reasons = reasons)
}
