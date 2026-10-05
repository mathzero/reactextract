# Shared controlled comparisons for question and option restrictions.
.validate_rule_conditions <- function(contract, dictionary) {
  required <- list(
    rules = c("routing_rule_id", "round_id", "rule_type", "review_state", "reviewed_by", "review_date"),
    conditions = c("routing_rule_id", "clause_id", "condition_order", "context_key", "parent_occurrence_id", "operator", "comparison_values_json"),
    targets = c("routing_rule_id", "target_occurrence_id"))
  for (name in names(required)) {
    if (!is.data.frame(contract[[name]]) || !all(required[[name]] %in% names(contract[[name]])))
      stop("Incomplete eligibility contract: ", name, ".", call. = FALSE)
    if (anyNA(contract[[name]][required[[name]]])) stop("Missing eligibility contract cells.", call. = FALSE)
  }
  r <- contract$rules; c <- contract$conditions; t <- contract$targets
  if (any(r$review_state != "approved" | r$reviewed_by != "mathzero" | !nzchar(r$review_date)))
    stop("Eligibility rules require recorded mathzero approval; candidates cannot run.", call. = FALSE)
  if (anyDuplicated(r$routing_rule_id)) stop("Duplicate rule IDs.", call. = FALSE)
  if (!setequal(r$routing_rule_id, c$routing_rule_id) || !setequal(r$routing_rule_id, t$routing_rule_id))
    stop("Eligibility rules need both conditions and targets.", call. = FALSE)
  if (any(!nzchar(c$clause_id)) || anyDuplicated(c[c("routing_rule_id", "clause_id", "condition_order")]))
    stop("Invalid eligibility clauses/order.", call. = FALSE)
  context <- nzchar(c$context_key)
  if (any(context == nzchar(c$parent_occurrence_id)) || any(!c$context_key[context] %in% "age"))
    stop("Unknown or ambiguous eligibility context source.", call. = FALSE)
  allowed <- c("equals", "in", "not_in", "gt", "gte", "lt", "lte", "selected_any", "is_missing", "not_missing")
  if (any(!c$operator %in% allowed)) stop("Unsupported eligibility comparison.", call. = FALSE)
  for (i in seq_len(nrow(c))) {
    v <- .parse_comparison_values(c$comparison_values_json[i])
    if (c$operator[i] %in% c("equals", "gt", "gte", "lt", "lte") && length(v) != 1L)
      stop("Scalar eligibility comparison requires one value.", call. = FALSE)
    if (c$operator[i] %in% c("in", "not_in") && !length(v))
      stop("Set eligibility comparison requires values.", call. = FALSE)
    if (c$operator[i] %in% c("selected_any", "is_missing", "not_missing") && length(v))
      stop("Unary eligibility comparison cannot contain values.", call. = FALSE)
    if (context[i] && c$operator[i] %in% c("is_missing", "not_missing", "selected_any"))
      stop("Age eligibility must use an explicit public boundary.", call. = FALSE)
  }
  o <- dictionary$occurrences
  for (tab in list(data.frame(rule = c$routing_rule_id[!context], occurrence = c$parent_occurrence_id[!context]),
                   data.frame(rule = t$routing_rule_id, occurrence = t$target_occurrence_id))) {
    found <- match(tab$occurrence, o$occurrence_id)
    if (anyNA(found) || any(o$round_id[found] != r$round_id[match(tab$rule, r$routing_rule_id)]))
      stop("Eligibility occurrence is missing or belongs to another round.", call. = FALSE)
  }
  invisible(TRUE)
}

.validate_eligibility_context <- function(contract, dictionary) {
  .validate_rule_conditions(contract, dictionary)
  r <- contract$rules
  if (!"replaces_rule_id" %in% names(r) || anyDuplicated(r$replaces_rule_id) ||
      any(r$rule_type != "mandatory_gate") ||
      !all(r$replaces_rule_id %in% dictionary$routing_rules$routing_rule_id))
    stop("Invalid eligibility rule/replacement references.", call. = FALSE)
  invisible(TRUE)
}

.evaluate_rule_conditions <- function(data, rule_id, conditions, variables, context = NULL, strict_missing = FALSE) {
  cc <- conditions[conditions$routing_rule_id == rule_id, , drop = FALSE]
  eligible <- rep(FALSE, nrow(data))
  for (clause in unique(cc$clause_id)) {
    rows <- cc[cc$clause_id == clause, , drop = FALSE]
    hit <- rep(TRUE, nrow(data))
    for (i in seq_len(nrow(rows))) {
      if ("context_key" %in% names(rows) && nzchar(rows$context_key[i])) {
        value <- context[[rows$context_key[i]]]
        if (is.null(value) || length(value) != nrow(data)) stop("Required age context unavailable.", call. = FALSE)
      } else {
        variable <- unname(variables[rows$parent_occurrence_id[i]])
        if (is.na(variable) || !variable %in% names(data)) stop("Required eligibility parent unavailable.", call. = FALSE)
        value <- data[[variable]]
      }
      hit <- hit & .condition_true(value, rows$operator[i], rows$comparison_values_json[i], strict_missing = strict_missing)
    }
    eligible <- eligible | hit
  }
  eligible
}

.eligibility_context_true <- function(data, rule_id, conditions, variables, context) {
  .evaluate_rule_conditions(data, rule_id, conditions, variables, context, strict_missing = TRUE)
}

.eligibility_required_occurrences <- function(dictionary, selected, contract, options = NULL) {
  .validate_eligibility_context(contract, dictionary)
  if (!is.null(options)) .validate_response_options(options, dictionary)
  o <- dictionary$occurrences
  needed <- unique(selected$occurrence_id)
  repeat {
    before <- needed
    ids <- contract$targets$routing_rule_id[contract$targets$target_occurrence_id %in% needed]
    parents <- contract$conditions$parent_occurrence_id[contract$conditions$routing_rule_id %in% ids]
    needed <- unique(c(needed, parents[nzchar(parents)]))
    if (!is.null(options)) {
      ids <- options$targets$routing_rule_id[options$targets$target_occurrence_id %in% needed]
      parents <- options$conditions$parent_occurrence_id[options$conditions$routing_rule_id %in% ids]
      needed <- unique(c(needed, parents[nzchar(parents)]))
    }
    # Include ordinary ancestors too; their own mandatory gates may add context parents.
    ids <- dictionary$routing_targets$routing_rule_id[dictionary$routing_targets$target_occurrence_id %in% needed]
    ids <- setdiff(ids, contract$rules$replaces_rule_id)
    needed <- unique(c(needed, dictionary$routing_conditions$parent_occurrence_id[
      dictionary$routing_conditions$routing_rule_id %in% ids]))
    if (setequal(before, needed)) break
  }
  rounds <- unique(o$round_id[o$occurrence_id %in% needed])
  needed <- unique(c(needed, o$occurrence_id[o$round_id %in% rounds & o$variable == "U_AGE"]))
  o[o$occurrence_id %in% needed, , drop = FALSE]
}

.apply_eligibility_context <- function(data, occurrences, dictionary, context, contract, return_skips = FALSE, options = NULL) {
  .validate_eligibility_context(contract, dictionary)
  variables <- stats::setNames(occurrences$variable, occurrences$occurrence_id)
  present <- names(variables)[variables %in% names(data)]
  r <- dictionary$routing_rules
  r <- r[r$review_state == "approved" & r$rule_type %in% c("gate", "terminate", "other_text") &
    !r$routing_rule_id %in% contract$rules$replaces_rule_id, , drop = FALSE]
  cc <- dictionary$routing_conditions[dictionary$routing_conditions$routing_rule_id %in% r$routing_rule_id, , drop = FALSE]
  tt <- dictionary$routing_targets[dictionary$routing_targets$routing_rule_id %in% r$routing_rule_id, , drop = FALSE]
  pending <- unique(c(tt$target_occurrence_id, contract$targets$target_occurrence_id, options$targets$target_occurrence_id))
  pending <- pending[pending %in% present]
  skipped <- list(); option_skipped <- list()
  while (length(pending)) {
    progressed <- FALSE
    for (target in pending) {
      ordinary <- unique(tt$routing_rule_id[tt$target_occurrence_id == target])
      mandatory <- unique(contract$targets$routing_rule_id[contract$targets$target_occurrence_id == target])
      option_ids <- options$targets$routing_rule_id[options$targets$target_occurrence_id == target]
      parents <- unique(c(cc$parent_occurrence_id[cc$routing_rule_id %in% ordinary],
        contract$conditions$parent_occurrence_id[contract$conditions$routing_rule_id %in% mandatory],
        options$conditions$parent_occurrence_id[options$conditions$routing_rule_id %in% option_ids]))
      parents <- parents[nzchar(parents)]
      if (any(parents %in% pending)) next
      if (!all(parents %in% present)) stop("Required eligibility parent unavailable; generate ancestors first.", call. = FALSE)
      answer_ok <- rep(!length(ordinary), nrow(data))
      for (rule in ordinary) answer_ok <- answer_ok | .gate_rule_eligibility(data, rule, cc, variables, strict_missing = TRUE)
      context_ok <- rep(!length(mandatory), nrow(data))
      for (rule in mandatory) context_ok <- context_ok |
        .eligibility_context_true(data, rule, contract$conditions, variables, context)
      field <- variables[[target]]
      skipped[[field]] <- !answer_ok | !context_ok
      option_ok <- rep(TRUE, nrow(data))
      for (rule in option_ids) option_ok <- option_ok &
        .eligibility_context_true(data, rule, options$conditions, variables, context)
      option_skipped[[field]] <- !option_ok & !skipped[[field]]
      codes <- unique(r$false_value[r$routing_rule_id %in% ordinary & r$false_value_kind == "exact_code" & nzchar(r$false_value)])
      if (length(codes) > 1L) stop("Conflicting questionnaire skip codes.", call. = FALSE)
      if (length(codes)) data[[field]][!answer_ok] <- .coerce_like(codes, data[[field]])
      else data[[field]][!answer_ok] <- NA
      # Mandatory restrictions cannot be bypassed by any ordinary OR route.
      # No new raw skip code is invented where its historic value is unknown.
      data[[field]][!context_ok | !option_ok] <- NA
      pending <- setdiff(pending, target)
      progressed <- TRUE
    }
    if (!progressed) stop("Eligibility routing contains a cycle.", call. = FALSE)
  }
  if (return_skips) list(data = data, skipped = skipped, option_skipped = option_skipped) else data
}
