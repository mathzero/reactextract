# One checksum-pinned runtime bundle; full questionnaire evidence lives in react_wiki.
.synthetic_rules_sha256 <- "37e6c1f3990aefe668dfd01a6bd29cca056f83fee1346a14abe389623f79892d"

.synthetic_rules_contract <- function(refresh = FALSE) {
  if (!refresh && !is.null(.reactextract_env$synthetic_rules)) return(.reactextract_env$synthetic_rules)
  archive <- system.file("extdata", "synthetic-rules.tar.gz", package = "reactextract")
  if (!nzchar(archive) || !identical(.sha256_file(archive), .synthetic_rules_sha256))
    stop("Synthetic rule archive checksum mismatch.", call. = FALSE)
  path <- file.path(tempdir(), paste0("reactextract-rules-", .synthetic_rules_sha256))
  if (!file.exists(file.path(path, "checksums.csv"))) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    utils::untar(archive, exdir = path)
  }
  names <- c("rules", "predicates", "bindings", "targets", "approvals", "manifest")
  h <- .read_literal_csv(file.path(path, "checksums.csv"))
  if (!setequal(h$file, paste0(names, ".csv")) || anyDuplicated(h$file))
    stop("Invalid synthetic rule file set.", call. = FALSE)
  for (i in seq_len(nrow(h))) if (!identical(.sha256_file(file.path(path, h$file[i])), h$sha256[i]))
    stop("Synthetic rule file checksum mismatch.", call. = FALSE)
  x <- stats::setNames(lapply(names, function(name) .read_literal_csv(file.path(path, paste0(name, ".csv")))), names)
  m <- stats::setNames(x$manifest$value, x$manifest$key)
  if (!identical(m[["schema_version"]], "1") ||
      !identical(m[["dictionary_manifest_sha256"]], react_dictionary_version()$manifest_sha256[[1L]]))
    stop("Synthetic rule schema/dictionary mismatch.", call. = FALSE)
  expanded <- .expand_synthetic_rules(x)
  .validate_eligibility_context(expanded$eligibility, react_dictionary())
  .validate_response_options(expanded$response_options, react_dictionary())
  expanded$pattern_count <- length(unique(x$predicates$pattern_id))
  .reactextract_env$synthetic_rules <- expanded
  expanded
}

.expand_synthetic_rules <- function(x) {
  r <- x$rules; p <- x$predicates; b <- x$bindings; a <- x$approvals
  if (anyNA(r) || anyNA(p) || anyNA(b) || anyNA(a) ||
      anyDuplicated(r$routing_rule_id) || anyDuplicated(a$decision_id) ||
      !setequal(r$pattern_id, p$pattern_id) || !setequal(r$decision_id, a$decision_id) ||
      any(!b$routing_rule_id %in% r$routing_rule_id) ||
      anyDuplicated(b[c("routing_rule_id", "input")]) ||
      anyDuplicated(p[c("pattern_id", "clause_id", "condition_order")]) ||
      any(!grepl("^(age|parent[1-9][0-9]*)$", p$input)) ||
      any(!r$rule_type %in% c("mandatory_gate", "option_restriction")))
    stop("Invalid compact rule references or inputs.", call. = FALSE)
  r <- cbind(r, a[match(r$decision_id, a$decision_id), c("review_state", "reviewed_by", "review_date")])
  conditions <- lapply(seq_len(nrow(r)), function(i) {
    rows <- p[p$pattern_id == r$pattern_id[i], , drop = FALSE]
    bindings <- b[b$routing_rule_id == r$routing_rule_id[i], , drop = FALSE]
    parents <- rows$input != "age"
    if (!setequal(bindings$input, rows$input[parents]) || any(!nzchar(bindings$parent_occurrence_id)))
      stop("Missing or unused compact rule binding.", call. = FALSE)
    ids <- rep("", nrow(rows))
    ids[parents] <- bindings$parent_occurrence_id[match(rows$input[parents], bindings$input)]
    data.frame(routing_rule_id = r$routing_rule_id[i], clause_id = rows$clause_id,
      condition_order = rows$condition_order, context_key = ifelse(parents, "", "age"),
      parent_occurrence_id = ids, operator = rows$operator, comparison_values_json = rows$comparison_values_json)
  })
  conditions <- do.call(rbind, conditions)
  split_contract <- function(type) {
    rules <- r[r$rule_type == type, , drop = FALSE]
    list(rules = rules, conditions = conditions[conditions$routing_rule_id %in% rules$routing_rule_id, , drop = FALSE],
      targets = x$targets[x$targets$routing_rule_id %in% rules$routing_rule_id, , drop = FALSE])
  }
  if (!setequal(r$routing_rule_id, x$targets$routing_rule_id)) stop("Invalid compact rule targets.", call. = FALSE)
  list(eligibility = split_contract("mandatory_gate"), response_options = split_contract("option_restriction"))
}

.eligibility_context_contract <- function(refresh = FALSE) .synthetic_rules_contract(refresh)$eligibility
.response_options_contract <- function(refresh = FALSE) .synthetic_rules_contract(refresh)$response_options
