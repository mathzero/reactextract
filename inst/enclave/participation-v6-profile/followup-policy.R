# Standalone enclave follow-up. Does not modify the installed package or v5 data.
pf6_dictionary_hash <- "28d03054e4b284cd44a040cf473991c441184739e84a7f6235392b1142a79236"
pf6_provenance_hash <- "720cdd6f4348315b58ff9bad4b10d0f996e28db541c219ce40f7da93f4932ebc"
pf6_age_levels <- c("under_5", "5_11", "12", "13_15", "16_17", "18_54", "55_plus", "missing_or_invalid")
pf6_quality_levels <- c("database_missing", "negative", "outside_public_range", "whole_years", "fractional_years")
pf6_mail_levels <- c("database_missing", "group_1", "group_2", "group_3", "other_code_not_released")
pf6_internal <- "enclave_internal_not_disclosure_approved"
pf6_pending <- "awaiting_normal_enclave_disclosure_approval"

pf6_meta <- function(x, key) {
  z <- x$manifest$value[x$manifest$key == key]
  if (length(z) != 1L || is.na(z)) stop("Missing or repeated manifest key: ", key)
  as.character(z)
}

pf6_check_dictionary <- function() {
  if (!requireNamespace("reactextract", quietly = TRUE)) stop("Install reactextract from your existing offline bundle first.")
  v <- reactextract::react_dictionary_version()
  if (!identical(v$manifest_sha256, pf6_dictionary_hash)) stop("This kit requires the rc14 dictionary used by reactextract 0.5.4-0.5.6. Do not mix dictionaries.")
  reactextract::react_dictionary()
}

pf6_counts <- function(round, table, field, reference, x, xlevels,
                       y = NULL, ylevels = NULL, kind = "state", reference_kind = "none") {
  z <- pv6_counts(round, table, field, reference, x, xlevels, y, ylevels)
  z$field_kind <- kind
  z$reference_kind <- reference_kind
  z
}

pf6_validate_counts <- function(d) {
  required <- c("round_id", "table", "field", "reference", "state", "reference_state", "count")
  if (!is.data.frame(d) || !nrow(d) || !all(required %in% names(d)) ||
      !is.numeric(d$count) || anyNA(d) || any(!is.finite(d$count)) ||
      any(d$count < 0 | d$count != floor(d$count))) stop("Expected complete, unsuppressed internal aggregate counts. Do not use the exported CSV folder.")
  keys <- setdiff(names(d), "count")
  if (anyDuplicated(d[keys])) stop("Duplicate aggregate cells.")
  invisible(TRUE)
}

pf6_reuse <- function(original) {
  pf6_check_dictionary()
  if (!identical(pf6_meta(original, "schema"), "participation-diagnostic-v1") ||
      !identical(pf6_meta(original, "status"), pf6_internal) ||
      !identical(pf6_meta(original, "dictionary_manifest_sha256"), pf6_dictionary_hash) ||
      !identical(pf6_meta(original, "provenance_sha256"), pf6_provenance_hash) ||
      "suppressed" %in% names(original$counts)) stop("Use the original participation_diagnostic object, not participation_export or returned CSVs.")
  pf6_validate_counts(original$counts)
  d <- original$counts
  d <- d[d$table == "code77_agreement", , drop = FALSE]
  if (!nrow(d)) stop("The original object contains no participation comparisons.")
  out <- anchors <- list()
  for (round in unique(d$round_id)) {
    z <- d[d$round_id == round, , drop = FALSE]
    # A comparison anchor is NOT an approved completion indicator.
    preferred <- if (grepl("^react1[.]", round)) c("FEELUN", "INDCONF", "INDCONFSF", "SMOKENOW") else c("ABCOMP", "INDCONFSF", "ABATTEMPT")
    anchor <- preferred[preferred %in% z$reference][1L]
    if (is.na(anchor)) stop("No recognised comparison anchor in ", round)
    z <- z[z$reference == anchor, , drop = FALSE]
    if (!all(z$state %in% c("77", "not77")) ||
        !all(z$reference_state %in% pv6_marker_levels)) stop("Unexpected original comparison domain.")
    groups <- split(seq_len(nrow(z)), z$field)
    for (idx in groups) {
      a <- z[idx, , drop = FALSE]
      if (nrow(a) != 14L) stop("Incomplete original comparison table.")
      direction <- ifelse(a$state == "77" & a$reference_state != "code_minus77", "field_only_code77",
        ifelse(a$state != "77" & a$reference_state == "code_minus77", "reference_only_code77", "matching_remainder_withheld"))
      levels <- c("field_only_code77", "reference_only_code77", "matching_remainder_withheld")
      totals <- vapply(levels, function(s) sum(a$count[direction == s]), numeric(1))
      out[[length(out) + 1L]] <- data.frame(round_id = round, table = "code77_discordance",
        field = a$field[1L], reference = anchor, state = levels, reference_state = "all",
        count = unname(totals), field_kind = "code77_discordance", reference_kind = "comparison_anchor")
    }
    anchors[[length(anchors) + 1L]] <- data.frame(round_id = round, field = anchor,
      role = "comparison_anchor_not_completion_indicator", status = "selected_from_existing_comparisons")
  }
  list(counts = do.call(rbind, out), inventory = do.call(rbind, anchors), issues = original$issues,
    manifest = data.frame(key = c("schema", "status", "package_version", "dictionary_manifest_sha256",
      "provenance_sha256", "source_schema", "source_package_version", "requested_rounds", "database_queries"),
      value = c("participation-reexport-v2", pf6_internal, as.character(utils::packageVersion("reactextract")),
        pf6_dictionary_hash, pf6_provenance_hash, "participation-diagnostic-v1", pf6_meta(original, "package_version"),
        pf6_meta(original, "requested_rounds"), "0")))
}

pf6_age <- function(x) {
  n <- suppressWarnings(as.numeric(as.character(x)))
  out <- as.character(cut(n, c(0, 5, 12, 13, 16, 18, 55, 121), right = FALSE, labels = pf6_age_levels[1:7]))
  out[is.na(out)] <- "missing_or_invalid"
  out
}

pf6_quality <- function(x) {
  n <- suppressWarnings(as.numeric(as.character(x)))
  out <- ifelse(n == floor(n), "whole_years", "fractional_years")
  out[!is.na(n) & (n >= 121 | !is.finite(n))] <- "outside_public_range"
  out[!is.na(n) & n < 0] <- "negative"
  out[is.na(n)] <- "database_missing"
  out
}

pf6_public_codes <- function(dictionary, round, field) {
  q <- dictionary$response_options
  values <- unique(q$return_value[q$round_id == round & q$variable == field])
  # Only documented integer codes; never copy observed free text into the result.
  values <- values[!is.na(values) & grepl("^-?[0-9]+$", values)]
  # The diagnostic tests literal -77 even when an early occurrence's response
  # list omits it. This is a known missing-code probe, NOT a dictionary update
  # or approval to use -77 as that field's whole-survey participation rule.
  # Leave fields without any documented answer support unavailable.
  if (length(values)) values <- unique(c(values, "-77"))
  values[order(as.numeric(values))]
}

pf6_projection <- function(field, type, kind, codes = character()) {
  q <- pv6_quote(field)
  numeric_type <- grepl("NUMBER|FLOAT|INTEGER|DOUBLE|DECIMAL", type)
  if (kind %in% c("age", "age_quality") && !numeric_type) stop("Age field is not a verified numeric database type.")
  if (kind == "state") return(list(sql = pv6_state_sql(field, type), levels = pv6_states))
  if (kind == "presence") return(list(sql = paste0("CASE WHEN ", q, " IS NULL THEN 0 ELSE 1 END"),
    levels = c("database_missing", "source_nonmissing_not_proof_of_completion")))
  if (kind == "codes") {
    if (!length(codes) || any(!grepl("^-?[0-9]+$", codes)) || !grepl("NUMBER|FLOAT|INTEGER|DOUBLE|DECIMAL|CHAR", type)) stop("No safe public code domain.")
    literals <- if (numeric_type) codes else paste0("'", codes, "'")
    return(list(sql = paste0("CASE WHEN ", q, " IS NULL THEN 0", paste(paste0(" WHEN ", q, " = ", literals, " THEN ", seq_along(codes)), collapse = ""),
      " ELSE ", length(codes) + 1L, " END"), levels = c("database_missing", paste0("code_", codes), "other_code_not_released")))
  }
  if (kind == "mail") return(list(sql = pv6_context_sql(field, "mail"), levels = pf6_mail_levels))
  if (kind == "age_quality") return(list(sql = paste0("CASE WHEN ", q, " IS NULL THEN 0 WHEN ", q,
    " < 0 THEN 1 WHEN ", q, " >= 121 THEN 2 WHEN ", q, " = FLOOR(", q, ") THEN 3 ELSE 4 END"), levels = pf6_quality_levels))
  if (kind == "age") return(list(sql = paste0("CASE WHEN ", q, " IS NULL OR ", q, " < 0 OR ", q,
    " >= 121 THEN 7 WHEN ", q, " < 5 THEN 0 WHEN ", q, " < 12 THEN 1 WHEN ", q,
    " < 13 THEN 2 WHEN ", q, " < 16 THEN 3 WHEN ", q, " < 18 THEN 4 WHEN ", q, " < 55 THEN 5 ELSE 6 END"), levels = pf6_age_levels))
  stop("Unknown projection kind.")
}

pf6_file_projection <- function(x, kind, codes) {
  if (kind == "state") return(pv6_state(x))
  if (kind == "age") {
    if (!is.numeric(x) || inherits(x, c("Date", "POSIXt"))) stop("Age must be numeric.")
    return(pf6_age(x))
  }
  if (kind == "age_quality") return(pf6_quality(x))
  if (kind == "presence") return(ifelse(is.na(x), "database_missing", "source_nonmissing_not_proof_of_completion"))
  v <- as.character(x)
  if (kind == "mail") return(ifelse(is.na(v), "database_missing", ifelse(v %in% c("1", "2", "3"), paste0("group_", v), "other_code_not_released")))
  if (kind == "codes") return(ifelse(is.na(v), "database_missing", ifelse(v %in% codes, paste0("code_", v), "other_code_not_released")))
  stop("Unknown projection kind.")
}

pf6_fetch <- function(source, reg, keys, specs, raw = NULL) {
  query <- function(rows) {
    if (source$kind == "files") {
      fields <- unique(rows$field)
      if (!all(fields %in% names(raw))) stop("field_unavailable")
      aligned <- pv6_align(raw[c(reg$observation_key, fields)], reg$observation_key, keys)
      return(setNames(lapply(seq_len(nrow(rows)), function(j) pf6_file_projection(aligned[[rows$field[j]]], rows$kind[j], rows$codes[[j]])), rows$alias))
    }
    projections <- lapply(seq_len(nrow(rows)), function(j) pf6_projection(rows$field[j], rows$type[j], rows$kind[j], rows$codes[[j]]))
    expressions <- vapply(seq_along(projections), function(j) paste(projections[[j]]$sql, "AS", pv6_quote(rows$alias[j])), character(1))
    data <- pv6_align(pv6_query(source, reg$object_name, c(pv6_quote(reg$observation_key), expressions), reg$observation_key), reg$observation_key, keys)
    setNames(lapply(seq_along(projections), function(j) {
      code <- suppressWarnings(as.numeric(as.character(data[[rows$alias[j]]])))
      levels <- projections[[j]]$levels
      if (length(code) != length(keys) || anyNA(code) || any(code != floor(code) | code < 0 | code >= length(levels))) stop("invalid_projection")
      levels[code + 1L]
    }), rows$alias)
  }
  z <- tryCatch(query(specs), error = identity)
  if (!inherits(z, "error")) return(list(data = z, failures = character()))
  if (nrow(specs) == 1L) return(list(data = list(), failures = specs$alias))
  half <- seq_len(floor(nrow(specs) / 2L))
  a <- pf6_fetch(source, reg, keys, specs[half, , drop = FALSE], raw)
  b <- pf6_fetch(source, reg, keys, specs[-half, , drop = FALSE], raw)
  list(data = c(a$data, b$data), failures = c(a$failures, b$failures))
}

pf6_targeted <- function(source, rounds = "all", batch_size = 30L, progress = TRUE,
                         mail_fields = c("U_MAIL_GRP", "u_mail_grp", "U_MAILGRP", "MAIL_GRP", "MAIL_GROUP", "MAILGRP", "Mail_Grp")) {
  dictionary <- pf6_check_dictionary()
  if (!source$kind %in% c("oracle", "files")) stop("Use a real Oracle or file source, not synthetic data.")
  if (length(batch_size) != 1L || is.na(batch_size) || batch_size < 1L || batch_size != floor(batch_size)) stop("Invalid batch size.")
  pv6_quote(mail_fields)
  registry <- if (source$kind == "oracle") source$registry else dictionary$source_registry
  selected <- if (identical(rounds, "all")) registry$round_id else vapply(rounds, getFromNamespace(".resolve_round_name", "reactextract"), character(1), rounds = dictionary$rounds)
  selected <- unname(selected)
  if (!length(selected) || anyNA(selected) || anyDuplicated(selected) || !all(selected %in% registry$round_id)) stop("Unknown or repeated rounds.")
  counts <- inventory <- issues <- list()
  add_issue <- function(round, field, reason) issues[[length(issues) + 1L]] <<- data.frame(round_id = round, field = field, issue = reason)
  fields <- c("REGREPORTFIG", "SFREPORTFIG", "REGISTRATIONFILE", "SYMPTOMFILE", "ABSFREPORTFIG",
    "DATE_OF_LAST_ACCESS", "DATE_OF_LAST_ACCESSSF", "WHENCOMPLETION", "INDCONF", "INDCONFSF", "FEELUN",
    "SMOKENOW", "ABATTEMPT", "ABCOMP", "NEWRESULT", "NEWRESULT_2", "RESULT", "FINALRESULT", "U_AGE", "AGE")
  exact <- c("REGREPORTFIG", "SFREPORTFIG", "REGISTRATIONFILE", "SYMPTOMFILE", "SMOKENOW",
    "INDCONF", "INDCONFSF", "FEELUN", "ABATTEMPT", "ABCOMP", "NEWRESULT", "NEWRESULT_2")
  dates <- c("DATE_OF_LAST_ACCESS", "DATE_OF_LAST_ACCESSSF", "WHENCOMPLETION")
  started <- proc.time()[[3L]]
  for (ri in seq_along(selected)) {
    round <- selected[ri]
    reg <- registry[registry$round_id == round, , drop = FALSE]
    raw <- NULL
    base <- tryCatch({
      if (source$kind == "files") {
        entry <- source$rounds[[round]]
        if (is.null(entry)) stop("round_unavailable")
        raw <- if (is.data.frame(entry)) entry else getFromNamespace(".read_data_file", "reactextract")(entry)
        raw[reg$observation_key]
      } else pv6_query(source, reg$object_name, pv6_quote(reg$observation_key), reg$observation_key)
    }, error = identity)
    if (inherits(base, "error") || !reg$observation_key %in% names(base)) { add_issue(round, "", "round_or_key_unavailable"); next }
    keys <- as.character(base[[reg$observation_key]])
    if (anyNA(keys) || any(!nzchar(keys)) || anyDuplicated(keys)) { add_issue(round, "", "key_not_unique_or_missing_round_skipped"); next }
    if (progress) message("[reactextract] Focused check ", ri, "/", length(selected), " ", round, " | ", length(keys), " records")
    p <- dictionary$occurrences[dictionary$occurrences$round_id == round & dictionary$occurrences$variable %in% fields, , drop = FALSE]
    # Probe upstream names without returning contents. Presence is not verification of meaning.
    for (mail in unique(mail_fields)) {
      available <- if (source$kind == "files") mail %in% names(raw) else {
        probe <- tryCatch(pv6_query(source, reg$object_name, pv6_quote(mail), reg$observation_key, empty = TRUE), error = identity)
        !inherits(probe, "error") && mail %in% names(probe)
      }
      inventory[[length(inventory) + 1L]] <- data.frame(round_id = round, field = mail, role = "mail_group_probe",
        status = if (available) "available_meaning_unverified" else "not_available_or_probe_failed")
      if (available && !mail %in% p$variable) p <- rbind(p, transform(p[1L, , drop = FALSE], variable = mail, data_type = "VARCHAR2"))
    }
    specs <- list()
    for (j in seq_len(nrow(p))) {
      field <- p$variable[j]
      kind <- if (field %in% c("U_AGE", "AGE")) "age" else if (field %in% mail_fields) "mail" else if (field %in% dates) "presence" else if (field %in% exact) "codes" else "state"
      codes <- if (kind == "codes") pf6_public_codes(dictionary, round, field) else character()
      if (kind == "codes" && !length(codes)) { add_issue(round, field, "no_documented_public_code_domain"); next }
      kinds <- if (kind == "age") c("age", "age_quality") else kind
      for (k in kinds) {
        alias <- paste0("P", length(specs) + 1L)
        specs[[length(specs) + 1L]] <- data.frame(field = field, type = p$data_type[j], kind = k, alias = alias, codes = I(list(codes)))
      }
    }
    s <- do.call(rbind, specs)
    values <- list()
    batches <- split(seq_len(nrow(s)), ceiling(seq_len(nrow(s)) / batch_size))
    for (bi in seq_along(batches)) {
      b <- s[batches[[bi]], , drop = FALSE]
      fetched <- pf6_fetch(source, reg, keys, b, raw)
      values <- c(values, fetched$data)
      for (alias in fetched$failures) add_issue(round, s$field[match(alias, s$alias)], "field_projection_or_key_alignment_failed")
      if (progress) message("[reactextract] ", round, " | batch ", bi, "/", length(batches), " | ", nrow(b),
        " safe projections | ", length(keys), " records aligned | ", round(proc.time()[[3L]] - started), " seconds elapsed")
    }
    s <- s[s$alias %in% names(values), , drop = FALSE]
    for (j in seq_len(nrow(s))) inventory[[length(inventory) + 1L]] <- data.frame(round_id = round, field = s$field[j],
      role = paste0("projection_", s$kind[j]), status = "available_not_automatically_approved")
    levels_for <- function(j) pf6_projection(s$field[j], s$type[j], s$kind[j], s$codes[[j]])$levels
    add_table <- function(table, i, j = NULL) {
      counts[[length(counts) + 1L]] <<- pf6_counts(round, table, s$field[i], if (is.null(j)) "" else s$field[j],
        values[[s$alias[i]]], levels_for(i), if (is.null(j)) NULL else values[[s$alias[j]]],
        if (is.null(j)) NULL else levels_for(j), s$kind[i], if (is.null(j)) "none" else s$kind[j])
    }
    # Internal marginals are retained for enclave review but omitted from the export
    # whenever their domain is also present in a two-way table.
    for (j in seq_len(nrow(s))) add_table("source_marginal", j)
    stage <- which(s$field %in% c("REGREPORTFIG", "SFREPORTFIG", "REGISTRATIONFILE", "SYMPTOMFILE", dates))
    answers <- which(s$field %in% c("INDCONF", "INDCONFSF", "FEELUN", "SMOKENOW", "ABATTEMPT", "ABCOMP", "NEWRESULT", "NEWRESULT_2", "RESULT", "FINALRESULT"))
    for (i in answers) for (j in stage) add_table("field_by_stage_marker", i, j)
    # Compact discordance partitions can be useful even when the full code-by-code
    # matrix is too sparse to release. Its matching remainder is always withheld.
    # The corresponding full matrix remains enclave-only to avoid overlapping sums.
    for (i in answers[s$kind[answers] == "codes"]) for (j in stage[s$field[stage] %in% c("SFREPORTFIG", "REGREPORTFIG")]) {
      x <- values[[s$alias[i]]] == "code_-77"
      y <- values[[s$alias[j]]] == "code_-77"
      state <- ifelse(x & !y, "field_only_code77", ifelse(!x & y, "reference_only_code77", "matching_remainder_withheld"))
      counts[[length(counts) + 1L]] <- pf6_counts(round, "code77_discordance", s$field[i], s$field[j], state,
        c("field_only_code77", "reference_only_code77", "matching_remainder_withheld"),
        kind = "code77_discordance", reference_kind = "comparison_anchor")
    }
    if (length(stage) > 1L) for (pair in combn(stage, 2L, simplify = FALSE)) add_table("stage_marker_agreement", pair[1L], pair[2L])
    age <- which(s$kind == "age")
    if (length(age) == 2L) add_table("age_agreement", age[1L], age[2L])
    for (i in which(s$field == "SMOKENOW")) for (j in age) add_table("smoking_by_age", i, j)
    for (i in which(s$kind == "mail")) {
      for (j in age) add_table("mail_by_age", i, j)
      for (j in which(s$field == "SMOKENOW")) add_table("smoking_by_mail", j, i)
    }
    rm(raw, base, keys, values, fetched)
  }
  bind <- function(x, names) if (length(x)) do.call(rbind, x) else setNames(as.data.frame(replicate(length(names), character(), simplify = FALSE)), names)
  list(counts = bind(counts, c("round_id", "table", "field", "reference", "state", "reference_state", "count", "field_kind", "reference_kind")),
    inventory = bind(inventory, c("round_id", "field", "role", "status")), issues = bind(issues, c("round_id", "field", "issue")),
    manifest = data.frame(key = c("schema", "status", "package_version", "dictionary_manifest_sha256", "requested_rounds", "elapsed_seconds", "age_policy", "completion_policy", "missing_code_policy"),
      value = c("participation-targeted-v3", pf6_internal, as.character(utils::packageVersion("reactextract")), pf6_dictionary_hash,
        paste(selected, collapse = "|"), as.character(proc.time()[[3L]] - started),
        "fractional_years_binned_without_rounding_no_mail_substitution", "public_codes_plus_known_missing_probe_no_automatic_stage_assignment",
        "literal_minus77_checked_even_if_occurrence_response_options_omit_it")))
}

pf6_prepare_export <- function(result) {
  if (!identical(pf6_meta(result, "status"), pf6_internal) || "suppressed" %in% names(result$counts)) stop("Prepare only unsuppressed internal results.")
  if (!identical(pf6_meta(result, "dictionary_manifest_sha256"), pf6_dictionary_hash)) stop("Saved result has the wrong dictionary.")
  schema <- pf6_meta(result, "schema")
  if (!schema %in% c("participation-reexport-v2", "participation-targeted-v2", "participation-targeted-v3")) stop("Unsupported follow-up schema.")
  pf6_validate_counts(result$counts)
  d <- result$counts
  # Never export round denominators or overlapping standalone marginals. The
  # enquiry's matrices carry the useful comparisons without redundant totals.
  joint <- d[d$reference_kind != "none" & d$reference_state != "all", , drop = FALSE]
  linked_domains <- unique(c(paste(joint$round_id, joint$field, joint$field_kind), paste(joint$round_id, joint$reference, joint$reference_kind)))
  omit <- d$table == "source_marginal" & paste(d$round_id, d$field, d$field_kind) %in% linked_domains
  compact <- d[d$table == "code77_discordance", , drop = FALSE]
  compact_pairs <- paste(compact$round_id, compact$field, compact$reference)
  omit <- omit | (d$table == "field_by_stage_marker" & paste(d$round_id, d$field, d$reference) %in% compact_pairs)
  # Age quality is enclave-only: its missing category overlaps age-band margins.
  omit <- omit | d$field_kind == "age_quality"
  d <- d[!omit, , drop = FALSE]
  small <- d$count < 10
  forced <- d$state == "matching_remainder_withheld"
  hidden <- small | forced
  partition <- paste(d$round_id, d$table, d$field, d$field_kind, d$reference, d$reference_kind, sep = "\r")
  groups <- split(seq_len(nrow(d)), partition)
  two <- which(d$reference_state != "all")
  groups <- c(groups, split(two, paste(partition[two], d$state[two], sep = "\r")),
    split(two, paste(partition[two], d$reference_state[two], sep = "\r")))
  # For linked matrices, equal cells mean the same field/condition pair, not just
  # coincidentally equal numbers. This handles mirrored tables without the v1
  # whole-round equal-number cascade.
  a <- paste(d$round_id, d$field, d$field_kind, d$state, sep = "\r")
  b <- paste(d$round_id, d$reference, d$reference_kind, d$reference_state, sep = "\r")
  cell <- ifelse(a < b, paste(a, b, sep = "\t"), paste(b, a, sep = "\t"))
  copies <- split(seq_len(nrow(d)), cell)
  copies <- copies[lengths(copies) > 1L]
  repeat {
    before <- hidden
    for (idx in copies) {
      if (length(unique(d$count[idx])) != 1L) stop("Conflicting copies of a linked aggregate.")
      if (any(hidden[idx])) hidden[idx] <- TRUE
    }
    for (idx in groups) {
      # A hidden small cell needs a positive, non-small hidden companion. This
      # also prevents suppressed zeros alone serving as the complement.
      if (any(small[idx]) && !any(hidden[idx] & !small[idx])) {
        candidates <- idx[!hidden[idx] & !small[idx]]
        if (length(candidates)) hidden[candidates[which.max(d$count[candidates])]] <- TRUE
      }
      # With known/shared margins, one remaining positive hidden cell would be
      # recoverable. Add a second positive complement where one is available.
      if (sum(hidden[idx] & d$count[idx] > 0) == 1L && !any(forced[idx])) {
        candidates <- idx[!hidden[idx] & d$count[idx] > 0]
        if (length(candidates)) hidden[candidates[which.max(d$count[candidates])]] <- TRUE
      }
    }
    if (identical(before, hidden)) break
  }
  d$protection <- ifelse(small, "below_10", ifelse(forced, "withheld_remainder", ifelse(hidden, "linked_complement", "released_rounded_to_5")))
  d$count <- round(d$count / 5) * 5
  d$count[hidden] <- NA_real_
  d$suppressed <- hidden
  out <- result
  out$counts <- d
  out$manifest$value[out$manifest$key == "status"] <- pf6_pending
  out$manifest <- rbind(out$manifest, data.frame(key = c("minimum_cell", "round_counts_to", "suppression_policy", "cross_release_review"),
    value = c("10", "5", "semantic_cells_and_partition_row_column_complements_no_round_totals", "required_against_v1_and_all_previously_released_profiles")))
  out
}

pf6_write <- function(result, path) {
  if (file.exists(path) || dir.exists(path)) stop("Output already exists. Choose a new output folder; do not overwrite the previous export.")
  if (!identical(pf6_meta(result, "status"), pf6_pending)) stop("Prepare the protected export first. Internal results must stay inside the enclave.")
  d <- result$counts
  expected <- c("round_id", "table", "field", "reference", "state", "reference_state", "count", "field_kind", "reference_kind", "protection", "suppressed")
  if (!identical(names(d), expected) || !is.logical(d$suppressed) || anyNA(d$suppressed) ||
      any(is.na(d$count) != d$suppressed) || any(!is.na(d$count) & (!is.finite(d$count) | d$count < 10 | d$count %% 5 != 0))) stop("Invalid protected export.")
  allowed <- c("participation-reexport-v2", "participation-targeted-v2", "participation-targeted-v3")
  if (!pf6_meta(result, "schema") %in% allowed) stop("Unsupported follow-up schema.")
  dir.create(path, recursive = TRUE)
  for (name in c("counts", "inventory", "issues", "manifest")) utils::write.csv(result[[name]], file.path(path, paste0(name, ".csv")), row.names = FALSE, na = "")
  writeLines(c("# Participation follow-up: awaiting enclave disclosure approval", "",
    "Candidate aggregate export only. Automated checks are not approval. Do not copy outside the enclave before normal disclosure review.",
    "Counts below ten are hidden; complementary cells are hidden and visible counts rounded to five. No round totals are exported.",
    "Review against the original diagnostic export and all previously released profiles: cross-release differencing is not certified by this script.",
    "Suppressed does not mean zero. A below_10 direction can contain any count from zero to nine; it does not prove exact agreement.",
    "The reused comparison anchor is not an approved completion indicator. not77 includes other missingness as well as answers.",
    "Dates are presence only, never exact dates. A recorded date or nonmissing laboratory value is not automatically proof of completion or a valid result.",
    "No questionnaire-stage or mail-group assignments have been approved by this diagnostic. Age-quality results remain enclave-only."), file.path(path, "README.md"))
  files <- list.files(path, full.names = TRUE)
  utils::write.csv(data.frame(file = basename(files), sha256 = vapply(files, getFromNamespace(".sha256_file", "reactextract"), character(1))),
    file.path(path, "checksums.csv"), row.names = FALSE)
  message("Created ", path, ". Keep it inside the enclave pending disclosure review.")
  invisible(path)
}
