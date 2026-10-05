# Read-only diagnostic for the proposed v6 participation contract.
# This is deliberately separate from the approved v5 generator.
pv6_states <- c("database_missing", "survey_code_77", "not_applicable_91",
  "item_nonresponse_92", "administrative_66", "other_missing_99",
  "source_missing_555", "unrecognised_negative", "substantive")
pv6_age_levels <- c("under_5", "5_11", "12", "13_15", "16_17", "18_54", "55_plus", "missing_or_invalid")
pv6_marker_levels <- c("database_missing", "code_0", "code_1", "code_2", "code_3", "code_minus77", "other_code_no_content")

pv6_marker <- function(x) {
  v <- as.character(x)
  found <- match(v, c("0", "1", "2", "3", "-77"))
  out <- rep("other_code_no_content", length(v))
  out[!is.na(found)] <- pv6_marker_levels[found[!is.na(found)] + 1L]
  out[is.na(v)] <- "database_missing"
  out
}

pv6_state <- function(x) {
  out <- rep.int("substantive", length(x))
  out[is.na(x)] <- "database_missing"
  if (inherits(x, c("Date", "POSIXt"))) return(out)
  text <- as.character(x)
  codes <- c("-77", "-91", "-92", "-66", "-99", "-555")
  negative <- if (is.numeric(x)) !is.na(x) & x < 0 else !is.na(text) & grepl("^-[0-9]+([.][0-9]+)?$", text)
  out[negative] <- "unrecognised_negative"
  for (j in seq_along(codes)) out[!is.na(text) & text == codes[j]] <- pv6_states[j + 1L]
  out
}

pv6_age <- function(x) {
  age <- suppressWarnings(as.numeric(as.character(x)))
  out <- as.character(cut(age, breaks = c(0, 5, 12, 13, 16, 18, 55, 121),
    right = FALSE, labels = pv6_age_levels[1:7]))
  out[is.na(out) | age != floor(age)] <- "missing_or_invalid"
  out
}

pv6_quote <- function(x) {
  if (anyNA(x) || any(!grepl("^[A-Za-z][A-Za-z0-9_]*$", x))) stop("Unsafe field name.")
  paste0('"', x, '"')
}

pv6_state_sql <- function(field, type) {
  q <- pv6_quote(field)
  if (grepl("DATE|TIMESTAMP", type)) {
    return(paste0("CASE WHEN ", q, " IS NULL THEN 0 ELSE 8 END"))
  }
  if (!grepl("NUMBER|FLOAT|INTEGER|DOUBLE|DECIMAL|CHAR", type)) stop("Unsupported state projection type.")
  codes <- c(-77, -91, -92, -66, -99, -555)
  literals <- if (grepl("CHAR", type)) paste0("'", codes, "'") else as.character(codes)
  parts <- paste0(" WHEN ", q, " = ", literals, " THEN ", seq_along(codes))
  # Do not cast arbitrary character contents to numbers or return their text.
  negative <- if (grepl("CHAR", type)) paste0(" WHEN REGEXP_LIKE(", q,
    ", '^-[0-9]+([.][0-9]+)?$') THEN 7") else paste0(" WHEN ", q, " < 0 THEN 7")
  paste0("CASE WHEN ", q, " IS NULL THEN 0", paste(parts, collapse = ""), negative, " ELSE 8 END")
}

pv6_context_sql <- function(field, kind) {
  q <- pv6_quote(field)
  if (kind == "marker") return(paste0("CASE WHEN ", q, " IS NULL THEN 0",
    paste(paste0(" WHEN ", q, " = '", c("0", "1", "2", "3", "-77"), "' THEN ", 1:5), collapse = ""),
    " ELSE 6 END"))
  if (kind == "mail") return(paste0("CASE WHEN ", q, " = '1' THEN 1 WHEN ", q,
    " = '2' THEN 2 WHEN ", q, " = '3' THEN 3 WHEN ", q, " IS NULL THEN 0 ELSE 4 END"))
  paste0("CASE WHEN ", q, " IS NULL OR ", q, " < 0 OR ", q, " >= 121 OR ", q,
    " <> FLOOR(", q, ") THEN 8 WHEN ", q, " < 5 THEN 1 WHEN ", q,
    " < 12 THEN 2 WHEN ", q, " < 13 THEN 3 WHEN ", q,
    " < 16 THEN 4 WHEN ", q, " < 18 THEN 5 WHEN ", q,
    " < 55 THEN 6 ELSE 7 END")
}

pv6_align <- function(data, key, base_keys) {
  if (!is.data.frame(data) || !key %in% names(data)) stop("key_missing")
  keys <- as.character(data[[key]])
  if (anyNA(keys) || any(!nzchar(keys)) || anyDuplicated(keys) ||
      length(keys) != length(base_keys) || !setequal(keys, base_keys)) stop("key_mismatch")
  data[match(base_keys, keys), , drop = FALSE]
}

pv6_query <- function(source, object, select, key, empty = FALSE) {
  if (!grepl("^[A-Z][A-Z0-9_.]*$", object)) stop("Unsafe source object.")
  sql <- paste0("SELECT ", paste(select, collapse = ", "), " FROM ", object,
    if (empty) " WHERE 1 = 0" else paste0(" ORDER BY ", pv6_quote(key)))
  source$query_fn(source$connection, sql)
}

pv6_fetch <- function(source, registry, base_keys, fields, types, raw = NULL,
                      kind = "state") {
  key <- registry$observation_key[[1L]]
  fetch_one_batch <- function(fields, types) {
    if (source$kind == "files") {
      if (!all(fields %in% names(raw))) stop("field_unavailable")
      data <- pv6_align(raw[c(key, fields)], key, base_keys)
      return(lapply(data[fields], function(x) {
        if (kind == "state") pv6_state(x) else if (kind == "age") pv6_age(x) else if (kind == "marker") pv6_marker(x) else {
          v <- as.character(x)
          ifelse(is.na(v), "missing", ifelse(v %in% c("1", "2", "3"), v, "other"))
        }
      }))
    }
    expressions <- vapply(seq_along(fields), function(j) {
      expr <- if (kind == "state") pv6_state_sql(fields[j], types[j]) else pv6_context_sql(fields[j], kind)
      paste(expr, "AS", pv6_quote(fields[j]))
    }, character(1))
    data <- pv6_align(pv6_query(source, registry$object_name[[1]],
      c(pv6_quote(key), expressions), key), key, base_keys)
    lapply(data[fields], function(x) {
      codes <- suppressWarnings(as.integer(as.character(x)))
      domain <- if (kind == "state") pv6_states else if (kind == "age") pv6_age_levels else if (kind == "marker") pv6_marker_levels else c("missing", "1", "2", "3", "other")
      index <- if (kind == "age") codes else codes + 1L
      if (anyNA(index) || any(index < 1L | index > length(domain))) stop("invalid_state_projection")
      domain[index]
    })
  }
  data <- tryCatch(fetch_one_batch(fields, types), error = identity)
  if (!inherits(data, "error")) return(list(data = data, failures = character()))
  if (length(fields) == 1L) return(list(data = list(), failures = fields))
  midpoint <- floor(length(fields) / 2L)
  a <- pv6_fetch(source, registry, base_keys, fields[seq_len(midpoint)], types[seq_len(midpoint)], raw, kind)
  b <- pv6_fetch(source, registry, base_keys, fields[-seq_len(midpoint)], types[-seq_len(midpoint)], raw, kind)
  list(data = c(a$data, b$data), failures = c(a$failures, b$failures))
}

pv6_counts <- function(round_id, table, field, reference, x, xlevels, y = NULL, ylevels = NULL) {
  if (is.null(y)) {
    counts <- as.data.frame(table(factor(x, levels = xlevels)), stringsAsFactors = FALSE)
    names(counts) <- c("state", "count")
    counts$reference_state <- "all"
  } else {
    counts <- as.data.frame(table(factor(x, levels = xlevels), factor(y, levels = ylevels)), stringsAsFactors = FALSE)
    names(counts) <- c("state", "reference_state", "count")
  }
  data.frame(round_id = round_id, table = table, field = field, reference = reference,
    state = as.character(counts$state), reference_state = as.character(counts$reference_state),
    count = as.numeric(counts$count), stringsAsFactors = FALSE)
}

run_participation_diagnostic <- function(source, contract, rounds = "all", batch_size = 40L, progress = TRUE) {
  if (!source$kind %in% c("oracle", "files")) stop("Use an Oracle or file source, not synthetic data.")
  if (length(batch_size) != 1L || is.na(batch_size) || batch_size < 1L || batch_size != as.integer(batch_size)) stop("Invalid batch size.")
  expected <- "28d03054e4b284cd44a040cf473991c441184739e84a7f6235392b1142a79236"
  version <- reactextract::react_dictionary_version()
  if (!identical(version$manifest_sha256, expected)) stop("This diagnostic requires the pinned rc14 dictionary. Do not mix review contracts.")
  dictionary <- reactextract::react_dictionary()
  provenance_path <- file.path(contract, "occurrence_provenance.csv")
  contract_hash <- getFromNamespace(".sha256_file", "reactextract")(provenance_path)
  checksums <- utils::read.csv(file.path(contract, "contract-checksums.csv"), colClasses = "character")
  expected_contract <- checksums$sha256[checksums$file == "occurrence_provenance.csv"]
  if (!identical(contract_hash, expected_contract)) stop("Provenance inventory checksum mismatch.")
  provenance <- utils::read.csv(provenance_path, colClasses = "character", na.strings = NULL)
  if (anyDuplicated(provenance$occurrence_id) || !setequal(provenance$occurrence_id, dictionary$occurrences$occurrence_id)) stop("Provenance inventory does not match the pinned dictionary.")
  registry <- if (source$kind == "oracle") source$registry else dictionary$source_registry
  selected <- if (identical(rounds, "all")) registry$round_id else vapply(rounds,
    getFromNamespace(".resolve_round_name", "reactextract"), character(1), rounds = dictionary$rounds)
  if (anyNA(selected) || anyDuplicated(selected)) stop("Unknown or repeated rounds.")
  counts <- inventory <- issues <- list()
  add_issue <- function(round, field, reason) {
    issues[[length(issues) + 1L]] <<- data.frame(round_id = round, field = field, issue = reason)
  }
  started <- proc.time()[[3L]]
  for (ri in seq_along(selected)) {
    round <- selected[[ri]]
    reg <- registry[registry$round_id == round, , drop = FALSE]
    key <- reg$observation_key[[1L]]
    raw <- NULL
    base <- tryCatch({
      if (source$kind == "files") {
        entry <- source$rounds[[round]]
        if (is.null(entry)) stop("round_unavailable")
        raw <- if (is.data.frame(entry)) entry else getFromNamespace(".read_data_file", "reactextract")(entry)
        raw[key]
      } else pv6_query(source, reg$object_name[[1L]], pv6_quote(key), key)
    }, error = identity)
    if (inherits(base, "error") || !key %in% names(base)) { add_issue(round, "", "round_or_key_unavailable"); next }
    keys <- as.character(base[[key]])
    if (anyNA(keys) || any(!nzchar(keys)) || anyDuplicated(keys)) { add_issue(round, "", "key_not_unique_or_missing_round_skipped"); next }
    n <- length(keys)
    p <- provenance[provenance$round_id == round, , drop = FALSE]
    kinds <- dictionary$synthetic_profile_specs$profile_kind[match(p$occurrence_id, dictionary$synthetic_profile_specs$occurrence_id)]
    # Only state projections of ordinary fields. No real identifier, photo or text content is selected.
    keep <- !is.na(kinds) & !kinds %in% c("identifier", "excluded") &
      grepl("NUMBER|FLOAT|INTEGER|DOUBLE|DECIMAL|CHAR|DATE|TIMESTAMP", p$data_type) & p$variable != key
    omitted <- p[!keep, , drop = FALSE]
    if (nrow(omitted)) inventory[[length(inventory) + 1L]] <- data.frame(round_id = round, field = omitted$variable,
      role = "dictionary_field", status = "not_profiled_identifier_or_unsupported", proposed_scope = omitted$participation_scope_proposed)
    p <- p[keep, , drop = FALSE]
    if (progress) message("[reactextract] Participation diagnostic ", ri, "/", length(selected), " ", round,
      " | ", n, " records | ", nrow(p), " state-only fields")
    # Probe named candidates without retrieving their contents. Presence does not imply an authoritative completion marker.
    mail_fields <- c("U_MAIL_GRP", "u_mail_grp", "U_MAILGRP", "MAIL_GRP", "MAIL_GROUP", "MAILGRP", "Mail_Grp")
    candidates <- c(mail_fields, "U_AGE", "AGE",
      "REG_COMPLETE", "REGISTRATION_COMPLETE", "SURVEY_COMPLETE", "QUESTIONNAIRE_COMPLETE", "IND_COMPLETE", "INDIVIDUAL_COMPLETE", "QCOMPLETE")
    available <- vapply(candidates, function(field) {
      if (source$kind == "files") return(field %in% names(raw))
      z <- tryCatch(pv6_query(source, reg$object_name[[1L]], pv6_quote(field), key, empty = TRUE), error = identity)
      !inherits(z, "error") && field %in% names(z)
    }, logical(1))
    inventory[[length(inventory) + 1L]] <- data.frame(round_id = round, field = candidates,
      role = "upstream_candidate", status = ifelse(available, "available_mapping_not_verified", "not_available_or_probe_failed"), proposed_scope = "needs_evidence")
    age <- rep("missing_or_invalid", n)
    for (agefield in intersect(c("U_AGE", "AGE"), candidates[available])) {
      a <- pv6_fetch(source, reg, keys, agefield, "NUMBER", raw, "age")
      if (length(a$data)) {
        if (agefield == "U_AGE") age <- a$data[[1L]]
        counts[[length(counts) + 1L]] <- pv6_counts(round, "age_context", agefield, "", a$data[[1L]], pv6_age_levels)
        if (agefield == "AGE" && "U_AGE" %in% candidates[available]) counts[[length(counts) + 1L]] <-
          pv6_counts(round, "age_agreement", "AGE", "U_AGE", a$data[[1L]], pv6_age_levels, age, pv6_age_levels)
      } else add_issue(round, agefield, "age_projection_failed")
    }
    for (mail in intersect(mail_fields, candidates[available])) {
      m <- pv6_fetch(source, reg, keys, mail, "NUMBER", raw, "mail")
      if (length(m$data)) counts[[length(counts) + 1L]] <- pv6_counts(round, "mail_age_agreement", mail, "U_AGE",
        m$data[[1L]], c("missing", "1", "2", "3", "other"), age, pv6_age_levels)
      else add_issue(round, mail, "mail_projection_failed")
    }
    marker_fields <- unique(c(intersect(c("INDCONF", "INDCONFSF", "SMOKENOW", "FEELUN", "ABATTEMPT", "ABCOMP"), p$variable),
      candidates[available & grepl("COMPLETE", candidates)]))
    marker_fetch <- pv6_fetch(source, reg, keys, marker_fields, rep("VARCHAR2", length(marker_fields)), raw, "marker")
    markers <- marker_fetch$data
    for (field in marker_fetch$failures) add_issue(round, field, "candidate_marker_projection_failed")
    stage77 <- stage_observed <- list(registration = integer(n), individual = integer(n))
    stage_fields <- c(registration = 0L, individual = 0L)
    stage_total_fields <- stage_fields
    batches <- split(seq_len(nrow(p)), ceiling(seq_len(nrow(p)) / batch_size))
    for (bi in seq_along(batches)) {
      rows <- batches[[bi]]
      fetched <- pv6_fetch(source, reg, keys, p$variable[rows], p$data_type[rows], raw)
      for (field in fetched$failures) add_issue(round, field, "field_unavailable_or_key_alignment_failed")
      for (field in names(fetched$data)) {
        state <- fetched$data[[field]]
        scope <- p$participation_scope_proposed[match(field, p$variable)]
        counts[[length(counts) + 1L]] <- pv6_counts(round, "field_states", field, "", state, pv6_states)
        # Pairwise missing-code agreement only: never export a respondent-level pattern or its hash.
        for (marker in setdiff(names(markers), field)) counts[[length(counts) + 1L]] <-
          pv6_counts(round, "code77_agreement", field, marker, ifelse(state == "survey_code_77", "77", "not77"),
            c("77", "not77"), markers[[marker]], pv6_marker_levels)
        # All field-state counts by public age boundaries; no exact ages or response contents.
        counts[[length(counts) + 1L]] <- pv6_counts(round, "field_states_by_age", field, "U_AGE",
          state, pv6_states, age, pv6_age_levels)
        if (scope %in% names(stage_fields)) {
          stage_total_fields[scope] <- stage_total_fields[scope] + 1L
          # Dates cannot literally contain -77. They must not make an otherwise
          # shared -77 pattern impossible, but recorded dates still flag partial answers.
          capable <- !grepl("DATE|TIMESTAMP", p$data_type[match(field, p$variable)])
          stage_fields[scope] <- stage_fields[scope] + as.integer(capable)
          stage77[[scope]] <- stage77[[scope]] + as.integer(state == "survey_code_77")
          stage_observed[[scope]] <- stage_observed[[scope]] + as.integer(state == "substantive")
        }
      }
      if (progress) message("[reactextract] ", round, " batch ", bi, "/", length(batches),
        " complete | ", round(proc.time()[[3L]] - started), " seconds elapsed")
    }
    for (stage in names(stage_fields)) {
      state <- if (!stage_fields[stage]) rep("no_fields_available", n) else
        ifelse(stage77[[stage]] > 0L & stage_observed[[stage]] > 0L, "77_and_substantive",
          ifelse(stage77[[stage]] == stage_fields[stage], "all_fields_77",
            ifelse(stage77[[stage]] > 0L, "77_and_other_missing", "no_77")))
      counts[[length(counts) + 1L]] <- pv6_counts(round, "provisional_stage_pattern", stage, "",
        state, c("all_fields_77", "77_and_substantive", "77_and_other_missing", "no_77", "no_fields_available"))
      inventory[[length(inventory) + 1L]] <- data.frame(round_id = round, field = stage, role = "provisional_stage",
        status = paste0(stage_fields[stage], "_code77_capable_fields_of_", stage_total_fields[stage], "_available"), proposed_scope = "candidate_not_completion_status")
    }
    counts[[length(counts) + 1L]] <- pv6_counts(round, "round_denominator", "", "", rep("records", n), "records")
    rm(raw, base, keys, fetched, markers, stage77, stage_observed)
  }
  bind <- function(x) if (length(x)) do.call(rbind, x) else data.frame()
  list(counts = bind(counts), inventory = bind(inventory), issues = bind(issues),
    manifest = data.frame(key = c("schema", "package_version", "dictionary_manifest_sha256", "provenance_sha256", "requested_rounds", "status", "age_reference", "elapsed_seconds"),
      value = c("participation-diagnostic-v1", as.character(utils::packageVersion("reactextract")), expected,
        contract_hash, paste(selected, collapse = "|"),
        "enclave_internal_not_disclosure_approved", "U_AGE_only_no_unverified_age_or_mail_substitution", as.character(proc.time()[[3L]] - started))))
}

prepare_participation_diagnostic_export <- function(result) {
  # Conservative linked suppression: a hidden count remains hidden everywhere
  # that same count appears in a round, including copied denominators/margins.
  # A partition may never have exactly one hidden cell. Whole-round totals are
  # withheld whenever any constituent partition is suppressed. Rounding is last.
  out <- result
  d <- out$counts
  if (!nrow(d)) stop("No successful round: inspect issues inside the enclave.")
  hidden <- d$count < 10
  partition <- interaction(d[c("round_id", "table", "field", "reference")], drop = TRUE, lex.order = TRUE)
  groups <- split(seq_len(nrow(d)), partition)
  # Also protect each row/column of the two-way agreement and age tables.
  groups <- c(groups,
    split(seq_len(nrow(d)), interaction(partition, d$state, drop = TRUE)),
    split(seq_len(nrow(d)), interaction(partition, d$reference_state, drop = TRUE)))
  by_round <- split(seq_len(nrow(d)), d$round_id)
  repeat {
    before <- hidden
    for (indices in by_round) {
      hidden[indices] <- hidden[indices] | d$count[indices] %in% d$count[indices[hidden[indices]]]
      if (any(hidden[indices])) hidden[indices[d$table[indices] == "round_denominator"]] <- TRUE
    }
    for (indices in groups) if (sum(hidden[indices] & d$count[indices] > 0) == 1L && length(indices) > 1L) {
      # Suppressed zero cells are not a substitute for a positive complement.
      eligible <- indices[!hidden[indices] & d$count[indices] > 0]
      if (length(eligible)) hidden[eligible[which.max(d$count[eligible])]] <- TRUE
    }
    if (identical(before, hidden)) break
  }
  d$count <- round(d$count / 5) * 5
  d$count[hidden] <- NA_real_
  d$suppressed <- hidden
  out$counts <- d
  out$manifest$value[out$manifest$key == "status"] <- "awaiting_normal_enclave_disclosure_approval"
  out$manifest <- rbind(out$manifest, data.frame(key = c("minimum_cell", "round_counts_to", "linked_suppression"),
    value = c("10", "5", "shared-count-propagation_and_partition_complements")))
  out
}

write_participation_diagnostic <- function(result, path) {
  if (dir.exists(path) || file.exists(path)) stop("Output already exists; choose a new directory.")
  if (!identical(result$manifest$value[result$manifest$key == "status"], "awaiting_normal_enclave_disclosure_approval")) stop("Prepare disclosure controls before writing an export.")
  required <- c("round_id", "table", "field", "reference", "state", "reference_state", "count", "suppressed")
  if (!identical(names(result$counts), required)) stop("Unexpected columns in diagnostic export.")
  if (any(!is.na(result$counts$count) & (result$counts$count < 10 | result$counts$count %% 5 != 0))) stop("Invalid released counts.")
  dir.create(path, recursive = TRUE)
  for (name in c("counts", "inventory", "issues", "manifest")) utils::write.csv(result[[name]], file.path(path, paste0(name, ".csv")), row.names = FALSE, na = "")
  writeLines(c("# Participation diagnostic — awaiting disclosure approval", "",
    "Only aggregate states are included. No raw answers, respondent identifiers, exact ages or row-level missingness patterns are written.",
    "Source/stage assignments are provisional. All-fields-77 is a pattern, not an inferred completion status.",
    "Counts below ten and linked complements are hidden; released counts are rounded to five.",
    "Normal enclave disclosure review is still required before copying this folder outside the enclave."), file.path(path, "README.md"))
  files <- list.files(path, full.names = TRUE)
  utils::write.csv(data.frame(file = basename(files), sha256 = vapply(files,
    getFromNamespace(".sha256_file", "reactextract"), character(1))), file.path(path, "checksums.csv"), row.names = FALSE)
  invisible(path)
}
