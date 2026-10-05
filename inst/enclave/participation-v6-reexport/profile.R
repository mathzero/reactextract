# Enclave-only conditional capture, separate from the released v5 generator.
pc6_internal <- "enclave_internal_not_disclosure_approved"
pc6_pending <- "awaiting_normal_enclave_disclosure_approval"
pc6_age_levels <- c("under_5", "5_11", "12", "13_15", "16_17", "18_54", "55_plus", "missing_or_invalid")
pc6_stage_levels <- c("shared_nonresponse", "response_evidenced", "undetermined")
pc6_detail_levels <- c("recorded_complete", "recorded_breakoff", "shared_nonresponse",
  "discordant_flag_and_answer", "response_evidenced", "undetermined", "source_555")
pc6_ns <- function(name) getFromNamespace(name, "reactextract")
pc6_hash <- function(path) pc6_ns(".sha256_file")(path)
pc6_read <- function(path) utils::read.csv(path, colClasses = "character", na.strings = character(), check.names = FALSE)
pc6_quote <- function(x) {
  if (anyNA(x) || any(!grepl("^[A-Za-z][A-Za-z0-9_]*$", x))) stop("Unsafe field name.")
  paste0('"', x, '"')
}
pc6_literal <- function(x) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
pc6_align <- function(data, key, keys) {
  if (!is.data.frame(data) || !key %in% names(data)) stop("Missing observation key.")
  found <- as.character(data[[key]])
  if (anyNA(found) || any(!nzchar(found)) || anyDuplicated(found) ||
      length(found) != length(keys) || !setequal(found, keys)) stop("Observation keys changed; this round must be rerun.")
  data[match(keys, found), , drop = FALSE]
}
pc6_query <- function(source, reg, expressions) {
  if (!grepl("^[A-Z][A-Z0-9_.]*$", reg$object_name)) stop("Unsafe source object.")
  source$query_fn(source$connection, paste0("SELECT ", paste(expressions, collapse = ", "),
    " FROM ", reg$object_name, " ORDER BY ", pc6_quote(reg$observation_key)))
}
pc6_contract <- function(path) {
  hashes <- pc6_read(file.path(path, "checksums.csv"))
  required <- c("contract.csv", "occurrence_participation.csv", "age_context.csv", "routing_capture.csv",
    "routing_conditions.csv", "routing_targets.csv", "pending_context_instructions.csv", "PARTICIPATION_V6_DECISIONS.md")
  if (!setequal(hashes$file, required) || anyDuplicated(hashes$file)) stop("Incomplete capture contract.")
  for (i in seq_len(nrow(hashes))) if (!identical(pc6_hash(file.path(path, hashes$file[i])), hashes$sha256[i])) stop("Capture contract checksum mismatch.")
  meta <- pc6_read(file.path(path, "contract.csv"))
  expected <- meta$value[meta$key == "base_dictionary_sha256"]
  if (!identical(reactextract::react_dictionary_version()$manifest_sha256, expected)) stop("This kit requires the pinned rc14 dictionary from reactextract 0.5.4 or later 0.5.x.")
  result <- lapply(sub("[.]csv$", "", required[grepl("[.]csv$", required)]), function(name) pc6_read(file.path(path, paste0(name, ".csv"))))
  names(result) <- sub("[.]csv$", "", required[grepl("[.]csv$", required)])
  result$hash <- pc6_hash(file.path(path, "checksums.csv"))
  d <- reactextract::react_dictionary()
  if (anyDuplicated(result$occurrence_participation$occurrence_id) ||
      !setequal(result$occurrence_participation$occurrence_id, d$occurrences$occurrence_id)) stop("Contract occurrence mismatch.")
  result
}
pc6_age <- function(x) {
  value <- suppressWarnings(as.numeric(as.character(x)))
  out <- as.character(cut(value, c(0, 5, 12, 13, 16, 18, 55, 121), right = FALSE, labels = pc6_age_levels[1:7]))
  out[is.na(out)] <- "missing_or_invalid"
  out
}
pc6_condition <- function(value, operator, comparison, substantive = NULL) {
  if (is.null(substantive)) {
    num <- suppressWarnings(as.numeric(as.character(value)))
    substantive <- !is.na(value) & (is.na(num) | num >= 0)
  }
  if (operator == "is_missing") return(!substantive)
  if (operator == "not_missing") return(substantive)
  result <- pc6_ns(".condition_true")(value, operator, comparison)
  result[is.na(result)] <- FALSE
  substantive & result
}

# Each source value becomes one public state; domains are fixed BEFORE queries.
# Oracle returns state indices, not respondent text, birthdays or exact values.
pc6_domain <- function(occ, spec, dictionary) {
  kind <- spec$profile_kind
  options <- pc6_ns(".profile_response_options")(occ, spec, dictionary)
  option_number <- suppressWarnings(as.numeric(options$return_value))
  missing <- unique(c(pc6_ns(".administrative_missing_domain")()$return_value,
    pc6_ns(".coded_missing_domain")(options, character())$return_value,
    options$return_value[!is.na(option_number) & option_number < 0]))
  states <- c("database_missing", paste0("coded:", missing))
  substantive <- rep(FALSE, length(states))
  values <- rep("", length(states))
  labels <- states
  bins <- dictionary$safe_bins[dictionary$safe_bins$bin_spec_id == spec$bin_spec_id, , drop = FALSE]
  # Add threshold-aligned bands only for the two age fields. Never use DOB.
  if (occ$variable %in% c("AGE", "U_AGE")) {
    kind <- "integer"
    a <- dictionary$participation_age_bins
    if (is.null(a)) stop("Missing shared age-bin contract.")
    bins <- data.frame(bin_id = a$bin_id, lower = a$lower,
      upper = a$upper_exclusive, boundary_rules = "lower_inclusive_upper_exclusive", stringsAsFactors = FALSE)
  }
  if (kind %in% c("categorical", "ordered_categorical")) {
    options <- options[!options$return_value %in% missing, , drop = FALSE]
    states <- c(states, if (nrow(options)) paste0("code:", options$return_value) else character())
    values <- c(values, options$return_value)
    labels <- c(labels, options$display_value)
    substantive <- c(substantive, rep(TRUE, nrow(options)))
  } else if (kind %in% c("integer", "continuous", "date")) {
    states <- c(states, if (nrow(bins)) paste0("bin:", bins$bin_id) else character())
    values <- c(values, rep("", nrow(bins)))
    labels <- c(labels, bins$bin_id)
    substantive <- c(substantive, rep(TRUE, nrow(bins)))
  } else if (kind == "free_text") {
    states <- c(states, "text_present", "empty_text")
    labels <- c(labels, "Text present (content never collected)", "Empty text")
    values <- c(values, "", "")
    substantive <- c(substantive, TRUE, FALSE)
  }
  states <- c(states, "outside_public_support")
  labels <- c(labels, "Outside public support (value never collected)")
  values <- c(values, "")
  substantive <- c(substantive, FALSE)
  if (length(unique(states)) != length(states) || length(labels) != length(states) ||
      length(substantive) != length(states) || length(values) != length(states)) stop("Invalid public profile domain.")
  list(field = occ$variable, type = occ$data_type, kind = kind, missing = missing, bins = bins,
    states = states, values = values, labels = labels, substantive = substantive,
    age = occ$variable %in% c("AGE", "U_AGE"))
}
pc6_project_file <- function(x, domain) {
  d <- domain
  out <- rep(length(d$states), length(x))
  text <- as.character(x)
  if (d$kind == "date") {
    n <- pc6_ns(".safe_date_numeric")(x)
    for (code in d$missing) {
      number <- suppressWarnings(as.numeric(code))
      if (!is.na(number)) text[!is.na(n) & n == number] <- code
    }
  }
  out[is.na(x)] <- 1L
  mi <- match(text, d$missing)
  out[!is.na(mi) & !is.na(x)] <- mi[!is.na(mi) & !is.na(x)] + 1L
  available <- out == length(d$states)
  if (d$kind %in% c("categorical", "ordered_categorical")) {
    ix <- match(paste0("code:", text), d$states)
  } else if (d$kind %in% c("integer", "continuous", "date")) {
    ix <- match(paste0("bin:", pc6_ns(".bin_values")(x, d$bins, d$kind)), d$states)
    if (d$kind == "date" && grepl("CHAR", d$type)) {
      explicit <- grepl("^([0-9]{4}[-/][0-9]{2}[-/][0-9]{2}|[0-9]{2}/[0-9]{2}/[0-9]{4})$", as.character(x))
      ix[!explicit] <- NA_integer_
    }
  } else if (d$kind == "free_text") {
    ix <- match(ifelse(nzchar(text), "text_present", "empty_text"), d$states)
  } else ix <- rep(NA_integer_, length(x))
  out[available & !is.na(ix)] <- ix[available & !is.na(ix)]
  as.integer(out)
}
pc6_sql <- function(d) {
  q <- pc6_quote(d$field)
  numeric <- grepl("NUMBER|FLOAT|INTEGER|DOUBLE|DECIMAL", d$type)
  date <- grepl("DATE|TIMESTAMP", d$type)
  literal <- function(x) {
    if (date) return(paste0("DATE '1970-01-01' + (", as.numeric(x), ")"))
    if (numeric) {
      if (anyNA(suppressWarnings(as.numeric(x)))) stop("Non-numeric support for a numeric field.")
      return(as.character(as.numeric(x)))
    }
    pc6_literal(x)
  }
  cases <- paste0("WHEN ", q, " IS NULL THEN 1")
  for (i in seq_along(d$missing)) {
    if ((numeric || date) && is.na(suppressWarnings(as.numeric(d$missing[i])))) next
    cases <- c(cases, paste0("WHEN ", q, " = ", literal(d$missing[i]), " THEN ", i+1L))
  }
  if (d$kind %in% c("categorical", "ordered_categorical")) {
    ix <- which(startsWith(d$states, "code:"))
    for (i in ix) {
      # An impossible text option in a numeric dictionary field remains in the
      # public domain with zero count; never cast arbitrary source text.
      if (numeric && is.na(suppressWarnings(as.numeric(d$values[i])))) next
      cases <- c(cases, paste0("WHEN ", q, " = ", literal(d$values[i]), " THEN ", i))
    }
  } else if (d$kind %in% c("integer", "continuous", "date")) {
    if (d$kind == "date" && !date && grepl("CHAR", d$type)) {
      # Character dates: normalize only explicit formats and validate day/month
      # without calling TO_DATE on arbitrary source strings. Invalid dates stay
      # outside support; Oracle never returns the original text.
      iso <- paste0("CASE WHEN REGEXP_LIKE(", q, ", '^[0-9]{4}-[0-9]{2}-[0-9]{2}$') THEN ", q,
        " WHEN REGEXP_LIKE(", q, ", '^[0-9]{4}/[0-9]{2}/[0-9]{2}$') THEN REPLACE(", q, ", '/', '-')",
        " WHEN REGEXP_LIKE(", q, ", '^[0-9]{2}/[0-9]{2}/[0-9]{4}$') THEN SUBSTR(", q, ",7,4)||'-'||SUBSTR(", q, ",4,2)||'-'||SUBSTR(", q, ",1,2) END")
      yr <- paste0("TO_NUMBER(SUBSTR((", iso, "),1,4))")
      mo <- paste0("SUBSTR((", iso, "),6,2)")
      dy <- paste0("TO_NUMBER(SUBSTR((", iso, "),9,2))")
      maxday <- paste0("CASE WHEN ", mo, " IN ('04','06','09','11') THEN 30 WHEN ", mo,
        " = '02' THEN CASE WHEN MOD(", yr, ",400)=0 OR (MOD(", yr, ",4)=0 AND MOD(", yr,
        ",100)<>0) THEN 29 ELSE 28 END ELSE 31 END")
      q <- paste0("(CASE WHEN ", yr, " BETWEEN 1 AND 9999 AND ", mo,
        " BETWEEN '01' AND '12' AND ", dy, " BETWEEN 1 AND (", maxday, ") THEN (", iso, ") END)")
    } else if (!numeric && !date) stop("Numeric projection requires a numeric database field.")
    for (i in seq_len(nrow(d$bins))) {
      b <- d$bins[i, , drop = FALSE]
      rule <- pc6_ns(".safe_bin_boundary_rule")(d$bins, i)
      lo <- if (rule %in% c("exclusive", "lower_exclusive_upper_inclusive")) ">" else ">="
      hi <- if (rule %in% c("exclusive", "lower_inclusive_upper_exclusive")) "<" else "<="
      bounds <- character()
      for (side in c("lower", "upper")) if (nzchar(as.character(b[[side]]))) {
        lim <- if (date) paste0("DATE ", pc6_literal(b[[side]])) else if (d$kind == "date") pc6_literal(b[[side]]) else as.character(as.numeric(b[[side]]))
        bounds <- c(bounds, paste(q, if (side == "lower") lo else hi, lim))
      }
      cases <- c(cases, paste0("WHEN ", paste(bounds, collapse = " AND "), " THEN ", match(paste0("bin:", b$bin_id), d$states)))
    }
  } else if (d$kind == "free_text") {
    cases <- c(cases, paste0("WHEN LENGTH(", q, ") > 0 THEN ", match("text_present", d$states)))
  }
  paste0("CASE ", paste(cases, collapse = " "), " ELSE ", length(d$states), " END")
}
pc6_fetch <- function(source, reg, keys, domains, raw = NULL) {
  one <- function(ds) {
    fields <- vapply(ds, `[[`, character(1), "field")
    if (source$kind == "files") {
      if (!all(fields %in% names(raw))) stop("Field unavailable.")
      rows <- pc6_align(raw[c(reg$observation_key, fields)], reg$observation_key, keys)
      return(setNames(lapply(ds, function(d) pc6_project_file(rows[[d$field]], d)), fields))
    }
    expressions <- vapply(ds, function(d) paste(pc6_sql(d), "AS", pc6_quote(d$field)), character(1))
    rows <- pc6_align(pc6_query(source, reg, c(pc6_quote(reg$observation_key), expressions)), reg$observation_key, keys)
    setNames(lapply(ds, function(d) {
      x <- suppressWarnings(as.numeric(as.character(rows[[d$field]])))
      if (anyNA(x) || any(x != floor(x) | x < 1 | x > length(d$states))) stop("Invalid public-state projection.")
      as.integer(x)
    }), fields)
  }
  result <- tryCatch(one(domains), error = identity)
  if (!inherits(result, "error")) return(list(data = result, failures = character()))
  # Never downgrade an alignment failure into field retries.
  if (grepl("Observation keys|Missing observation key", conditionMessage(result))) stop(conditionMessage(result))
  if (length(domains) == 1L) return(list(data = list(), failures = domains[[1L]]$field))
  half <- seq_len(floor(length(domains)/2L))
  a <- pc6_fetch(source, reg, keys, domains[half], raw)
  b <- pc6_fetch(source, reg, keys, domains[-half], raw)
  list(data = c(a$data, b$data), failures = c(a$failures, b$failures))
}
pc6_stage <- function(round, marker, anchors, answer, fully_observed = TRUE) {
  n <- length(answer)
  detail <- rep("undetermined", n)
  detail[answer] <- "response_evidenced"
  if (startsWith(round, "react1.")) {
    if (!is.null(marker)) {
      detail[marker == "code:1"] <- "recorded_complete"
      detail[marker == "code:0"] <- "recorded_breakoff"
      detail[marker == "coded:-555" & !answer] <- "source_555"
      detail[marker == "coded:-77" & answer] <- "discordant_flag_and_answer"
      if (fully_observed) detail[marker == "coded:-77" & !answer] <- "shared_nonresponse"
    }
  } else if (length(anchors) >= 2L && fully_observed) {
    all77 <- Reduce(`&`, lapply(anchors, function(x) x == "coded:-77"))
    detail[all77 & !answer] <- "shared_nonresponse"
  }
  state <- ifelse(detail == "shared_nonresponse", "shared_nonresponse",
    ifelse(answer | detail == "recorded_complete", "response_evidenced", "undetermined"))
  list(state = state, detail = detail)
}
pc6_counts <- function(round, table_name, field, reference, x, xlevels, y, ylevels) {
  if (anyNA(match(x, xlevels)) || anyNA(match(y, ylevels))) stop("Non-public profile state.")
  z <- as.data.frame(table(factor(x, levels = xlevels), factor(y, levels = ylevels)), stringsAsFactors = FALSE)
  names(z) <- c("state", "reference_state", "count")
  data.frame(round_id = round, table = table_name, field = field, reference = reference,
    state = as.character(z$state), reference_state = as.character(z$reference_state), count = as.numeric(z$count),
    field_kind = "public_value_state", reference_kind = "participation_context", stringsAsFactors = FALSE)
}
pc6_meta <- function(x, key) x$manifest$value[match(key, x$manifest$key)]

# Two bounded passes per round: first identify response evidence, then capture
# distributions. Only aggregate checkpoints are saved. No crosswalk is queried.
pc6_run <- function(source, contract = "contract", rounds = "all", batch_size = 30L,
                    checkpoint = "participation-v6-INTERNAL", progress = TRUE,
                    resume = FALSE, source_label) {
  if (!source$kind %in% c("oracle", "files")) stop("Use an Oracle or file source.")
  if (missing(source_label) || length(source_label) != 1L || !nzchar(source_label)) stop("Supply a source_label for this fixed database snapshot (no credentials).")
  if (length(batch_size) != 1L || is.na(batch_size) || batch_size < 1 || batch_size != as.integer(batch_size)) stop("Invalid batch_size.")
  c6 <- pc6_contract(contract)
  dictionary <- reactextract::react_dictionary()
  dictionary$participation_age_bins <- c6$age_context
  code_path <- file.path(dirname(normalizePath(contract, mustWork=TRUE)), "profile.R")
  if (!file.exists(code_path)) stop("Use the complete kit: profile.R must sit alongside the contract folder.")
  implementation_hash <- pc6_hash(code_path)
  ids <- pc6_ns(".resolve_rounds")(rounds, dictionary$rounds)
  registry <- if (source$kind == "oracle") source$registry else dictionary$source_registry
  config <- list(contract_hash = c6$hash, implementation_hash = implementation_hash,
    package_version = as.character(utils::packageVersion("reactextract")),
    source_kind = source$kind, registry = registry, source_label = source_label, rounds = ids)
  if (dir.exists(checkpoint)) {
    if (!resume || !file.exists(file.path(checkpoint, "configuration.rds")) ||
        !identical(readRDS(file.path(checkpoint, "configuration.rds")), config)) stop("Existing checkpoint: use resume=TRUE with the identical snapshot, rounds, package and contract, or a fresh directory.")
  } else {
    dir.create(checkpoint, recursive = TRUE)
    saveRDS(config, file.path(checkpoint, "configuration.rds"))
  }
  specs <- pc6_ns(".approved_profile_specs")(dictionary)
  p <- c6$occurrence_participation
  spec <- specs[match(p$occurrence_id, specs$occurrence_id), , drop = FALSE]
  safe <- !is.na(spec$profile_kind) & !spec$profile_kind %in% "identifier" &
    !spec$generation_action %in% c("excluded", "synthetic_identifier") & !p$variable %in% c("DOB", "DATEOFBIRTH")
  p <- p[safe, , drop = FALSE]
  spec <- spec[safe, , drop = FALSE]
  results <- list()
  started <- proc.time()[[3L]]
  report <- function(...) if (progress) message("[reactextract v6 capture] ", ...)
  for (ri in seq_along(ids)) {
    round <- ids[ri]
    saved <- file.path(checkpoint, paste0(round, ".rds"))
    if (file.exists(saved)) {
      part <- readRDS(saved)
      if (!identical(part$contract_hash, c6$hash) || !identical(part$round_id, round)) stop("Invalid round checkpoint.")
      report(round, " | using completed aggregate checkpoint")
      results[[round]] <- part
      next
    }
    t0 <- proc.time()[[3L]]
    reg <- registry[registry$round_id == round, , drop = FALSE]
    raw <- NULL
    if (source$kind == "files") {
      entry <- source$rounds[[round]]
      if (is.null(entry)) stop("Round file unavailable: ", round)
      raw <- if (is.data.frame(entry)) entry else pc6_ns(".read_data_file")(entry)
      base <- raw[reg$observation_key]
    } else base <- pc6_query(source, reg, pc6_quote(reg$observation_key))
    keys <- as.character(base[[reg$observation_key]])
    pc6_align(base, reg$observation_key, keys)
    n <- length(keys)
    rows <- which(p$round_id == round)
    pp <- p[rows, , drop = FALSE]
    ss <- spec[rows, , drop = FALSE]
    oo <- dictionary$occurrences[match(pp$occurrence_id, dictionary$occurrences$occurrence_id), , drop = FALSE]
    ds <- lapply(seq_len(nrow(pp)), function(i) pc6_domain(oo[i, , drop = FALSE], ss[i, , drop = FALSE], dictionary))
    names(ds) <- pp$variable
    batches <- split(seq_len(nrow(pp)), ceiling(seq_len(nrow(pp))/batch_size))
    answer <- list(registration = rep(FALSE, n), individual = rep(FALSE, n))
    fully <- c(registration = TRUE, individual = TRUE)
    selected <- list()
    failures <- character()
    pass_hashes <- list()
    report("Round ", ri, "/", length(ids), " ", round, " | ", n, " records | ", nrow(pp), " safe fields")
    for (bi in seq_along(batches)) {
      idx <- batches[[bi]]
      report(round, " | participation pass ", bi, "/", length(batches), " | ", length(idx), " fields")
      fetched <- pc6_fetch(source, reg, keys, ds[idx], raw)
      failures <- union(failures, fetched$failures)
      for (i in idx) {
        field <- pp$variable[i]
        stage <- pp$stage[i]
        if (!field %in% names(fetched$data)) {
          if (stage %in% names(fully)) fully[stage] <- FALSE
          next
        }
        value <- fetched$data[[field]]
        pass_hashes[[field]] <- digest::digest(value, algo = "sha256")
        d <- ds[[field]]
        if (stage %in% names(answer)) answer[[stage]] <- answer[[stage]] | d$substantive[value]
        if (field %in% c("U_AGE", "SFREPORTFIG", "REGREPORTFIG", "INDCONFSF", "ABATTEMPT", "ABCOMP")) selected[[field]] <- d$states[value]
      }
    }
    if (length(failures) == nrow(pp)) stop("No safe fields could be read in ", round, "; no round checkpoint was saved.")
    individual <- pc6_stage(round, selected$SFREPORTFIG,
      selected[intersect(c("INDCONFSF", "ABATTEMPT", "ABCOMP"), names(selected))], answer$individual, fully[["individual"]])
    # Registration agreement is never interpreted as survey completion/nonresponse.
    registration <- ifelse(answer$registration, "response_evidenced", "undetermined")
    age <- if (is.null(selected$U_AGE)) rep("missing_or_invalid", n) else
      c6$age_context$band[match(sub("^bin:", "", selected$U_AGE), c6$age_context$bin_id)]
    age[!age %in% pc6_age_levels] <- "missing_or_invalid"
    context <- paste(registration, age, sep = "|")
    context_levels <- as.vector(outer(c("response_evidenced", "undetermined"), pc6_age_levels, paste, sep = "|"))
    counts <- list(pc6_counts(round, "participation_by_age", "individual_stage_detail", "registration_and_NHS_age",
      individual$detail, pc6_detail_levels, context, context_levels))
    domain_rows <- list()
    for (bi in seq_along(batches)) {
      idx <- batches[[bi]]
      report(round, " | conditional profile pass ", bi, "/", length(batches), " | elapsed ", round(proc.time()[[3L]]-t0), "s")
      fetched <- pc6_fetch(source, reg, keys, ds[idx], raw)
      # Fail closed if evidence availability changes between the two passes.
      if (!setequal(fetched$failures, intersect(failures, pp$variable[idx]))) stop("Field availability changed between passes. No checkpoint saved for ", round, ".")
      for (i in idx) {
        field <- pp$variable[i]
        if (!field %in% names(fetched$data)) next
        d <- ds[[field]]
        if (!identical(digest::digest(fetched$data[[field]], algo = "sha256"), pass_hashes[[field]])) stop("Source values changed between passes. No checkpoint saved for ", round, ".")
        value <- d$states[fetched$data[[field]]]
        scope <- pp$stage[i]
        state <- if (scope == "individual") individual$state else if (scope == "registration") registration else rep("not_assigned", n)
        levels <- if (scope == "individual") pc6_stage_levels else if (scope == "registration") c("response_evidenced", "undetermined") else "not_assigned"
        # Capture age strata for every questionnaire field, not an assumed final
        # eligibility mask. Later rule corrections can reuse these aggregates.
        use_age <- scope != "unassigned" || field %in% c("AGE", "U_AGE") || nzchar(pp$minimum_age[i])
        ref <- if (use_age) paste(state, age, sep = "|") else state
        refs <- if (use_age) as.vector(outer(levels, pc6_age_levels, paste, sep = "|")) else levels
        counts[[length(counts)+1L]] <- pc6_counts(round, "field_by_participation_age", pp$occurrence_id[i],
          paste0(scope, if (use_age) "_and_NHS_age" else ""), value, d$states, ref, refs)
        domain_rows[[length(domain_rows)+1L]] <- data.frame(occurrence_id = pp$occurrence_id[i], state = d$states,
          label = d$labels, substantive = d$substantive, stringsAsFactors = FALSE)
      }
    }
    issue <- if (length(failures)) data.frame(round_id = round, variable = failures, issue = "field_unavailable_no_nonresponse_inference_if_stage_incomplete") else
      data.frame(round_id = character(), variable = character(), issue = character())
    part <- list(round_id = round, contract_hash = c6$hash, counts = do.call(rbind, counts),
      domains = do.call(rbind, domain_rows), issues = issue,
      timing = data.frame(round_id = round, elapsed_seconds = proc.time()[[3L]]-t0))
    # Save only complete-round aggregates, never keys, rows, values or state vectors.
    temp <- paste0(saved, ".tmp")
    saveRDS(part, temp)
    if (!file.rename(temp, saved)) stop("Could not save round checkpoint.")
    results[[round]] <- part
    rm(raw, keys, base, fetched, selected, answer)
    report(round, " | aggregate checkpoint saved")
  }
  result <- list(counts = do.call(rbind, lapply(results, `[[`, "counts")),
    domains = do.call(rbind, lapply(results, `[[`, "domains")),
    issues = do.call(rbind, lapply(results, `[[`, "issues")),
    timings = do.call(rbind, lapply(results, `[[`, "timing")),
    manifest = data.frame(key = c("schema", "status", "dictionary_manifest_sha256", "capture_contract_sha256", "implementation_sha256", "package_version",
      "rounds", "source_label", "elapsed_seconds", "eligibility_status", "registration_status", "generator_ready"),
      value = c("participation-conditional-capture-v1", pc6_internal, reactextract::react_dictionary_version()$manifest_sha256,
        c6$hash, implementation_hash, as.character(utils::packageVersion("reactextract")), paste(ids, collapse = "|"), source_label,
        as.character(proc.time()[[3L]]-started), "age_stratified_not_fully_route_conditioned",
        "response_evidence_only_no_shared_nonresponse_inference", "FALSE"), stringsAsFactors = FALSE))
  saveRDS(result, file.path(checkpoint, "conditional-profile-INTERNAL.rds"))
  result
}

pc6_prepare_export <- function(x, progress = TRUE) {
  if (!identical(pc6_meta(x, "status"), pc6_internal) || "suppressed" %in% names(x$counts)) stop("Use the internal aggregate result, not an exported folder.")
  # Reuse the independently tested linked row/column complementary suppression
  # policy from the v3 follow-up. No automatic release approval is implied.
  z <- list(manifest = data.frame(key = c("schema", "status", "dictionary_manifest_sha256"),
    value = c("participation-targeted-v3", pf6_internal, pf6_dictionary_hash)))
  parts <- list()
  rounds <- unique(x$counts$round_id)
  # The disclosure policy's partitions and semantic-copy keys include round.
  # Applying it one round at a time is equivalent and bounds temporary memory.
  for (i in seq_along(rounds)) {
    if (progress) message("[reactextract v6 capture] Protecting aggregate counts ", i, "/", length(rounds), " ", rounds[i])
    z$counts <- x$counts[x$counts$round_id == rounds[i], , drop = FALSE]
    protected <- pf6_prepare_export(z)
    parts[[i]] <- protected$counts
  }
  out <- x
  out$counts <- do.call(rbind, parts)
  out$manifest$value[out$manifest$key == "status"] <- pc6_pending
  # Snapshot labels and timings are enclave-only: no paths or free-form strings.
  out$manifest <- out$manifest[!out$manifest$key %in% c("source_label", "elapsed_seconds"), , drop = FALSE]
  out$timings <- NULL
  out$manifest <- rbind(out$manifest, protected$manifest[!protected$manifest$key %in% c("schema", "status", "dictionary_manifest_sha256"), ])
  out
}
pc6_write <- function(x, path) {
  if (!identical(pc6_meta(x, "status"), pc6_pending)) stop("Prepare disclosure controls before writing.")
  d <- x$counts
  if (!all(c("suppressed", "count", "protection") %in% names(d)) || anyNA(d$suppressed) ||
      any(is.na(d$count) != d$suppressed) || any(!is.na(d$count) & (d$count < 10 | d$count %% 5 != 0))) stop("Invalid protected counts.")
  if (file.exists(path) || dir.exists(path)) stop("Choose a fresh output folder; never overwrite an export.")
  dir.create(path, recursive = TRUE)
  # Fixed whitelist; never serialize the supplied object wholesale.
  tables <- list(counts = d, domains = x$domains, issues = x$issues, manifest = x$manifest)
  for (name in names(tables)) utils::write.csv(tables[[name]], file.path(path, paste0(name, ".csv")), row.names = FALSE, na = "")
  writeLines(c("Conditional profile candidate: normal enclave disclosure approval required.",
    "Not a generator-ready v6 profile. Age/stage strata are captured; unresolved compound routes are not claimed as eligibility.",
    "Review linked tables and previous profile/diagnostic releases together. Do not infer suppressed counts from totals.",
    "No identifiers, birthdays or respondent text are included. Never export the INTERNAL checkpoint directory."), file.path(path, "README.txt"))
  files <- sort(list.files(path))
  utils::write.csv(data.frame(file = files, sha256 = vapply(file.path(path, files), pc6_hash, character(1))), file.path(path, "checksums.csv"), row.names = FALSE)
  invisible(path)
}
