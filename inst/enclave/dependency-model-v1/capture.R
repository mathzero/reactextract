dm_ns <- function(name) getFromNamespace(name, "reactextract")
dm_load_contract <- function() {
  c6 <- pc6_contract(file.path(dm_root, "contract"))
  path <- file.path(dm_root, "rules")
  h <- pc6_read(file.path(path, "checksums.csv"))
  for (i in seq_len(nrow(h))) if (!identical(pc6_hash(file.path(path, h$file[i])), h$sha256[i])) stop("Rule checksum mismatch.")
  names <- c("rules", "predicates", "bindings", "targets", "approvals", "manifest")
  compact <- setNames(lapply(names, function(n) pc6_read(file.path(path, paste0(n, ".csv")))), names)
  version <- compact$manifest$value[compact$manifest$key == "dictionary_manifest_sha256"]
  if (!identical(version, reactextract::react_dictionary_version()$manifest_sha256[[1L]])) stop("Rule dictionary mismatch.")
  expanded <- dm_rules$.expand_synthetic_rules(compact)
  dm_rules$.validate_eligibility_context(expanded$eligibility, reactextract::react_dictionary())
  dm_rules$.validate_response_options(expanded$response_options, reactextract::react_dictionary())
  list(participation = c6, rules = expanded)
}
dm_round_plan <- function(dictionary, round, contract) {
  specs <- dm_ns(".dependency_specs")(dictionary, round)
  if (!nrow(specs)) stop("No approved outcome specification for round.")
  outcome <- unique(specs$outcome_id)
  stopifnot(length(outcome) == 1L)
  oo <- dictionary$occurrences[dictionary$occurrences$round_id == round, , drop = FALSE]
  outcomes <- dm_ns(".dependency_outcome_variables")(round, outcome)
  if (!all(outcomes %in% oo$variable)) stop("A required outcome field is absent from the dictionary.")
  predictors <- unique(unlist(lapply(seq_len(nrow(specs)), function(i) dm_ns(".dependency_occurrences")(specs[i, , drop = FALSE], dictionary)$variable)))
  extra <- oo$variable[grepl("^(VACCINE3(SYM)?|VACCINEREG|VACCDOSE(SYM)?|VACCINE(FIRST|SECOND|THIRD)(SYM)?|VACCINETYPE(SYM)?(_[0-9]+)?|SWABDATE_NEW|SWABDATE|ABDATE|U_AGE|SFREPORTFIG|REGREPORTFIG|INDCONFSF|ABATTEMPT|ABCOMP)$", oo$variable)]
  selected <- oo[oo$variable %in% unique(c(outcomes, predictors, extra)), , drop = FALSE]
  selected <- dm_rules$.eligibility_required_occurrences(dictionary, selected, contract$rules$eligibility, contract$rules$response_options)
  if (is.character(selected)) selected <- dictionary$occurrences[dictionary$occurrences$occurrence_id %in% selected, , drop = FALSE]
  prof <- dm_ns(".approved_profile_specs")(dictionary)
  good <- prof$occurrence_id[!prof$profile_kind %in% c("identifier", "free_text") & !prof$generation_action %in% c("excluded", "synthetic_identifier")]
  # LAB is a laboratory name, not respondent free text. The reviewed Round 1
  # outcome needs only its exact Eurofin/non-Eurofin distinction. It is used
  # transiently and never becomes a model predictor or exported level.
  if (round == "react1.r01") good <- union(good, oo$occurrence_id[oo$variable == "LAB"])
  selected <- selected[selected$round_id == round & selected$occurrence_id %in% good & !selected$variable %in% c("DOB", "DATEOFBIRTH"), , drop = FALSE]
  if (!all(outcomes %in% selected$variable)) stop("A required outcome field does not have a safe public specification.")
  list(specs = specs, outcome_id = outcome, outcome_fields = outcomes, occurrences = selected)
}
dm_raw_fetch <- function(source, reg, keys, fields, raw, batch_size, progress) {
  result <- list(); failures <- character()
  for (fields in split(fields, ceiling(seq_along(fields)/batch_size))) {
    if (source$kind == "files") {
      good <- intersect(fields, names(raw)); failures <- union(failures, setdiff(fields, good))
      z <- pc6_align(raw[c(reg$observation_key, good)], reg$observation_key, keys)
      result <- c(result, as.list(z[good]))
    } else {
      z <- dm_ns(".oracle_fetch_batch")(source, reg, keys, fields)
      if (any(z$issues$severity == "error" & grepl("key|align", z$issues$code))) stop("Observation key alignment failed.")
      result <- c(result, z$data); failures <- union(failures, setdiff(fields, names(z$data)))
    }
    progress("model fields received: ", length(result))
  }
  list(data = as.data.frame(result, check.names = FALSE, optional = TRUE), failures = failures)
}
dm_capture <- function(source, round, dictionary, contract, batch_size, progress) {
  plan <- dm_round_plan(dictionary, round, contract)
  registry <- if (source$kind == "oracle") source$registry else dictionary$source_registry
  reg <- registry[registry$round_id == round, , drop = FALSE]
  if (nrow(reg) != 1L) stop("Round registry is missing or duplicated.")
  raw <- NULL
  if (source$kind == "files") {
    entry <- source$rounds[[round]]
    if (is.null(entry)) stop("Round file is unavailable.")
    raw <- if (is.data.frame(entry)) entry else dm_ns(".read_data_file")(entry)
    base <- raw[reg$observation_key]
  } else base <- pc6_query(source, reg, pc6_quote(reg$observation_key))
  keys <- as.character(base[[reg$observation_key]])
  pc6_align(base, reg$observation_key, keys)
  keys <- sort(keys, method = "radix")
  n <- length(keys); if (!n) stop("Round has no observations.")
  p <- contract$participation$occurrence_participation
  sp <- dm_ns(".approved_profile_specs")(dictionary)
  s <- sp[match(p$occurrence_id, sp$occurrence_id), , drop = FALSE]
  safe <- !is.na(s$profile_kind) & !s$profile_kind %in% "identifier" &
    !s$generation_action %in% c("excluded", "synthetic_identifier") & !p$variable %in% c("DOB", "DATEOFBIRTH")
  ix <- which(p$round_id == round & safe); pp <- p[ix, , drop = FALSE]; ss <- s[ix, , drop = FALSE]
  oo <- dictionary$occurrences[match(pp$occurrence_id, dictionary$occurrences$occurrence_id), , drop = FALSE]
  dictionary$participation_age_bins <- contract$participation$age_context
  domains <- lapply(seq_len(nrow(pp)), function(i) pc6_domain(oo[i, , drop = FALSE], ss[i, , drop = FALSE], dictionary))
  names(domains) <- pp$variable
  answer <- list(registration = rep(FALSE, n), individual = rep(FALSE, n))
  fully <- c(registration = TRUE, individual = TRUE)
  anchors <- list(); hashes <- list(); failures <- character()
  batches <- split(seq_len(nrow(pp)), ceiling(seq_len(nrow(pp))/batch_size))
  for (bi in seq_along(batches)) {
    ii <- batches[[bi]]
    z <- pc6_fetch(source, reg, keys, domains[ii], raw)
    failures <- union(failures, z$failures)
    for (i in ii) {
      field <- pp$variable[i]; stage <- pp$stage[i]
      if (!field %in% names(z$data)) {if (stage %in% names(fully)) fully[stage] <- FALSE; next}
      value <- z$data[[field]]; domain <- domains[[field]]
      if (stage %in% names(answer)) answer[[stage]] <- answer[[stage]] | domain$substantive[value]
      if (field %in% c("SFREPORTFIG", "REGREPORTFIG", "INDCONFSF", "ABATTEMPT", "ABCOMP")) anchors[[field]] <- domain$states[value]
      if (field %in% plan$occurrences$variable) hashes[[field]] <- digest::digest(value, algo = "sha256")
    }
    progress("participation scan ", bi, "/", length(batches), " | ", n, " records | ", length(ii), " fields")
  }
  individual <- pc6_stage(round, anchors$SFREPORTFIG, anchors[intersect(c("INDCONFSF", "ABATTEMPT", "ABCOMP"), names(anchors))], answer$individual, fully[["individual"]])
  registration <- ifelse(answer$registration, "response_evidenced", "undetermined")
  fetched <- dm_raw_fetch(source, reg, keys, plan$occurrences$variable, raw, batch_size, progress)
  if (!all(plan$outcome_fields %in% names(fetched$data))) stop("Required outcome field unavailable; outcome model not fitted.")
  if (nrow(fetched$data) != n) stop("Model field row count mismatch.")
  for (field in intersect(names(hashes), names(fetched$data))) {
    projected <- pc6_project_file(fetched$data[[field]], domains[[field]])
    if (!identical(digest::digest(projected, algo = "sha256"), hashes[[field]]))
      stop("Source values changed between passes or SQL/file projection disagrees; rerun this round on a stable snapshot.")
  }
  all_failures <- union(failures, fetched$failures)
  for (field in setdiff(plan$occurrences$variable, names(fetched$data))) fetched$data[[field]] <- rep(NA_real_, n)
  list(raw = fetched$data, plan = plan, individual = individual, registration = registration,
    failures = all_failures, complete_stage_scan = fully, n = n)
}
dm_date <- function(x, type = NULL) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  text <- as.character(x); out <- as.Date(rep(NA_character_, length(text)))
  formats <- c("%Y-%m-%d", "%d/%m/%Y")
  patterns <- c("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", "^[0-9]{2}/[0-9]{2}/[0-9]{4}$")
  for (i in seq_along(formats)) {
    ix <- which(!is.na(text) & grepl(patterns[i], text) & is.na(out))
    if (!length(ix)) next
    value <- suppressWarnings(as.Date(text[ix], format = formats[i]))
    ok <- !is.na(value) & format(value, formats[i]) == text[ix]
    out[ix[ok]] <- value[ok]
  }
  out
}
dm_public_options <- function(dictionary, occurrences) {
  unique(dictionary$response_options[dictionary$response_options$occurrence_id %in% occurrences$occurrence_id,
    c("return_value", "display_value")])
}
dm_vaccine <- function(data, occurrences, dictionary, round) {
  n <- nrow(data)
  # One source stage per round; never coalesce a missing later answer from an
  # earlier stage. The earlier fields remain in the internal preflight checks.
  suffix <- if ("VACCINE3SYM" %in% names(data)) "SYM" else ""
  status_field <- paste0("VACCINE3", suffix)
  dose_field <- paste0("VACCDOSE", suffix)
  result <- list(fields = c(status = status_field, dose = dose_field), warnings = character())
  if (!status_field %in% names(data)) return(result)
  spec <- dm_ns(".dependency_specs")(dictionary, round)
  spec <- spec[spec$predictor_id == "vaccination_status", , drop = FALSE]
  if (!nrow(spec)) return(result)
  spec$source_fields <- status_field
  result$status <- dm_ns(".dependency_predictor_state")(data, spec, dictionary)
  result$dose <- rep("unreported", n)
  if (dose_field %in% names(data)) {
    o <- occurrences[occurrences$variable == dose_field, , drop = FALSE]
    opt <- dm_public_options(dictionary, o)
    lab <- tolower(trimws(opt$display_value))
    mapping <- setNames(ifelse(lab == "one", "one", ifelse(lab == "two", "two",
      ifelse(lab == "three", "three", ifelse(lab == "more than two", "three_plus",
        ifelse(lab == "more than three", "four_plus", "unreported"))))), opt$return_value)
    result$dose <- unname(mapping[as.character(data[[dose_field]])]); result$dose[is.na(result$dose)] <- "unreported"
  }
  result$dose[result$status == "no" & result$dose == "unreported"] <- "none"
  conflict <- result$status == "no" & result$dose %in% c("one", "two", "three", "three_plus", "four_plus")
  result$dose[conflict] <- "discordant"
  result$conflicts <- sum(conflict)
  result$product <- rep("unreported", n)
  prefix <- paste0("VACCINETYPE", suffix)
  product_rows <- occurrences[occurrences$variable == prefix | startsWith(occurrences$variable, paste0(prefix, "_")), , drop = FALSE]
  product_rows <- product_rows[!grepl("OTHER", product_rows$variable), , drop = FALSE]
  category <- function(label) {
    z <- tolower(label)
    ifelse(grepl("pfizer|bion", z), "pfizer", ifelse(grepl("astra|oxford", z), "astrazeneca",
      ifelse(grepl("moderna", z), "moderna", ifelse(grepl("janssen|johnson|johnsoon", z), "janssen",
        ifelse(grepl("other", z), "other", "unknown")))))
  }
  options <- product_rows[product_rows$variable != prefix, , drop = FALSE]
  if (nrow(options)) {
    selected <- lapply(seq_len(nrow(options)), function(i) data[[options$variable[i]]] == 1)
    selected <- do.call(cbind, selected); selected[is.na(selected)] <- FALSE
    count <- rowSums(selected); labels <- category(options$label)
    for (i in seq_along(labels)) result$product[count == 1 & selected[, i]] <- labels[i]
    result$product[count > 1] <- "multiple_selections"
  } else if (prefix %in% names(data)) {
    opt <- dm_public_options(dictionary, product_rows)
    mapping <- setNames(category(opt$display_value), opt$return_value)
    result$product <- unname(mapping[as.character(data[[prefix]])]); result$product[is.na(result$product)] <- "unreported"
  }
  # Date-based effects remain held unless a real test date is present. Exact
  # dates are converted to public elapsed bins inside the enclave only.
  reference <- if (startsWith(round, "react2")) "ABDATE" else
    intersect(c("SWABDATE_NEW", "SWABDATE"), names(data))[1L]
  result$elapsed <- rep("timing_unavailable", n)
  date_fields <- paste0("VACCINE", c("FIRST", "SECOND", "THIRD"), suffix)
  if (!is.na(reference) && reference %in% names(data)) {
    ref <- dm_date(data[[reference]])
    elapsed <- rep(NA_real_, n)
    doses <- c("one", "two", "three")
    for (i in 1:3) if (date_fields[i] %in% names(data)) {
      delta <- as.numeric(ref-dm_date(data[[date_fields[i]]]))
      hit <- result$dose == doses[i]
      elapsed[hit] <- delta[hit]
    }
    result$elapsed[!is.na(elapsed) & elapsed < 0] <- "reported_dose_after_test"
    result$elapsed[!is.na(elapsed) & elapsed >= 0 & elapsed <= 13] <- "0_13_days"
    result$elapsed[!is.na(elapsed) & elapsed >= 14 & elapsed <= 90] <- "14_90_days"
    result$elapsed[!is.na(elapsed) & elapsed > 90 & elapsed <= 180] <- "91_180_days"
    result$elapsed[!is.na(elapsed) & elapsed > 180 & elapsed <= 730] <- "181_730_days"
  }
  result$warnings <- c("vaccination_is_reported_at_selected_questionnaire_stage_not_verified_pre_test_history",
    "product_and_elapsed_terms_captured_for_review_but_excluded_from_first_outcome_fit")
  result
}
dm_states <- function(capture, dictionary, contract, round) {
  raw <- capture$raw; plan <- capture$plan; n <- nrow(raw)
  # Unknown laboratory text is counted only by field; never echo it in errors.
  if (startsWith(round, "react1")) for (v in intersect(c("RESULT", "FINALRESULT"), plan$outcome_fields))
    if (any(dm_ns(".dependency_lab_result_unknown")(raw[[v]], round, v, dictionary)))
      stop("Unrecognised laboratory result support; outcome model held. Use the existing enclave-only diagnostic.")
  if (startsWith(round, "react2")) for (v in plan$outcome_fields) {
    x <- suppressWarnings(as.numeric(raw[[v]]))
    codes <- if (v == "NEWRESULT_2") c(1:4) else 0:7
    missing <- is.na(raw[[v]]) | dm_ns(".dependency_admin_value")(raw[[v]])
    if (any(!missing & (is.na(x) | !x %in% codes)))
      stop("Unrecognised antibody result support; outcome model held.")
  }
  outcome <- dm_ns(".dependency_outcome_state")(raw, round, plan$outcome_id, dictionary)
  outcome[is.na(outcome)] <- "missing"
  # Shared evaluator is applied to a temporary copy, never real source data.
  age <- if ("U_AGE" %in% names(raw)) suppressWarnings(as.numeric(raw$U_AGE)) else rep(NA_real_, n)
  age[!is.finite(age) | age < 0 | age >= 121] <- NA_real_
  gated <- dm_rules$.apply_eligibility_context(raw, plan$occurrences, dictionary, list(age = age),
    contract$rules$eligibility, return_skips = TRUE, options = contract$rules$response_options)
  d <- data.frame(participation = capture$individual$detail, registration = capture$registration,
    outcome = outcome, outcome_availability = ifelse(outcome %in% c("negative", "positive"), "evaluable", outcome))
  p <- contract$participation$occurrence_participation
  mask_state <- function(value, variables) {
    o <- plan$occurrences[plan$occurrences$variable %in% variables, , drop = FALSE]
    pp <- p[match(o$occurrence_id, p$occurrence_id), , drop = FALSE]
    if (nrow(pp) && all(pp$stage == "individual" & pp$governed == "TRUE"))
      value[capture$individual$state == "shared_nonresponse"] <- "survey_nonresponse"
    masks <- gated$skipped[intersect(variables, names(gated$skipped))]
    if (length(masks) == length(variables) && length(masks)) {
      skip <- Reduce(`&`, masks)
      value[skip & value != "survey_nonresponse"] <- "structural_skip"
    }
    value[is.na(value)] <- "missing"; value
  }
  for (i in seq_len(nrow(plan$specs))) {
    s <- plan$specs[i, , drop = FALSE]
    if (s$predictor_id == "classic_symptom_status") next
    oo <- dm_ns(".dependency_occurrences")(s, dictionary)
    # A failed field becomes an explicit unavailable predictor rather than a
    # substantive category inferred from the remaining checkbox columns.
    if (any(oo$variable %in% capture$failures)) d[[s$predictor_id]] <- "field_unavailable"
    else d[[s$predictor_id]] <- mask_state(dm_ns(".dependency_predictor_state")(gated$data, s, dictionary), oo$variable)
  }
  vaccine <- dm_vaccine(gated$data, plan$occurrences, dictionary, round)
  if (!is.null(vaccine$status)) {
    d$vaccination_status <- mask_state(vaccine$status, vaccine$fields[["status"]])
    d$vaccine_dose <- mask_state(vaccine$dose, vaccine$fields[["dose"]])
    d$vaccine_product <- vaccine$product; d$vaccine_elapsed <- vaccine$elapsed
  }
  # Keep each exact symptom occurrence and its source wording/window; no
  # equivalence is inferred from a label or a paper's one-week definition.
  wanted <- paste0("health.acute_symptoms.", c("loss_or_change_smell", "loss_or_change_taste", "cough", "fever", "chills", "decreased_appetite", "aching_muscles", "runny_nose", "sore_throat"))
  symptoms <- plan$occurrences[plan$occurrences$primary_concept_id %in% wanted & grepl("^SYMPTANY", plan$occurrences$variable), , drop = FALSE]
  tables <- list(); symptom_notes <- list()
  for (i in seq_len(nrow(symptoms))) {
    o <- symptoms[i, , drop = FALSE]; field <- o$variable
    if (field %in% capture$failures) next
    opt <- dm_public_options(dictionary, o)
    codes <- suppressWarnings(as.numeric(opt$return_value)); codes <- unique(codes[!is.na(codes) & codes >= 0])
    if (!setequal(codes, c(0,1))) next
    x <- gated$data[[field]]
    state <- ifelse(!is.na(x) & x == 1, "selected", ifelse(!is.na(x) & x == 0, "not_selected", "item_missing"))
    state <- mask_state(state, field)
    dd <- d; dd$symptom <- state
    tables[[o$occurrence_id]] <- dm_table(dd, "symptom", intersect(c("outcome", "age_band", "participation"), names(d)))
    symptom_notes[[o$occurrence_id]] <- o[c("occurrence_id", "variable", "label", "primary_concept_id")]
  }
  ct <- intersect(c("CT_VALUE1", "CT_VALUE2", "NGENE_CTVALUE", "EGENE_CTVALUE"), names(raw))
  ct_tables <- lapply(ct, function(field) {
    x <- suppressWarnings(as.numeric(raw[[field]]))
    state <- rep("missing_or_outside_support", n)
    state[!is.na(x) & x == 0] <- "zero"
    bins <- cut(x, c(0,10,20,30,40,50,60), labels = c("over0_to10", "over10_to20", "over20_to30", "over30_to40", "over40_to50", "over50_to60"))
    state[!is.na(bins)] <- as.character(bins[!is.na(bins)])
    dd <- d; dd$ct_state <- state
    dm_table(dd, "ct_state", "outcome")
  }); names(ct_tables) <- ct
  list(data = d, symptoms = tables, symptom_metadata = symptom_notes, ct = ct_tables,
    vaccine = vaccine[c("fields", "warnings", "conflicts")],
    source_fields = plan$occurrences[c("occurrence_id", "variable", "label", "data_type")],
    eligibility_skips = vapply(gated$skipped, sum, numeric(1)))
}
