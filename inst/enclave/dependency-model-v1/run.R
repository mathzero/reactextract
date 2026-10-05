dm_atomic_save <- function(value, path) {
  temp <- tempfile(".pending-", tmpdir = dirname(path))
  on.exit(unlink(temp), add = TRUE)
  saveRDS(value, temp, compress = "gzip")
  backup <- NULL
  if (file.exists(path)) {
    backup <- tempfile(".previous-", tmpdir = dirname(path))
    if (!file.rename(path, backup)) stop("Could not replace previous checkpoint; close files using this folder.")
  }
  if (!file.rename(temp, path)) {
    if (!is.null(backup)) file.rename(backup, path)
    stop("Could not save checkpoint; previous version retained.")
  }
  if (!is.null(backup)) unlink(backup)
}
dm_marginals <- function(data) {
  do.call(rbind, lapply(names(data), function(field) {
    z <- table(data[[field]])
    data.frame(field = field, state = names(z), count = as.numeric(z))
  }))
}
dm_fit_round <- function(source, round, dictionary, contract, batch_size, seed, synthetic_n, progress) {
  started <- proc.time()[[3L]]
  capture <- dm_capture(source, round, dictionary, contract, batch_size, progress)
  after_capture <- proc.time()[[3L]]
  state <- dm_states(capture, dictionary, contract, round)
  data <- state$data
  # Product/timing are profiled, but not promoted to predictors until their
  # occurrence and time-reference choices have passed the consolidated review.
  predictors <- setdiff(names(data), c("outcome", "outcome_availability", "vaccine_product", "vaccine_elapsed"))
  progress("fitting outcome model | ", nrow(data), " records | ", length(predictors), " candidate predictors")
  fit <- dm_fit_outcome(data, predictors, dm_seed(seed, round, "fit"))
  after_fit <- proc.time()[[3L]]
  tables <- dm_fit_state_sampler(data, predictors)
  compare <- generated <- fallback <- NULL
  checks <- list(outcome_domain = all(data$outcome %in% dm_ns(".dependency_outcome_levels")(capture$plan$outcome_id)),
    no_identifiers_in_modelling_states = !any(c("U_PASSCODE", "SUBJECT_ID", "DOB") %in% names(data)))
  if (!is.null(fit$model)) {
    progress("checking a fictional modelling-state sample (not full extracted raw data)")
    generated <- dm_generate_states(tables, fit$model, synthetic_n, seed, round)
    checks$synthetic_rows <- nrow(generated$data) == synthetic_n
    checks$finite_predictions <- all(is.finite(dm_predict(fit$model, generated$data)))
    checks$valid_outcome_availability <- all((generated$data$outcome_availability == "evaluable") ==
      (generated$data$outcome %in% c("positive", "negative")))
    compare <- dm_marginals(generated$data); fallback <- generated$fallbacks
  }
  if (!all(unlist(checks))) stop("Internal modelling invariant failed; candidate not saved.")
  list(schema = "dependency-model-internal-v1", round_id = round, status = fit$status,
    outcome_id = capture$plan$outcome_id, package_version = as.character(utils::packageVersion("reactextract")),
    fit = fit, conditional_tables = tables, symptom_tables = state$symptoms,
    symptom_metadata = state$symptom_metadata, ct_tables = state$ct,
    real_marginals = dm_marginals(data), synthetic_marginals = compare, sampling_fallbacks = fallback,
    vaccine = state$vaccine, fields = state$source_fields, unavailable_fields = capture$failures,
    participation_scan_complete = capture$complete_stage_scan, eligibility_skips = state$eligibility_skips,
    checks = checks, n = capture$n,
    limitations = c("not_a_generator_ready_profile", "coefficient_release_needs_separate_disclosure_review",
      "no_interaction_terms_in_first_candidate", "product_and_elapsed_effects_held_for_review",
      "symptom_tables_are_exact_occurrence_marginals_not_a_joint_symptom_vector",
      "ct_tables_not_yet_a_joint_gene_result_encoder", "synthetic_check_covers_model_states_not_full_raw_outputs"),
    timings = c(capture = after_capture-started, fit = after_fit-after_capture,
      remaining = proc.time()[[3L]]-after_fit))
}
dm_summary <- function(directory, rounds) {
  rows <- lapply(rounds, function(round) {
    file <- file.path(directory, paste0(round, ".rds"))
    failed <- file.path(directory, paste0(round, ".error.rds"))
    if (file.exists(file)) {
      z <- readRDS(file)
      data.frame(round = round, status = z$status,
        input_review = if (length(z$unavailable_fields)) "unavailable_fields_recorded" else "no_field_failures",
        scan = if (all(z$participation_scan_complete)) "complete" else "incomplete_no_blanket_nonresponse_inference")
    } else data.frame(round = round, status = if (file.exists(failed)) "round_failed_see_internal_report" else "not_run",
      input_review = "not_complete", scan = "not_complete")
  })
  do.call(rbind, rows)
}
dm_report <- function(directory, rounds) {
  summary <- dm_summary(directory, rounds)
  lines <- c("# Dependency modelling run status", "",
    "Operational status only. This file contains no counts, coefficients, source values or error messages.",
    "It is not a declaration of disclosure approval or generator readiness.", "",
    "| Round | Outcome model | Input review | Participation scan |", "|---|---|---|---|")
  for (i in seq_len(nrow(summary))) lines <- c(lines, paste0("| ", paste(summary[i, ], collapse = " | "), " |"))
  writeLines(lines, file.path(directory, "RUN_STATUS.md"))
  detail <- c("# INTERNAL — do not remove from enclave", "", "All numbers below are unsuppressed. Models and linked tables require joint disclosure review.", "")
  for (round in rounds) {
    file <- file.path(directory, paste0(round, ".rds")); err <- file.path(directory, paste0(round, ".error.rds"))
    detail <- c(detail, paste0("## ", round), "")
    if (file.exists(file)) {
      z <- readRDS(file)
      detail <- c(detail, paste("Status:", z$status), paste("Records:", z$n),
        paste("Unavailable fields:", paste(z$unavailable_fields, collapse = ", ")),
        paste("Elapsed seconds:", round(sum(z$timings), 1)),
        "", "Validation:", "```", capture.output(print(z$fit$validation)), "```",
        "", "Fitting diagnostics (internal only):", "```", capture.output(print(z$fit$diagnostics)), "```",
        "", "Limits:", paste0("- ", z$limitations), "")
    } else if (file.exists(err)) detail <- c(detail, readRDS(err)$message, "")
    else detail <- c(detail, "Not run.", "")
  }
  writeLines(detail, file.path(directory, "INTERNAL_REVIEW.md"))
  invisible(summary)
}
dm_run <- function(source, source_label, output = "dependency-model-INTERNAL-v1", rounds = "all",
                   resume = TRUE, batch_size = 30L, seed = 20260928L, synthetic_n = 10000L, progress = TRUE,
                   previous_output = NULL, retry_failed = FALSE) {
  if (!source$kind %in% c("oracle", "files")) stop("Use an Oracle or file source.")
  if (missing(source_label) || length(source_label) != 1L || is.na(source_label) || !nzchar(source_label))
    stop("Supply a non-secret source_label for the fixed database snapshot.")
  for (value in list(batch_size, synthetic_n, seed)) if (length(value) != 1L || is.na(value) || value < 1 || value != as.integer(value)) stop("Invalid run setting.")
  dictionary <- reactextract::react_dictionary(); contract <- dm_load_contract()
  ids <- dm_ns(".resolve_rounds")(rounds, dictionary$rounds)
  pilots <- intersect(c("react1.r13", "react2.r06"), ids)
  order <- unique(c(pilots, ids))
  file_hashes <- if (source$kind == "files") vapply(source$rounds[ids], function(x) {
    if (is.data.frame(x)) digest::digest(x, algo = "sha256") else digest::digest(file = x, algo = "sha256")
  }, character(1)) else NULL
  config <- list(schema = "dependency-model-run-v1", code_sha256 = dm_implementation_hash,
    dictionary_sha256 = reactextract::react_dictionary_version()$manifest_sha256,
    package_version = as.character(utils::packageVersion("reactextract")), source_kind = source$kind,
    source_label = source_label, registry = if (source$kind == "oracle") source$registry else dictionary$source_registry,
    file_hashes = file_hashes, rounds = ids, batch_size = batch_size, seed = seed, synthetic_n = synthetic_n)
  previous <- NULL
  if (!is.null(previous_output)) {
    if (dir.exists(output) || file.exists(output)) stop("Migration requires a fresh output folder; resume it later without previous_output.")
    previous_output <- normalizePath(previous_output, mustWork = TRUE)
    if (dir.exists(file.path(previous_output, ".running"))) stop("Previous run is locked; verify it has stopped first.")
    previous <- readRDS(file.path(previous_output, "configuration.rds"))
    if (!identical(previous$code_sha256, "76be80cc83b5dfe562e6386132fe0d7bdea55de8416a1a6345653829b5dccbc0"))
      stop("Only the original v1 kit is supported for migration.")
    compare <- previous; compare$code_sha256 <- config$code_sha256
    if (!identical(compare, config)) stop("Previous run settings, package, dictionary or source differ; cannot reuse rounds.")
  }
  if (dir.exists(output)) {
    file <- file.path(output, "configuration.rds")
    if (!resume || !file.exists(file) || !identical(readRDS(file), config))
      stop("Existing folder does not match this run. Use the same settings/snapshot to resume, or choose a fresh output folder.")
  } else {
    dir.create(output, recursive = TRUE); dm_atomic_save(config, file.path(output, "configuration.rds"))
  }
  output <- normalizePath(output)
  if (!file.create(file.path(output, ".write-test"))) stop("Output folder is not writable.")
  unlink(file.path(output, ".write-test"))
  lock <- file.path(output, ".running")
  if (!dir.create(lock, showWarnings = FALSE)) stop("Another run may be using this folder. If R stopped, verify it is no longer running before removing the empty .running folder.")
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  if (!is.null(previous)) {
    reused <- character()
    for (round in ids) {
      path <- file.path(previous_output, paste0(round, ".rds"))
      if (!file.exists(path)) next
      z <- readRDS(path)
      if (!identical(z$round_id, round) || !identical(z$schema, "dependency-model-internal-v1")) stop("Invalid previous checkpoint.")
      if (!identical(z$status, "fitted_pending_review")) next
      if (!file.copy(path, file.path(output, basename(path)), overwrite = FALSE)) stop("Checkpoint copy failed.")
      reused[round] <- digest::digest(file = path, algo = "sha256")
    }
    dm_atomic_save(list(previous_configuration = previous, reused_checkpoints = reused,
      note = "Successful v1 fits retained unchanged; other rounds use the revised solver."), file.path(output, "migration.rds"))
  }
  writeLines("INTERNAL ONLY: checkpoints, models and all numeric diagnostics require enclave disclosure review. Do not copy this folder out.", file.path(output, "DO_NOT_EXPORT.txt"))
  started <- proc.time()[[3L]]
  report <- function(...) if (progress) message("[dependency kit] ", ...)
  for (i in seq_along(order)) {
    round <- order[i]; file <- file.path(output, paste0(round, ".rds"))
    report("round ", i, "/", length(order), " ", round, " | elapsed ", round(proc.time()[[3L]]-started), "s")
    saved <- if (file.exists(file)) readRDS(file) else NULL
    retry <- !is.null(saved) && isTRUE(retry_failed) && saved$status %in% c("fitting_failed", "refit_failed")
    if (!is.null(saved) && !retry) {
      z <- saved
      if (!identical(z$round_id, round) || !identical(z$schema, "dependency-model-internal-v1")) stop("Invalid checkpoint.")
      report("using saved round")
    } else {
      if (retry) {
        archive <- tempfile(paste0(round, "-"), tmpdir = output, fileext = ".previous.rds")
        if (!file.rename(file, archive)) stop("Could not preserve failed checkpoint before retry.")
      }
      z <- tryCatch(dm_fit_round(source, round, dictionary, contract, batch_size, seed, synthetic_n, report), error = identity)
      if (inherits(z, "error")) {
        dm_atomic_save(list(round = round, message = conditionMessage(z)), file.path(output, paste0(round, ".error.rds")))
        report("round failed; details saved inside enclave")
      } else dm_atomic_save(z, file)
    }
    dm_report(output, ids)
    if (round %in% pilots && (inherits(z, "error") || !identical(z$status, "fitted_pending_review"))) {
      report("pilot check did not pass; remaining rounds held. Resolve the internal report and run the same command to resume.")
      break
    }
    rm(z); invisible(gc(FALSE))
  }
  report("finished; read RUN_STATUS.md and INTERNAL_REVIEW.md. Nothing has been approved for release.")
  invisible(dm_summary(output, ids))
}
dm_prepare_review <- function(output = "dependency-model-INTERNAL-v1", destination = "dependency-model-review-INTERNAL-v1") {
  # This is a disclosure officer's review workspace, NOT an export function.
  if (dir.exists(destination) || file.exists(destination)) stop("Choose a fresh review folder.")
  config <- readRDS(file.path(output, "configuration.rds"))
  if (!identical(config$code_sha256, dm_implementation_hash)) stop("Review kit does not match the run.")
  dm_report(output, config$rounds)
  dir.create(destination, recursive = TRUE)
  file.copy(file.path(output, c("RUN_STATUS.md", "INTERNAL_REVIEW.md")), destination)
  models <- list()
  if (file.exists(file.path(output, "migration.rds"))) file.copy(file.path(output, "migration.rds"), destination)
  for (round in config$rounds) {
    file <- file.path(output, paste0(round, ".rds")); if (!file.exists(file)) next
    z <- readRDS(file)
    models[[round]] <- z[c("round_id", "status", "fit", "conditional_tables", "symptom_tables", "symptom_metadata", "ct_tables", "vaccine", "fields", "limitations")]
  }
  saveRDS(models, file.path(destination, "parameters-AND-COUNTS-NOT-APPROVED.rds"))
  file.copy(file.path(dm_root, "REVIEWER.md"), destination)
  writeLines(c("# INTERNAL review candidate — NOT authorised for export", "",
    "This folder contains unsuppressed counts and unapproved model parameters.",
    "Keep it inside the enclave. The disclosure officer must select a release format and assess models and linked tables together.",
    "Do not treat model coefficients as protected merely because training records are absent.",
    "After review, a separate, explicitly approved release bundle is required; this kit does not create one."),
    file.path(destination, "DO_NOT_EXPORT.md"))
  invisible(normalizePath(destination))
}
