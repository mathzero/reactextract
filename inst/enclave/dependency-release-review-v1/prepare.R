# INTERNAL selection only. This is not an authorised export writer.
ds_hash <- function(path) digest::digest(file=path,algo="sha256")
ds_protect_table <- function(spec) {
  if (!identical(spec$kind,"conditional_counts_internal") || !length(spec$tables)) stop("Invalid count specification.")
  # Keep only the most detailed table. Do not also release overlapping fallback
  # marginals, totals, exact proportions or suppression reasons by cell.
  parents <- spec$parents[[1L]]; target <- spec$target
  tab <- spec$tables[[1L]]
  if (!identical(names(tab),c(parents,target,"count")) || anyDuplicated(names(tab)) ||
      !is.numeric(tab$count) || anyNA(tab$count) || any(!is.finite(tab$count)) ||
      any(tab$count<0 | tab$count!=floor(tab$count)) ||
      anyNA(tab[c(parents,target)])) stop("Invalid count table.")
  if (anyDuplicated(tab[c(parents,target)])) stop("Duplicate count cells.")
  # Conservative complementary protection: if any cell in a conditional
  # distribution is below 10, hide the entire distribution, not just one cell.
  group <- if(length(parents)) interaction(tab[parents],drop=TRUE,lex.order=TRUE) else factor(rep("all",nrow(tab)))
  suppressed <- rep(FALSE,nrow(tab))
  for (ix in split(seq_len(nrow(tab)),group)) if (any(tab$count[ix]<10)) suppressed[ix] <- TRUE
  tab$count[suppressed] <- NA_real_
  tab$count <- 5*floor(tab$count/5+.5)
  tab$suppressed <- suppressed
  list(target=target,parents=parents,table=tab,
    policy="below_10_whole_conditional_distribution_suppression_round5",
    cross_table_disclosure_review="pending")
}
ds_model <- function(fit) {
  m <- fit$model
  if (!identical(m$kind,"ridge_logistic_v1") || !is.list(m$levels) ||
      is.null(names(m$levels)) || anyDuplicated(names(m$levels))) stop("Invalid model.")
  for (l in m$levels) if (!is.character(l) || anyNA(l) || anyDuplicated(l) || length(l)<2L) stop("Invalid model levels.")
  expected <- c("(Intercept)",unlist(lapply(names(m$levels),function(f) paste(f,m$levels[[f]][-1],sep="=")),use.names=FALSE))
  if (!is.numeric(m$coefficients) || !identical(names(m$coefficients),expected) ||
      any(!is.finite(m$coefficients)) || !is.numeric(m$lambda) || length(m$lambda)!=1 ||
      !is.finite(m$lambda) || m$lambda<=0) stop("Invalid model parameters.")
  # Strip diagnostics attached to the coefficient vector and all other fields.
  b <- setNames(as.numeric(m$coefficients),names(m$coefficients))
  list(kind=m$kind,levels=m$levels,coefficients=b,lambda=m$lambda,
    outcome="positive_among_evaluable",disclosure_approval="pending")
}
react_prepare_dependency_release_review <- function(input,output="dependency-release-review-INTERNAL-v1") {
  if (!requireNamespace("digest",quietly=TRUE)) stop("digest is required (already used by reactextract).")
  if(file.exists(output)) stop("Use a fresh output folder; nothing is overwritten.")
  source <- file.path(input,"parameters-AND-COUNTS-NOT-APPROVED.rds")
  models <- readRDS(source)
  rounds <- c(sprintf("react1.r%02d",1:19),sprintf("react2.r%02d",1:6))
  if(!is.list(models) || anyDuplicated(names(models)) || !setequal(names(models),rounds)) stop("All 25 unique rounds are required.")
  selected <- list(); inventory <- list()
  allowed <- c("age_band","gender","region","ethnicity_broad","imd_quintile","participation","registration",
    "household_size_band","economic_activity_broad","key_worker_care_role","covid_history",
    "vaccination_status","vaccine_dose","confirmed_contact","outcome_availability")
  add <- function(round,component,id,note) inventory[[length(inventory)+1L]] <<-
    data.frame(round=round,component=component,id=id,decision="pending",note=note)
  for(r in rounds) {
    z <- models[[r]]
    if(!identical(z$round_id,r) || !identical(z$status,"fitted_pending_review")) stop("Round not ready for review: ",r)
    model <- ds_model(z$fit)
    if(any(!names(model$levels) %in% allowed)) stop("Unrecognised predictor; explicit review required.")
    counts <- list()
    if(any(!names(z$conditional_tables) %in% allowed)) stop("Unrecognised context table.")
    for (field in names(z$conditional_tables)) {
      spec <- z$conditional_tables[[field]]
      if(!identical(spec$target,field) || any(!spec$parents[[1]] %in% allowed)) stop("Unexpected context table structure.")
      counts[[field]] <- ds_protect_table(spec)
      add(r,"context_counts",field,"Review with other selected tables and prior releases; no fallback marginals included")
    }
    symptoms <- list()
    for(id in names(z$symptom_tables)) {
      if(!grepl("^occ_[a-f0-9]+$",id)) stop("Invalid symptom occurrence ID.")
      spec <- z$symptom_tables[[id]]
      if(!identical(spec$target,"symptom") || any(!spec$parents[[1]] %in% c("outcome","age_band","participation"))) stop("Invalid symptom structure.")
      symptoms[[id]] <- ds_protect_table(spec)
      add(r,"symptom_counts",id,"Exact occurrence, not joint symptom patterns")
    }
    ct <- list()
    for(field in names(z$ct_tables)) {
      if(!field %in% c("CT_VALUE1","CT_VALUE2","NGENE_CTVALUE","EGENE_CTVALUE")) stop("Unexpected Ct field.")
      spec <- z$ct_tables[[field]]
      if(!identical(spec$target,"ct_state") || !identical(spec$parents[[1]],"outcome")) stop("Invalid Ct structure.")
      ct[[field]] <- ds_protect_table(spec)
      add(r,"ct_counts",field,"Zero separate; joint gene/result consistency still requires integration")
    }
    add(r,"model_parameters","outcome","Separate coefficient disclosure assessment required; shrinkage is not a privacy guarantee")
    selected[[r]] <- list(model=model,context_counts=counts,symptom_counts=symptoms,ct_counts=ct)
  }
  dir.create(output,recursive=TRUE)
  payload <- list(schema="dependency-release-selection-v1",status="INTERNAL_PENDING_DISCLOSURE_REVIEW",
    source_sha256=ds_hash(source),rounds=selected,
    scientific_review=list(low_prevalence_imprecision="accepted_for_code_development_with_limitations",
      supported_group_mismatches="explicit_round_level_acceptance_pending",
      round3_baseline_warning="retain_as_limitation_not_evidence_of_relationship_recovery"),
    limitations=c("not_generator_ready","cross_table_and_prior_release_disclosure_review_pending",
      "no_automatic_approval","observed_support_labels_require_review","suppressed_state_fallback_not_yet_implemented"))
  saveRDS(payload,file.path(output,"SELECTION-NOT-APPROVED.rds"))
  write.csv(do.call(rbind,inventory),file.path(output,"REVIEW_DECISIONS.csv"),row.names=FALSE)
  if(file.exists(file.path(input,"migration.rds"))) {
    # Retain hash only, not potentially identifying source paths/configuration.
    writeLines(ds_hash(file.path(input,"migration.rds")),file.path(output,"migration-reference.sha256"))
  }
  writeLines(c("INTERNAL ONLY — NOT AN AUTHORISED EXPORT",
    "Keep this folder inside the enclave. Tables have preliminary protections only.",
    "Model parameters, observed support and linked tables require disclosure review.",
    "Changing REVIEW_DECISIONS.csv does not make the payload approved.",
    "Do not upload to OneDrive or return to the chat without explicit file-level disclosure approval."),
    file.path(output,"DO_NOT_EXPORT.txt"))
  files <- list.files(output)
  write.csv(data.frame(file=files,sha256=vapply(file.path(output,files),ds_hash,character(1))),
    file.path(output,"selection-checksums.csv"),row.names=FALSE)
  message("Internal selection prepared. Keep the entire folder inside the enclave; consult the kit README.")
  invisible(normalizePath(output))
}
