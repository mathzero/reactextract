# Standalone code-only kit tests. All records are fictional. No Oracle access.
root <- normalizePath(".")
kit <- file.path(root, "inst/enclave/dependency-model-v1")
setwd(kit); source("00_load.R")
dictionary <- reactextract::react_dictionary(); contract <- dm_load_contract()
passed <- 0L
check <- function(value, label) {stopifnot(isTRUE(value)); passed <<- passed+1L; cat("PASS:", label, "\n")}
fails <- function(code) inherits(tryCatch({force(code); NULL}, error = identity), "error")

fixture <- function(round, n = 400L) {
  oo <- dictionary$occurrences[dictionary$occurrences$round_id == round, , drop = FALSE]
  d <- as.data.frame(setNames(lapply(oo$data_type, function(type) {
    if (grepl("DATE|TIMESTAMP", type)) as.Date(rep(NA_character_, n)) else if (grepl("CHAR", type)) rep(NA_character_, n) else rep(NA_real_, n)
  }), oo$variable), check.names = FALSE)
  set <- function(field, value) if (field %in% names(d)) d[[field]] <<- rep(value, length.out = n)
  key <- dictionary$source_registry$observation_key[dictionary$source_registry$round_id == round]
  d[[key]] <- paste0("FICTIONAL-KEY-", seq_len(n))
  set("U_AGE", if (startsWith(round, "react1")) c(12,17,25,40,58,72) else c(20,30,45,58,72,85))
  for (field in c("U_GENDER", "ETHNIC", "REGION")) set(field, c(1,2))
  set("IMD_DECILE", 1:10); set("COVIDA", c(4,4,1,2)); set("EMPL", c(1,3,6,7))
  for (field in c("NADULTS", "NADULTS1")) set(field, c(1,2,3))
  for (field in c("NCHILD", "NCHILD1")) set(field, c(0,1,2))
  for (field in c("INDCONF", "INDCONFSF", "ABATTEMPT", "ABCOMP", "SWAATTEMPT", "SWASTATUS", "SFREPORTFIG")) set(field, 1)
  set("REGREPORTFIG", 2)
  for (field in c("COVIDCON", "COVIDCON_1", "COVIDCON_2", "COVIDCON_3")) set(field, c(1,0,0,0))
  for (field in c("VACCINE3", "VACCINE3SYM")) set(field, c(1,1,1,2))
  for (field in c("VACCDOSE", "VACCDOSESYM")) set(field, c(1,2,2,-91))
  for (field in grep("^VACCINETYPE", names(d), value = TRUE)) {
    if (grepl("_1$", field)) set(field, c(1,0,1,-91))
    else if (grepl("_2$", field)) set(field, c(0,1,0,-91))
    else if (grepl("_[0-9]+$", field)) set(field, c(0,0,0,-91))
    else if (!grepl("OTHER", field)) set(field, c(1,2,1,-91))
  }
  set("VACCINEREG", c(1,1,1,2))
  for (field in intersect(c("SWABDATE_NEW", "SWABDATE", "ABDATE"), names(d))) set(field, as.Date("2021-07-01"))
  for (field in grep("^VACCINE(FIRST|SECOND)(SYM)?$", names(d), value = TRUE)) {
    type <- oo$data_type[match(field, oo$variable)]
    value <- if (type == "DATE") as.Date("2021-05-01") else "01/05/2021"
    set(field, value)
  }
  outcome <- if (startsWith(round, "react1")) "react1_pcr_positive" else "react2_igg_positive"
  # Balanced-ish randomized fixture outcomes permit all pilot fitting paths.
  state <- dm_with_seed(dm_seed(21, round, "fixture"), ifelse(runif(n)<.35, "positive", "negative"))
  d <- dm_ns(".set_dependency_outcome")(d, state, round, outcome, NULL, dictionary)
  for (field in grep("^SYMPTANY", names(d), value = TRUE)) set(field, ifelse(state == "positive", 1, 0))
  # Keep known stage non-response aligned, preserving independent lab/age.
  p <- contract$participation$occurrence_participation
  governed <- p$variable[p$round_id == round & p$stage == "individual" & p$governed == "TRUE"]
  for (field in intersect(governed, names(d))) {
    if (inherits(d[[field]], "Date")) d[[field]][1:4] <- NA else d[[field]][1:4] <- -77
  }
  set("SFREPORTFIG", c(rep(-77,4),rep(1,n-4)))
  d
}

check(all(is.na(dm_date(c("31/02/2021", "2021-02-30", "NA", "-77", "07/2021")))), "invalid or partial dates are not guessed")
check(identical(dm_date(c("01/05/2021", "2021-05-01")), as.Date(c("2021-05-01","2021-05-01"))), "explicit date formats agree")
check(fails(pc6_align(data.frame(K=c("a","a")), "K", c("a","b"))), "duplicate observation keys fail")
check(fails(pc6_align(data.frame(K=c("a","c")), "K", c("a","b"))), "changed observation keys fail")

data <- data.frame(age_band = rep(c("young","old"),200), vaccination_status = rep(c("yes","yes","no","no"),100))
data$outcome <- dm_with_seed(1, ifelse(runif(400) < ifelse(data$age_band=="old", .6,.15), "positive","negative"))
fit <- dm_fit_outcome(data,c("age_band","vaccination_status"),123)
check(fit$status == "fitted_pending_review", "ridge model fits without extra dependencies")
check(all(is.finite(dm_predict(fit$model, data))), "numeric-only model predicts")
check(identical(fit,dm_fit_outcome(data,c("age_band","vaccination_status"),123)), "fitting is deterministic")
data$outcome <- "negative"
check(dm_fit_outcome(data,"age_band",123)$status == "insufficient_outcome_support", "single-class round is held")

rounds <- dictionary$rounds$round_id
files <- setNames(lapply(rounds, fixture), rounds)
source_data <- reactextract::react_files(files)
scratch <- tempfile("dependency-kit-tests-");dir.create(scratch)
cat("Synthetic-only test workspace:",scratch,"\n")

# Test field semantics and the complete file-source scan on both pilot rounds.
for (r in c("react1.r13","react2.r06")) {
  c <- dm_capture(source_data,r,dictionary,contract,30L,function(...)NULL)
  z <- dm_states(c,dictionary,contract,r)
  check(nrow(z$data)==400L,paste(r,"capture retains every fictional observation"))
  check(!any(c("U_PASSCODE","SUBJECT_ID","DOB") %in% names(z$data)),paste(r,"no identifiers enter modelling states"))
  check(all(c("vaccine_dose","vaccine_product","vaccine_elapsed") %in% names(z$data)),paste(r,"vaccination components captured"))
}
o <- dictionary$occurrences[dictionary$occurrences$round_id == "react1.r13",]
dd <- files[["react1.r13"]]
for (f in grep("^VACCINETYPESYM_", names(dd),value=TRUE)) dd[[f]] <- 0
dd$VACCINETYPESYM_5 <- 1
vv <- dm_vaccine(dd,o,dictionary,"react1.r13")
check(all(vv$product == "other"),"product suffix 5 uses its stage-specific label, not Janssen")

# Oracle adapter simulation exercises the actual generated CASE/SELECT query
# path and intentionally reverses returned rows. It does not claim an Oracle
# server syntax/driver acceptance test, which still happens inside the enclave.
r <- "react1.r13"
pp <- contract$participation$occurrence_participation
ss <- dm_ns(".approved_profile_specs")(dictionary)
ii <- which(pp$round_id == r); oo <- dictionary$occurrences[match(pp$occurrence_id[ii],dictionary$occurrences$occurrence_id),]
ddict <- dictionary;ddict$participation_age_bins <- contract$participation$age_context
domains <- setNames(lapply(seq_along(ii),function(j) pc6_domain(oo[j,,drop=FALSE],ss[match(oo$occurrence_id[j],ss$occurrence_id),,drop=FALSE],ddict)),oo$variable)
key <- dictionary$source_registry$observation_key[dictionary$source_registry$round_id==r]
query <- function(connection,sql) {
  raw <- files[[r]]
  if(grepl("CASE",sql,fixed=TRUE)) {
    aliases <- regmatches(sql,gregexpr('AS "[A-Z0-9_]+"',sql))[[1]]
    fields <- substring(aliases,5,nchar(aliases)-1)
    z <- raw[key]
    for(field in fields) z[[field]] <- pc6_project_file(raw[[field]],domains[[field]])
  } else {
    select <- strsplit(sql," FROM ",fixed=TRUE)[[1]][1]
    names <- regmatches(select,gregexpr('"[A-Z0-9_]+"',select))[[1]]
    fields <- substring(names,2,nchar(names)-1)
    z <- raw[unique(fields)]
  }
  z[rev(seq_len(nrow(z))),,drop=FALSE]
}
oracle <- dm_ns(".new_oracle_source")(NULL,dictionary$source_registry,30L,query)
oc <- dm_capture(oracle,r,dictionary,contract,30L,function(...)NULL)
fc <- dm_capture(source_data,r,dictionary,contract,30L,function(...)NULL)
check(identical(dm_states(oc,dictionary,contract,r)$data,dm_states(fc,dictionary,contract,r)$data),"Oracle and file modelling states agree despite reversed query rows")
badquery <- function(connection,sql) {z<-query(connection,sql);z[[key]][1]<-z[[key]][2];z}
bad_oracle <- dm_ns(".new_oracle_source")(NULL,dictionary$source_registry,30L,badquery)
check(fails(dm_capture(bad_oracle,r,dictionary,contract,30L,function(...)NULL)),"Oracle duplicate keys cannot reach a model")

status <- dm_run(source_data,"fictional-test-snapshot",file.path(scratch,"all"),synthetic_n=300L,progress=FALSE)
check(nrow(status)==25L,"all 25 rounds get a status")
print(status)
check(all(status$status == "fitted_pending_review"),"automatic pilots and all-round fitting complete")
hashes <- vapply(file.path(scratch,"all",paste0(rounds,".rds")),pc6_hash,character(1))
again <- dm_run(source_data,"fictional-test-snapshot",file.path(scratch,"all"),synthetic_n=300L,progress=FALSE)
check(identical(status,again),"resuming reuses completed rounds")
check(identical(hashes,vapply(file.path(scratch,"all",paste0(rounds,".rds")),pc6_hash,character(1))),"resume leaves checkpoint bytes unchanged")
check(fails(dm_run(source_data,"changed-snapshot",file.path(scratch,"all"),synthetic_n=300L,progress=FALSE)),"changed snapshot cannot mix checkpoints")
check(fails(dm_run(source_data,"fictional-test-snapshot",file.path(scratch,"all"),seed=2L,synthetic_n=300L,progress=FALSE)),"changed seed cannot mix checkpoints")
dm_prepare_review(file.path(scratch,"all"),file.path(scratch,"review"))
check(file.exists(file.path(scratch,"review","DO_NOT_EXPORT.md")),"combined review candidate is explicitly internal")
objects <- readRDS(file.path(scratch,"review","parameters-AND-COUNTS-NOT-APPROVED.rds"))
check(!any(grepl("FICTIONAL-KEY-",unlist(objects,use.names=FALSE),fixed=TRUE)),"no record identifiers saved in candidate")

# Pilot failure stops automatically, without treating other rounds as complete.
bad <- files;bad[["react1.r13"]]$RESULT <- "NEVER_EXPORT_THIS_UNKNOWN_RAW_VALUE"
stop_status <- dm_run(reactextract::react_files(bad),"bad-fixture",file.path(scratch,"bad"),synthetic_n=100L,progress=FALSE)
check(sum(stop_status$status == "not_run")==24L,"pilot failure holds the rest of the batch")
check(!grepl("NEVER_EXPORT",paste(readLines(file.path(scratch,"bad","RUN_STATUS.md")),collapse=" "),fixed=TRUE),"operational report contains no unexpected raw values")

later <- files;later[["react1.r02"]]$RESULT <- "NEVER_EXPORT_LATER_BAD_VALUE"
scope <- c("react1.r13","react2.r06","react1.r02","react1.r03")
later_status <- dm_run(reactextract::react_files(later),"later-failure-fixture",file.path(scratch,"later"),rounds=scope,synthetic_n=100L,progress=FALSE)
check(later_status$status[later_status$round=="react1.r03"]=="fitted_pending_review","non-pilot failure does not stop unrelated rounds")
check(later_status$status[later_status$round=="react1.r02"]=="round_failed_see_internal_report","non-pilot failure is explicit")

large <- data.frame(age_band=rep(c("young","middle","old"),length.out=20000L),vaccination_status=rep(c("yes","no"),10000L))
large$outcome <- dm_with_seed(9,ifelse(runif(20000)<ifelse(large$vaccination_status=="yes",.03,.10),"positive","negative"))
started <- proc.time()[[3L]]
large_fit <- dm_fit_outcome(large,c("age_band","vaccination_status"),91)
check(large_fit$status=="fitted_pending_review","20,000-record synthetic fitting check")
cat("Large fitting check elapsed seconds:",round(proc.time()[[3L]]-started,2),"\n")
# Force the final refit to fail and confirm diagnostic retention.
original_ridge <- dm_ridge
calls <- 0L
dm_ridge <- function(...) {
  calls <<- calls+1L
  if (calls == 5L) stop("fictional refit failure")
  original_ridge(...)
}
failed_fit <- dm_fit_outcome(large,c("age_band","vaccination_status"),91)
dm_ridge <- original_ridge
check(failed_fit$status == "refit_failed" && length(failed_fit$diagnostics) == 5L &&
  !is.null(failed_fit$validation), "failed refit retains validation and diagnostics")
check(identical(failed_fit$diagnostics[[5]]$error,"fictional refit failure"),"underlying error is preserved internally")

# Simulate the original v1 two-pilot checkpoint layout without changing inputs.
old <- file.path(scratch,"old-v1");dir.create(old)
cfg <- readRDS(file.path(scratch,"all","configuration.rds"))
cfg$code_sha256 <- "76be80cc83b5dfe562e6386132fe0d7bdea55de8416a1a6345653829b5dccbc0"
saveRDS(cfg,file.path(old,"configuration.rds"))
file.copy(file.path(scratch,"all","react1.r13.rds"),old)
bad_checkpoint <- readRDS(file.path(scratch,"all","react2.r06.rds"))
bad_checkpoint$status <- "refit_failed";bad_checkpoint$fit <- failed_fit
saveRDS(bad_checkpoint,file.path(old,"react2.r06.rds"))
old_hash <- pc6_hash(file.path(old,"react2.r06.rds"))
new <- file.path(scratch,"migrated")
migrated <- dm_run(source_data,"fictional-test-snapshot",new,synthetic_n=300L,
  progress=FALSE,previous_output=old)
check(all(migrated$status == "fitted_pending_review"),"migration retries failed pilot and finishes all rounds")
check(identical(pc6_hash(file.path(old,"react1.r13.rds")),pc6_hash(file.path(new,"react1.r13.rds"))),"successful original pilot retained byte-for-byte")
check(identical(old_hash,pc6_hash(file.path(old,"react2.r06.rds"))),"failed original checkpoint remains untouched")
check(file.exists(file.path(new,"migration.rds")),"mixed solver provenance recorded")
check(fails(dm_run(source_data,"different-snapshot",file.path(scratch,"reject"),
  synthetic_n=300L,progress=FALSE,previous_output=old)),"migration rejects mismatched snapshot")
saveRDS(bad_checkpoint,file.path(new,"react2.r06.rds"))
retry_status <- dm_run(source_data,"fictional-test-snapshot",new,synthetic_n=300L,
  progress=FALSE,retry_failed=TRUE)
check(all(retry_status$status == "fitted_pending_review"),"explicit retry replaces failed fit without rerunning successes")
check(length(list.files(new,pattern="previous[.]rds$"))==1L,"explicit retry archives failed checkpoint")
cat("Completed",passed,"checks. Detailed synthetic-only outputs:",scratch,"\n")
