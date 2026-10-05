kit_path <- Sys.getenv("REACT_PARTICIPATION_PROFILE_DIR", unset = system.file("enclave", "participation-v6-profile", package = "reactextract"))
source(file.path(kit_path, "profile.R"), local = TRUE)
source(file.path(kit_path, "followup-policy.R"), local = TRUE)

testthat::test_that("age thresholds and missing-code comparisons are explicit", {
  testthat::expect_identical(pc6_age(c(4,5,11,12,13,15,16,17,18,54,55,120,121,-77,NA,17.5)),
    c("under_5","5_11","5_11","12","13_15","13_15","16_17","16_17","18_54","18_54","55_plus","55_plus",rep("missing_or_invalid",3),"16_17"))
  for (op in c("not_in", "not_missing", "gte", "selected_any")) {
    testthat::expect_false(any(pc6_condition(c(-77,-91,-92,-66,-99,-555,NA), op, "[1]")))
  }
  testthat::expect_identical(pc6_condition(c(1,2,-77,NA), "not_in", "[1]"), c(FALSE,TRUE,FALSE,FALSE))
})
testthat::test_that("participation never overwrites partial or contradictory answers", {
  z <- pc6_stage("react1.r02", c("coded:-77","coded:-77","code:0","code:1","coded:-555"),
    list(), c(FALSE,TRUE,TRUE,FALSE,FALSE))
  testthat::expect_identical(z$detail, c("shared_nonresponse","discordant_flag_and_answer","recorded_breakoff","recorded_complete","source_555"))
  testthat::expect_false(any(pc6_stage("react1.r02", "coded:-77", list(), FALSE, FALSE)$state == "shared_nonresponse"))
  testthat::expect_identical(pc6_stage("react1.r02", "code:0", list(), FALSE)$state, "undetermined")
  a <- c("coded:-77", "coded:-77", "code:1")
  b <- c("coded:-77", "code:1", "coded:-77")
  z <- pc6_stage("react2.r01", NULL, list(a,b), c(FALSE,TRUE,TRUE))
  testthat::expect_identical(z$state, c("shared_nonresponse","response_evidenced","response_evidenced"))
  testthat::expect_identical(pc6_stage("react2.r01", NULL, list(a), rep(FALSE,3))$state, rep("undetermined",3))
})
testthat::test_that("the shared contract preserves independent fields and exceptions", {
  c6 <- pc6_contract(file.path(kit_path,"contract"))
  p <- c6$occurrence_participation
  testthat::expect_equal(nrow(p),15093L)
  testthat::expect_false(any(p$governed[p$variable %in% c("U_AGE","U_GENDER","LSOA","IMD_DECILE","RESULT","FINALRESULT")] == "TRUE"))
  testthat::expect_true(all(p$minimum_age[grepl("^react1[.]",p$round_id) & grepl("^(SMOKE|VAPNOW)",p$variable)] == "18"))
  testthat::expect_equal(sum(p$age_condition == "D01_asked_all_registration_NHS_age_band"),4L)
  transition <- p$round_id == "react1.r09" & grepl("^(SMOKE|VAPNOW)",p$variable)
  testthat::expect_true(all(p$governed[transition] == "FALSE"))
  testthat::expect_true(any(c6$routing_capture$capture_status == "incomplete_context_do_not_claim_eligible"))
})
testthat::test_that("public projections preserve literal codes and exclude text content", {
  d <- reactextract::react_dictionary()
  d$participation_age_bins <- pc6_contract(file.path(kit_path,"contract"))$age_context
  s <- pc6_ns(".approved_profile_specs")(d)
  domain <- function(field, round="react1.r02") {
    o <- d$occurrences[d$occurrences$round_id == round & d$occurrences$variable == field,,drop=FALSE]
    pc6_domain(o,s[match(o$occurrence_id,s$occurrence_id),,drop=FALSE],d)
  }
  age <- domain("U_AGE")
  testthat::expect_identical(age$states[pc6_project_file(c(12,16,18,-77,NA,17.5),age)],
    c("bin:12","bin:16_17","bin:18_24","coded:-77","database_missing","bin:16_17"))
  testthat::expect_false(grepl("FLOOR", pc6_sql(age), fixed=TRUE))
  smoke <- domain("SMOKENOW")
  testthat::expect_identical(smoke$states[pc6_project_file(c(1,-77,-91,999,NA),smoke)],
    c("code:1","coded:-77","coded:-91","outside_public_support","database_missing"))
  text <- smoke; text$kind <- "free_text"; text$type <- "VARCHAR2"
  text$states <- c("database_missing",paste0("coded:",text$missing),"text_present","empty_text","outside_public_support")
  testthat::expect_identical(text$states[pc6_project_file(c("private respondent text","-77","NA",NA,""),text)],
    c("text_present","coded:-77","text_present","database_missing","empty_text"))
  testthat::expect_match(pc6_sql(text), "LENGTH", fixed=TRUE)
  # Non-numeric public code in a NUMBER field must not make Oracle cast text.
  impossible <- domain("ADULTAGE_NA")
  testthat::expect_match(pc6_sql(impossible), "CASE", fixed=TRUE)
  date <- domain("DATESYMPTOMSTART", "react1.r01")
  testthat::expect_match(pc6_sql(date), "REGEXP_LIKE", fixed=TRUE)
  states <- date$states[pc6_project_file(c("2020-02-29","2020-02-31","unknown text","-77",NA),date)]
  testthat::expect_match(states[1], "^bin:")
  testthat::expect_identical(states[-1], c("outside_public_support","outside_public_support","coded:-77","database_missing"))
})
testthat::test_that("retries align by key and key errors are fatal", {
  raw <- data.frame(U_PASSCODE=c("b","a"),F=c(2,1))
  d <- list(field="F",type="NUMBER",kind="categorical",missing="-77",states=c("database_missing","coded:-77","code:1","code:2","outside_public_support"),values=c("","","1","2",""))
  reg <- data.frame(observation_key="U_PASSCODE",object_name="TEST_V")
  src <- list(kind="files")
  testthat::expect_identical(pc6_fetch(src,reg,c("a","b"),list(d),raw)$data$F,c(3L,4L))
  raw$U_PASSCODE <- c("a","a")
  testthat::expect_error(pc6_fetch(src,reg,c("a","b"),list(d),raw),"keys changed")
  # Oracle fixture returns reordered projected states, not unaligned columns.
  src <- list(kind="oracle",connection=NULL,query_fn=function(con,sql) data.frame(U_PASSCODE=c("b","a"),F=c(4,3)))
  testthat::expect_identical(pc6_fetch(src,reg,c("a","b"),list(d))$data$F,c(3L,4L))
})
testthat::test_that("protected exports suppress linked complements and exclude internal labels", {
  counts <- pc6_counts("react1.r02","test","occ_test","context",c(rep("a",5),rep("b",20),rep("c",25)),c("a","b","c"),rep("all",50),"all")
  x <- list(counts=counts,domains=data.frame(state=c("a","b","c")),issues=data.frame(),timings=data.frame(private="internal"),
    manifest=data.frame(key=c("status","schema","source_label","elapsed_seconds"),value=c(pc6_internal,"participation-conditional-capture-v1","private_path",1)))
  y <- pc6_prepare_export(x, progress=FALSE)
  testthat::expect_true(y$counts$suppressed[1])
  testthat::expect_gte(sum(y$counts$suppressed),2L)
  testthat::expect_true(all(is.na(y$counts$count)==y$counts$suppressed))
  testthat::expect_false(any(y$manifest$key %in% c("source_label","elapsed_seconds")))
  testthat::expect_null(y$timings)
  testthat::expect_error(pc6_prepare_export(y),"internal aggregate")
  y$counts$count[1] <- 5
  testthat::expect_error(pc6_write(y,tempfile()),"Invalid protected")
})

testthat::test_that("a whole-round fictional capture resumes without querying or saving rows", {
  d <- reactextract::react_dictionary()
  round <- "react1.r02"
  o <- d$occurrences[d$occurrences$round_id==round,,drop=FALSE]
  n <- 30L
  fields <- setNames(lapply(o$data_type,function(type) {
    if (grepl("DATE",type)) as.Date(rep(NA_character_,n)) else if (grepl("CHAR",type)) rep(NA_character_,n) else rep(NA_real_,n)
  }),o$variable)
  fields$U_PASSCODE <- paste0("FICTIONAL_",seq_len(n))
  fields$U_AGE <- rep(c(12,16.5,18),10)
  fields$SFREPORTFIG <- rep(c(-77,1,-77),each=10)
  fields$FEELUN <- rep(c(-77,1,2),each=10)
  fields$INDCONFSF <- rep(c(-77,1,NA),each=10)
  fields$SMOKENOW <- rep(c(-77,1,NA),each=10)
  src <- reactextract::react_files(list(react1.r02=as.data.frame(fields,check.names=FALSE)))
  tmp <- tempfile("capture-checkpoint-")
  result <- pc6_run(src,contract=file.path(kit_path,"contract"),rounds=round,
    checkpoint=tmp,source_label="fictional",progress=FALSE)
  testthat::expect_equal(nrow(result$issues),0L)
  stage <- aggregate(count~state,subset(result$counts,table=="participation_by_age"),sum)
  testthat::expect_equal(stage$count[match(c("shared_nonresponse","recorded_complete","discordant_flag_and_answer"),stage$state)],rep(10,3))
  save <- readRDS(file.path(tmp,paste0(round,".rds")))
  testthat::expect_setequal(names(save),c("round_id","contract_hash","counts","domains","issues","timing"))
  testthat::expect_false(grepl("FICTIONAL_",paste(capture.output(str(save)),collapse="\n"),fixed=TRUE))
  # Remove the input: a resume must use the completed checkpoint without reading it.
  src$rounds <- list()
  resumed <- pc6_run(src,contract=file.path(kit_path,"contract"),rounds=round,
    checkpoint=tmp,source_label="fictional",progress=FALSE,resume=TRUE)
  testthat::expect_identical(result$counts,resumed$counts)
  testthat::expect_error(pc6_run(src,contract=file.path(kit_path,"contract"),rounds=round,
    checkpoint=tmp,source_label="changed-snapshot",progress=FALSE,resume=TRUE),"identical snapshot")
  testthat::expect_error(pc6_write(result,tempfile()),"disclosure controls")
})
