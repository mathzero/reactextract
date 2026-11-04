# The fixture is an invented candidate in the inventory kit's layout; see
# fixtures/make-clinical-candidate.R
fixture <- test_path("fixtures", "clinical-candidate")
profile <- react_clinical_profile(fixture, supplement = "")
source <- react_synthetic(n_per_round = c(REACT1_R08 = 400L, REACT2_S5_R05 = 300L), seed = 3L)
gelc <- react_clinical(source, profile = profile)
reg <- gelc$registration_data
t0 <- gelc$clinic_data$clinical_tests$tests_t0
t1 <- gelc$clinic_data$clinical_tests$tests_t1
survey <- gelc$health_survey$survey_t0

leaf <- function(x, path) Reduce(function(node, name) node[[name]], strsplit(path, "/")[[1]], x)

test_that("the profile reads the kit's files, including empty ones", {
  expect_s3_class(profile, "react_clinical_profile")
  expect_identical(profile$status, "local_candidate")
  expect_identical(unname(profile$anchors[["lc_group"]]), "lc_status")
  expect_identical(nrow(profile$availability_pairs), 0L)
  expect_identical(nrow(profile$visit_pairs), 2L)
  expect_identical(nrow(profile$availability), 0L)
  expect_true(is.numeric(profile$numeric$p50))
  expect_true(is.logical(profile$tables$has_persons))
  # shapes and levels stay text even when they look like numbers
  expect_type(profile$id_shapes$level, "character")
  expect_true("999999999" %in% profile$id_shapes$level)
})

test_that("a missing profile path says how to set one", {
  expect_error(react_clinical_profile(""), "REACTEXTRACT_CLINICAL_PROFILE")
  withr::local_envvar(REACTEXTRACT_CLINICAL_PROFILE = "")
  expect_error(react_clinical(source), "No approved Clinical Study profile")
})

test_that("files changed since the candidate was written are refused", {
  copy <- file.path(withr::local_tempdir(), "candidate")
  dir.create(copy)
  file.copy(list.files(fixture, full.names = TRUE), copy, recursive = TRUE)
  utils::write.csv(data.frame(file = "inventory/03_tables.csv", md5 = "0"),
                   file.path(copy, "manifest.csv"), row.names = FALSE)
  expect_error(react_clinical_profile(copy), "changed since the candidate was written")
  md5 <- unname(tools::md5sum(file.path(copy, "inventory/03_tables.csv")))
  utils::write.csv(data.frame(file = "inventory/03_tables.csv", md5 = md5),
                   file.path(copy, "manifest.csv"), row.names = FALSE)
  expect_identical(nrow(react_clinical_profile(copy)$manifest), 1L)
})

test_that("a supplement replaces the rows of the sources it lists", {
  supplement <- file.path(withr::local_tempdir(), "supplement")
  dir.create(file.path(supplement, "inventory"), recursive = TRUE)
  dir.create(file.path(supplement, "profile"))
  for (file in c("inventory/00_sources.csv", "inventory/02_structure.csv", "inventory/03_tables.csv",
                 "inventory/04_columns.csv", "profile/10_numeric.csv")) {
    file.copy(file.path(fixture, file), file.path(supplement, file))
  }
  numeric <- utils::read.csv(file.path(supplement, "profile/10_numeric.csv"), check.names = FALSE)
  changed <- numeric$column == "sys_bp" & numeric$stratum_var == "all" & grepl("t0$", numeric$table)
  numeric$p50[changed] <- 999
  utils::write.csv(numeric, file.path(supplement, "profile/10_numeric.csv"), row.names = FALSE, na = "")
  merged <- react_clinical_profile(fixture, supplement = supplement)
  rows <- merged$numeric[merged$numeric$column == "sys_bp" & merged$numeric$stratum_var == "all", ]
  expect_identical(sort(rows$p50), c(129, 999))
  expect_identical(nrow(merged$structure), nrow(profile$structure))
  # files the supplement lacks (or left empty) keep the first run's rows
  expect_identical(nrow(merged$categorical), nrow(profile$categorical))
})

test_that("tables have the inventory's paths, column names, order and classes", {
  expect_named(gelc, c("registration_data", "clinic_data", "health_survey", "react_records", "infection_log",
                       "date_rules", "test_log", "outsiders"))
  wrangled <- profile$structure[profile$structure$source == "wrangled" &
                                  grepl("data.frame", profile$structure$class), ]
  for (i in seq_len(nrow(wrangled))) {
    path <- wrangled$path[[i]]
    data <- leaf(gelc, path)
    expected <- profile$columns[profile$columns$table == path, ]
    expected <- expected[order(expected$position), ]
    expect_identical(names(data), expected$column, info = path)
    classes <- vapply(data, function(x) class(x)[[1]], character(1))
    expect_identical(unname(classes), expected$class, info = path)
  }
  expect_identical(class(reg), c("tbl_df", "tbl", "data.frame"))
  expect_identical(class(t1), "data.frame")
  # a data.table can't be made in base R, so it becomes a data.frame
  expect_identical(class(gelc$react_records), "data.frame")
  expect_identical(attr(t0$sample_time, "tzone"), "UTC")
})

test_that("tables have the profile's numbers of people and rows", {
  expect_identical(nrow(reg), 400L)
  expect_false(anyDuplicated(reg$subject_id) > 0)
  expect_identical(nrow(t0), 300L)
  expect_identical(nrow(t1), 60L)
  expect_identical(nrow(survey), 280L)
  expect_identical(nrow(gelc$health_survey$dictionary), 50L)
  records <- gelc$react_records
  expect_identical(length(unique(records$subject_id)), 390L)
  expect_gt(nrow(records), 400L)
})

test_that("tables of a visit nest, and follow-up people are in the main cohort", {
  expect_true(all(t0$subject_id %in% reg$subject_id))
  expect_true(all(t1$subject_id %in% t0$subject_id))
  main_in_survey <- survey$subject_id %in% t0$subject_id
  expect_identical(sum(main_in_survey), 270L)
  expect_true(all(gelc$react_records$subject_id %in% reg$subject_id))
})

test_that("subject_id is a nine-digit number made from the synthetic SUBJECT_ID", {
  expect_true(all(reg$subject_id >= 1e8 & reg$subject_id < 1e9))
  expect_identical(.clinical_subject_id("SYN-SUBJECT-REACT1-R08-0000012"), 1e8 + 8e6 + 12)
  expect_identical(.clinical_subject_id(c("123456789", "42")), c(123456789, 42))
  expect_error(.clinical_subject_id("P0001"), "whole numbers or reactextract synthetic IDs")
  people <- react_extract(source, families = "people-households/demographics", progress = FALSE)
  expect_true(all(reg$subject_id %in% .clinical_subject_id(people$data$SUBJECT_ID)))
})

test_that("a person's anchors and identifiers agree across tables", {
  both <- merge(as.data.frame(reg)[c("subject_id", "sex_reg", "reg_code")],
                as.data.frame(t0)[c("subject_id", "reg_code")], by = "subject_id")
  expect_identical(both$reg_code.x, both$reg_code.y)
  sexes <- merge(as.data.frame(reg)[c("subject_id", "sex_reg")],
                 as.data.frame(survey)[c("subject_id", "sex_reg")], by = "subject_id")
  expect_identical(sexes$sex_reg.x, sexes$sex_reg.y)
  expect_true(all(grepl("^[A-Z]{2}[0-9]{4}$", reg$reg_code)))
  expect_false(anyDuplicated(reg$reg_code) > 0)
  expect_setequal(stats::na.omit(unique(reg$age_band)), c("18-34", "35-54", "55-74", "75+"))
})

test_that("suppressed levels are never generated and blank counts are zero", {
  expect_false("Rarely" %in% reg$fatigue)
  expect_setequal(stats::na.omit(unique(reg$fatigue)), c("Never", "Sometimes", "Often"))
  expect_false(93 %in% t0$o2_sat)
})

test_that("values follow the profile's distributions and anchors", {
  expect_lt(abs(stats::median(t0$sys_bp, na.rm = TRUE) - 128), 6)
  expect_true(all(t0$sys_bp >= 105 - 2, na.rm = TRUE))
  # sex shifts blood pressure; long COVID group shifts fatigue and symptom counts
  male <- reg$sex_reg[match(t0$subject_id, reg$subject_id)] == "Male"
  expect_gt(stats::median(t0$sys_bp[male], na.rm = TRUE), stats::median(t0$sys_bp[!male], na.rm = TRUE))
  persistent <- reg$lc_status %in% "Persistent"
  expect_gt(mean(reg$fatigue[persistent] %in% "Often"), mean(reg$fatigue[reg$lc_status %in% "None"] %in% "Often"))
  expect_gt(stats::median(reg$symptom_count[persistent], na.rm = TRUE),
            stats::median(reg$symptom_count[reg$lc_status %in% "None"], na.rm = TRUE))
  # missing codes, decimals and integer classes
  expect_true(-77 %in% t0$o2_sat)
  expect_true(all(abs(t0$body_weight * 10 - round(t0$body_weight * 10)) < 1e-8, na.rm = TRUE))
  expect_type(survey$sleep_hours, "integer")
  # missingness at the inventory's rate
  expect_gt(mean(is.na(reg$free_comment)), 0.6)
  expect_true(all(reg$free_comment %in% c(NA, "Synthetic free text")))
  expect_true(all(is.na(reg$dob)))
  expect_true(all(is.na(reg$blank_col)))
})

test_that("dates come from released months or month quantiles", {
  log <- gelc$infection_log
  expect_true(all(format(log$onset_date, "%Y-%m") %in% c(NA, "2020-04", "2020-06", "2020-10", "2020-12", "2021-01")))
  summary <- profile$date_summary
  dates <- .hes_with_seed(1L, .clinical_draw_dates(profile$date_months[0, ], summary, 200L))
  expect_true(all(format(dates, "%Y-%m") %in% c("2021-10", "2021-11", "2021-12")))
})

test_that("dates in different tables follow the timing intervals", {
  linked <- merge(as.data.frame(reg)[c("subject_id", "reg_date")], as.data.frame(t0)[c("subject_id", "visit_date")])
  real <- linked$reg_date > as.Date("1900-01-01")
  days <- as.numeric(linked$visit_date - linked$reg_date)[real]
  expect_true(all(days >= 4 & days <= 58))
  expect_lt(abs(stats::median(days) - 20), 6)
  visits <- merge(as.data.frame(t0)[c("subject_id", "visit_date")], as.data.frame(t1)[c("subject_id", "visit_date")],
                  by = "subject_id")
  later <- as.numeric(visits$visit_date.y - visits$visit_date.x)
  expect_true(all(later >= 157 & later <= 248))
  onset <- merge(as.data.frame(gelc$infection_log)[c("subject_id", "onset_date")],
                 as.data.frame(reg)[c("subject_id", "reg_date")])
  onset <- onset[!is.na(onset$onset_date) & onset$reg_date > as.Date("1900-01-01"), ]
  expect_true(all(onset$reg_date - onset$onset_date >= 0))
})

test_that("placeholder dates are kept, but don't start intervals", {
  expect_true(as.Date("1582-10-14") %in% reg$reg_date)
  expect_true(all(t0$visit_date > as.Date("1900-01-01")))
  expect_identical(.clinical_placeholder_date(c("1582-10", "1899-12")), as.Date(c("1582-10-14", "1899-12-01")))
})

test_that("follow-up values are correlated with the same person's baseline values", {
  visits <- merge(as.data.frame(t0)[c("subject_id", "sys_bp")], as.data.frame(t1)[c("subject_id", "sys_bp")],
                  by = "subject_id")
  expect_gt(stats::cor(visits$sys_bp.x, visits$sys_bp.y, method = "spearman", use = "complete.obs"), 0.4)
  expect_identical(names(.clinical_visit_pairs(profile, .clinical_tables(profile))), "clinic_data/clinical_tests/tests_t1")
})

test_that("population tables cover the whole synthetic population", {
  log <- gelc$infection_log
  population <- react_extract(source, families = "people-households/demographics", progress = FALSE)
  expect_identical(nrow(log), length(unique(population$data$SUBJECT_ID)))
  expect_true(all(reg$subject_id %in% log$subject_id))
  expect_true(.clinical_tables(profile)$population[.clinical_tables(profile)$table == "infection_log"])
})

test_that("branches have their own people and borrow the profile they mirror", {
  outsiders <- gelc$outsiders$registration_data
  expect_identical(nrow(outsiders), 10L)
  expect_true(all(outsiders$subject_id > 9e8))
  expect_false(any(outsiders$subject_id %in% reg$subject_id))
  expect_true(all(outsiders$fatigue %in% c(NA, "Never", "Sometimes", "Often")))
  expect_true(any(!is.na(outsiders$symptom_count)))
  # a table whose row count is suppressed has no rows
  expect_identical(nrow(gelc$outsiders$clinic_data$clinical_tests$tests_t1), 0L)
  expect_identical(names(gelc$outsiders$clinic_data$clinical_tests$tests_t1), c("subject_id", "visit_date", "sys_bp", "dia_bp"))
})

test_that("leaves that aren't tables, and many rows per person", {
  expect_identical(gelc$date_rules$min_date, as.Date("2020-04-01"))
  expect_identical(gelc$date_rules$max_date, as.Date("2021-09-30"))
  log <- gelc$test_log
  expect_identical(length(unique(log$subject_id)), 200L)
  expect_lt(abs(nrow(log) - 4000) / 4000, 0.15)
  expect_gt(max(table(log$subject_id)), 20L)
})

test_that("availability flags follow the released patterns", {
  cohort <- .hes_with_seed(3L, .clinical_cohort(source, NULL, profile, .clinical_tables(profile), NULL))
  people <- cohort$people
  flags <- attr(people, "flags")
  expect_setequal(colnames(flags), c("has_bloods_t0", "has_panel_t0", "has_genome", "has_rnaseq_t0",
                                     "has_bloods_t1", "has_panel_t1"))
  expect_true(all(flags[people$main, "has_bloods_t0"]))
  expect_false(any(flags[!people$main, ]))
  # follow-up flags only in the follow-up group; the suppressed pattern never appears
  expect_false(any(flags[!people$followup, c("has_bloods_t1", "has_panel_t1")]))
  expect_false(any(flags[, "has_genome"] & !flags[, "has_panel_t0"]))
  expect_identical(sum(people$followup), 60L)
})

test_that("columns of a table follow its rank correlations", {
  rho <- stats::cor(t0$sys_bp, t0$dia_bp, method = "spearman", use = "complete.obs")
  expect_gt(rho, 0.5)
  expect_lt(stats::cor(as.numeric(survey$mood_score > 0), survey$sleep_hours,
                       method = "spearman", use = "complete.obs"), 0)
})

test_that("the same seed gives the same tables whichever are requested", {
  set.seed(99)
  before <- .Random.seed
  again <- react_clinical(source, tables = "clinic_data", profile = profile)
  expect_identical(.Random.seed, before)
  expect_named(again, "clinic_data")
  expect_identical(again$clinic_data$clinical_tests$tests_t0, t0)
  other <- react_clinical(source, tables = "clinic_data/clinical_tests/tests_t1", profile = profile, seed = 4L)
  expect_false(identical(other$clinic_data$clinical_tests$tests_t1, t1))
  expect_error(react_clinical(source, tables = "bloods", profile = profile), "Unknown Clinical Study table")
})

test_that("n scales people and a small pool is refused", {
  small <- react_clinical(source, tables = c("registration_data", "clinic_data"), profile = profile, n = 100L)
  expect_identical(nrow(small$registration_data), 100L)
  expect_identical(nrow(small$clinic_data$clinical_tests$tests_t0), 75L)
  expect_identical(nrow(small$clinic_data$clinical_tests$tests_t1), 15L)
  expect_error(react_clinical(source, profile = profile, n = 5000L), "fewer than the 5000 registrants")
})

test_that("people can be supplied as a data frame", {
  people <- data.frame(SUBJECT_ID = as.character(200000000 + 1:500), sex = rep(c("Female", "Male"), 250),
                       age = rep(c(17, 30, 45, 60, 80), 100), stringsAsFactors = FALSE)
  result <- react_clinical(source, tables = "registration_data", profile = profile, people = people, n = 300L)
  ids <- result$registration_data$subject_id
  expect_true(all(ids %in% as.numeric(people$SUBJECT_ID)))
  expect_false(any(ids %in% as.numeric(people$SUBJECT_ID[people$age < 18])))
})

test_that("partial tables are generated with a warning", {
  partial <- profile
  partial$tables$n_cols[partial$tables$table == "react_records"] <- 10
  expect_warning(react_clinical(source, tables = "react_records", profile = partial),
                 "lists only some columns of: react_records")
})

test_that("the real list is read and checked against the profile", {
  path <- file.path(withr::local_tempdir(), "ge_lc_wrangled.rds")
  saveRDS(gelc, path)
  real <- react_clinical(react_wrangled(path), tables = c("registration_data", "health_survey/survey_t0"))
  expect_identical(real$registration_data, reg)
  expect_named(real$health_survey, "survey_t0")
  changed <- gelc
  changed$registration_data$extra <- 1
  expect_warning(react_clinical(react_wrangled(changed), profile = profile),
                 "registration_data: column names or order differ")
  expect_silent(react_clinical(react_wrangled(gelc), profile = profile))
  expect_error(react_wrangled(file.path(tempdir(), "missing.rds")), "RDS file")
  files <- structure(list(kind = "files"), class = c("react_file_source", "react_source"))
  expect_error(react_clinical(files), "react_synthetic\\(\\)` or `react_wrangled")
})

test_that("omics scores follow the low-rank structure", {
  platform <- profile$omics_platform[profile$omics_platform$platform == "panel_a", ]
  u <- .hes_with_seed(5L, .clinical_omics_scores(profile, platform, 3000L))
  rho <- stats::cor(u, method = "spearman")
  expect_gt(rho[1, 2], 0.45)
  expect_lt(rho[1, 2], 0.75)
  expect_lt(abs(rho[1, 4]), 0.1)
  bad <- platform
  bad$platform <- "panel_z"
  expect_error(.clinical_omics_scores(profile, bad, 10L), "no features for platform panel_z")
})

omics <- react_clinical_omics(source, profile = profile)
panel <- omics$panel

test_that("omics files have the structure of the real files", {
  expect_named(omics, c("panel", "rnaseq_t0", "gene"))
  # the lookup table wasn't profiled, so it is left out
  expect_named(panel, c("df_panel_a", "df_panel_long", "df_stack"))
  wide <- panel$df_panel_a
  expect_identical(class(wide), c("tbl_df", "tbl", "data.frame"))
  expect_identical(names(wide), c("subject_id", "sample_id", "timepoint", paste0("f", 1:6)))
  expect_identical(nrow(wide), 240L)
  expect_true(all(grepl("^S[0-9]{5}$", wide$sample_id)))
  long <- panel$df_panel_long
  expect_identical(nrow(long), 240L * 6L)
  expect_setequal(names(long), c("subject_id", "sample_ref", "protein", "assay_group", "value", "value_norm", "lod"))
  # a column with no more values than features describes the feature
  expect_identical(nrow(unique(long[c("protein", "assay_group")])), 6L)
  expect_identical(unique(panel$df_stack$platform), "A")
  tx <- omics$rnaseq_t0
  expect_named(tx, c("abundance", "counts", "length", "countsFromAbundance"))
  expect_identical(dim(tx$counts), c(5L, 50L))
  expect_identical(rownames(tx$abundance), paste0("G", 1:5))
  expect_identical(colnames(tx$counts), colnames(tx$abundance))
  expect_true(all(colSums(tx$counts) > 1e7))
})

test_that("omics samples belong to flagged people of the same cohort", {
  wide <- panel$df_panel_a
  ids <- as.numeric(wide$subject_id)
  expect_true(all(ids %in% t0$subject_id))
  # 40 follow-up samples, from the follow-up group; some have no baseline sample
  expect_lte(sum(duplicated(ids)), 40L)
  expect_gt(sum(duplicated(ids)), 10L)
  expect_true(all(ids[duplicated(ids)] %in% t1$subject_id))
  carriers <- omics$gene
  expect_identical(nrow(carriers), 45L)
  # identifiers shared with the wrangled list link the files
  expect_true(all(carriers$REG_CODE %in% reg$reg_code))
})

test_that("tables holding the same measurements agree", {
  wide <- as.data.frame(panel$df_panel_a)
  long <- as.data.frame(panel$df_panel_long)
  first <- long[long$sample_ref == wide$sample_id[[1]], ]
  # P1-P6 are f1-f6 in reverse order (matched by their identical quantiles)
  expect_equal(first$value[first$protein == "P6"], wide$f1[[1]])
  expect_equal(first$value[first$protein == "P2"], wide$f5[[1]])
  expect_equal(first$value_norm, first$value - 10)
  stack <- as.data.frame(panel$df_stack)
  expect_equal(stack$value, long$value)
  expect_identical(stack$sample_id, long$sample_ref)
})

test_that("follow-up omics samples are correlated with baseline samples", {
  wide <- as.data.frame(panel$df_panel_a)
  repeat_ids <- wide$subject_id[duplicated(wide$subject_id)]
  base <- wide[wide$subject_id %in% repeat_ids & !duplicated(wide$subject_id), ]
  later <- wide[duplicated(wide$subject_id), ]
  later <- later[match(base$subject_id, later$subject_id), ]
  expect_gt(stats::cor(base$f3, later$f3, method = "spearman"), 0.4)
  expect_error(react_clinical_omics(source, sources = "bloods", profile = profile), "Unknown or unprofiled omics source")
})
