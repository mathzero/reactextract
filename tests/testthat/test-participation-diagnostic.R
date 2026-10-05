pv6_file <- system.file("enclave", "participation-v6", "diagnostic.R", package = "reactextract")
source(pv6_file, local = TRUE)

test_that("diagnostic states preserve distinct missingness and never emit text", {
  x <- c(NA, "-77", "-91", "-92", "-66", "-99", "-555", "-123", "NA", "private text")
  expect_identical(pv6_state(x), c(pv6_states, "substantive"))
  expect_identical(pv6_state(as.Date(c(NA, "2020-01-01"))), c("database_missing", "substantive"))
  expect_identical(pv6_age(c(4, 5, 11, 12, 13, 15, 16, 17, 18, 54, 55, NA, -77)),
    c("under_5", "5_11", "5_11", "12", "13_15", "13_15", "16_17", "16_17", "18_54", "18_54", "55_plus", "missing_or_invalid", "missing_or_invalid"))
  expect_error(pv6_quote('A" OR 1=1'), "Unsafe")
  expect_false(grepl("-77", pv6_state_sql("DOB", "DATE"), fixed = TRUE))
  expect_identical(pv6_marker(c(NA, "0", "1", "2", "3", "-77", "private")), pv6_marker_levels)
  expect_match(pv6_context_sql("SURVEY_COMPLETE", "marker"), "THEN 5", fixed = TRUE)
  expect_identical(pv6_state(-1e-10), "unrecognised_negative")
})

test_that("keyed diagnostic batches never align by row order", {
  d <- data.frame(U_PASSCODE = c("b", "a"), SMOKENOW = c(2, 1))
  expect_identical(pv6_align(d, "U_PASSCODE", c("a", "b"))$SMOKENOW, c(1, 2))
  expect_error(pv6_align(d, "U_PASSCODE", c("a", "c")), "key_mismatch")
  expect_error(pv6_align(d[c(1, 1), ], "U_PASSCODE", c("a", "b")), "key_mismatch")
  calls <- character()
  source <- list(kind = "oracle", connection = NULL, query_fn = function(con, sql) {
    calls <<- c(calls, sql)
    if (grepl('AS "MISSING"', sql, fixed = TRUE)) stop("secret database error")
    data.frame(U_PASSCODE = c("b", "a"), SMOKENOW = c(8L, 1L))
  })
  reg <- data.frame(observation_key = "U_PASSCODE", object_name = "SAFE_VIEW")
  got <- pv6_fetch(source, reg, c("a", "b"), c("SMOKENOW", "MISSING"), c("NUMBER", "NUMBER"))
  expect_identical(got$data$SMOKENOW, c("survey_code_77", "substantive"))
  expect_identical(got$failures, "MISSING")
  expect_true(all(grepl('"U_PASSCODE"', calls, fixed = TRUE)))
  expect_false(any(grepl("SELECT *", calls, fixed = TRUE)))
})

test_that("disclosure preparation coordinates linked counts and never writes internal results", {
  d <- rbind(pv6_counts("r1", "field_states", "A", "", c(rep("a", 3), rep("b", 27)), c("a", "b")),
    pv6_counts("r1", "field_states", "B", "", c(rep("a", 3), rep("b", 27)), c("a", "b")),
    pv6_counts("r1", "round_denominator", "", "", rep("records", 30), "records"))
  result <- list(counts = d, inventory = data.frame(), issues = data.frame(),
    manifest = data.frame(key = "status", value = "internal"))
  expect_error(write_participation_diagnostic(result, tempfile()), "Prepare")
  out <- prepare_participation_diagnostic_export(result)
  expect_true(all(out$counts$suppressed))
  expect_true(all(is.na(out$counts$count)))
  result$counts <- pv6_counts("r2", "field_states", "A", "", c(rep("a", 11), rep("b", 24)), c("a", "b"))
  out <- prepare_participation_diagnostic_export(result)
  expect_equal(out$counts$count, c(10, 25))
  result$counts <- pv6_counts("r3", "field_states", "A", "", c(rep("a", 3), rep("b", 27)), c("a", "b", "zero"))
  out <- prepare_participation_diagnostic_export(result)
  expect_true(all(out$counts$suppressed))
})

test_that("a complete file diagnostic returns aggregates and flags partial questionnaire patterns", {
  contract <- system.file("enclave", "participation-v6", package = "reactextract")
  n <- 60L
  raw <- data.frame(U_PASSCODE = paste0("fictional", seq_len(n)), U_AGE = rep(c(12L, 17L, 18L), each = 20),
    U_MAIL_GRP = rep(c(3L, 2L, 1L), each = 20), SMOKENOW = c(rep(-77L, 20), rep(2L, 40)),
    FEELUN = c(rep(-77L, 10), rep(1L, 50)), U_GENDER = 1L, RESULT = "Detected")
  p <- read.csv(file.path(contract, "occurrence_provenance.csv"))
  date_fields <- subset(p, round_id == "react1.r02" & participation_scope_proposed == "individual" & data_type == "DATE")$variable
  expect_true(length(date_fields) > 0L)
  raw[[date_fields[1L]]] <- as.Date(c("2020-01-01", rep(NA_character_, n - 1L)))
  result <- run_participation_diagnostic(react_files(list(REACT1_R02 = raw)), contract,
    rounds = "REACT1_R02", progress = FALSE)
  expect_true(nrow(result$issues) > 0)
  expect_equal(sum(subset(result$counts, table == "mail_age_agreement" & field == "U_MAIL_GRP")$count), n)
  expect_false(any(c("U_PASSCODE", "SUBJECT_ID", "value", "raw_value") %in% names(result$counts)))
  expect_false(any(grepl("fictional|Detected", unlist(result), fixed = FALSE)))
  smoke <- subset(result$counts, table == "field_states" & field == "SMOKENOW" & state == "survey_code_77")
  expect_equal(smoke$count, 20)
  preserved <- subset(result$counts, table == "field_states" & field == "RESULT" & state == "substantive")
  expect_equal(preserved$count, 60)
  stage <- subset(result$counts, table == "provisional_stage_pattern" & field == "individual")
  expect_equal(stage$count[stage$state == "all_fields_77"], 9)
  expect_equal(stage$count[stage$state == "77_and_substantive"], 1)
  bad <- raw
  bad$U_PASSCODE[2] <- bad$U_PASSCODE[1]
  failed <- run_participation_diagnostic(react_files(list(REACT1_R02 = bad)), contract,
    rounds = "REACT1_R02", progress = FALSE)
  expect_equal(nrow(failed$counts), 0)
  expect_true("key_not_unique_or_missing_round_skipped" %in% failed$issues$issue)
})
