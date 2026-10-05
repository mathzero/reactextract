kit <- Sys.getenv("REACT_PARTICIPATION_FOLLOWUP_DIR", unset = system.file("enclave", "participation-v6-followup", package = "reactextract"))
source(file.path(kit, "diagnostic-v1.R"), local = TRUE)
source(file.path(kit, "followup.R"), local = TRUE)

test_that("public age boundaries allow decimal years and distinguish quality", {
  ages <- c(NA, -77, 4.9, 5, 11.9, 12, 12.99, 13, 15.9, 16, 17.99, 18, 54.99, 55, 120.9, 121)
  expect_identical(pf6_age(ages), c("missing_or_invalid", "missing_or_invalid", "under_5", "5_11", "5_11", "12", "12", "13_15", "13_15", "16_17", "16_17", "18_54", "18_54", "55_plus", "55_plus", "missing_or_invalid"))
  expect_identical(pf6_quality(c(NA, -77, 121, 18, 17.9)), pf6_quality_levels)
  expect_false(grepl("FLOOR", pf6_projection("AGE", "NUMBER", "age")$sql))
  expect_match(pf6_projection("AGE", "NUMBER", "age_quality")$sql, "FLOOR")
  expect_error(pf6_projection("AGE", "VARCHAR2", "age"), "verified numeric")
  expect_error(pf6_projection("A; DROP TABLE X", "NUMBER", "state"), "Unsafe")
  p <- pf6_projection("SFREPORTFIG", "NUMBER", "codes", c("-77", "0", "1"))
  expect_identical(p$levels, c("database_missing", "code_-77", "code_0", "code_1", "other_code_not_released"))
  expect_match(p$sql, "ELSE 4 END", fixed = TRUE)
  expect_identical(pf6_file_projection(c(NA, "-77", "1", "confidential"), "codes", c("-77", "1")), c("database_missing", "code_-77", "code_1", "other_code_not_released"))
  expect_identical(pf6_file_projection(as.Date(c(NA, "2021-01-01")), "presence", character()), c("database_missing", "source_nonmissing_not_proof_of_completion"))
})

pf_test_internal <- function(counts, schema = "participation-targeted-v2") list(counts = counts,
  inventory = data.frame(round_id = character(), field = character(), role = character(), status = character()),
  issues = data.frame(round_id = character(), field = character(), issue = character()),
  manifest = data.frame(key = c("schema", "status", "dictionary_manifest_sha256"), value = c(schema, pf6_internal, pf6_dictionary_hash)))

test_that("reuse validates the original contract and makes no database queries", {
  d <- pv6_counts("react1.r06", "code77_agreement", "SFREPORTFIG", "FEELUN",
    c(rep("77", 30), rep("not77", 70)), c("77", "not77"),
    c(rep("code_minus77", 20), rep("code_1", 80)), pv6_marker_levels)
  original <- list(counts = d, inventory = data.frame(), issues = data.frame(round_id = character(), field = character(), issue = character()),
    manifest = data.frame(key = c("schema", "status", "dictionary_manifest_sha256", "provenance_sha256", "package_version", "requested_rounds"),
      value = c("participation-diagnostic-v1", pf6_internal, pf6_dictionary_hash, pf6_provenance_hash, "0.5.4", "react1.r06")))
  before <- serialize(original, NULL)
  reused <- pf6_reuse(original)
  expect_equal(reused$counts$count, c(10, 0, 90))
  expect_identical(pf6_meta(reused, "database_queries"), "0")
  expect_identical(serialize(original, NULL), before)
  protected <- pf6_prepare_export(reused)
  expect_equal(protected$counts$count, c(10, NA, NA))
  expect_identical(protected$counts$protection, c("released_rounded_to_5", "below_10", "withheld_remainder"))
  # Lost or suppressed results cannot be repaired by guessing zeroes.
  bad <- original
  bad$counts$count[1] <- NA
  expect_error(pf6_reuse(bad), "unsuppressed")
  bad <- original
  bad$manifest$value[bad$manifest$key == "status"] <- pf6_pending
  expect_error(pf6_reuse(bad), "original participation")
  bad <- original
  bad$manifest$value[bad$manifest$key == "provenance_sha256"] <- "wrong"
  expect_error(pf6_reuse(bad), "original participation")
})

test_that("suppression follows real partitions, not equal numbers in a round", {
  # The value 30 must be hidden as A's complement, but the unrelated B=30
  # is releasable. The v1 global-equal-count cascade would hide both.
  a <- pf6_counts("react1.r06", "source_marginal", "A", "", c(rep("a", 3), rep("b", 30)), c("a", "b"))
  b <- pf6_counts("react1.r06", "source_marginal", "B", "", c(rep("a", 30), rep("b", 40)), c("a", "b"))
  x <- pf6_prepare_export(pf_test_internal(rbind(a, b)))$counts
  expect_true(all(x$suppressed[x$field == "A"]))
  expect_equal(x$count[x$field == "B"], c(30, 40))
  expect_identical(x$protection[x$field == "A"], c("below_10", "linked_complement"))
  expect_error(pf6_prepare_export(pf_test_internal(transform(a, count = -1))), "unsuppressed")
})

test_that("matrices protect rows, columns and mirrored copies; redundant marginals are omitted", {
  joint <- pf6_counts("react1.r06", "joint", "A", "B",
    c(rep("a", 3), rep("a", 30), rep("b", 20), rep("b", 40)), c("a", "b"),
    c(rep("x", 3), rep("y", 30), rep("x", 20), rep("y", 40)), c("x", "y"), "codes", "codes")
  mirror <- joint
  mirror$table <- "mirror"
  mirror$field <- "B"; mirror$reference <- "A"
  mirror$state <- joint$reference_state; mirror$reference_state <- joint$state
  marginal <- pf6_counts("react1.r06", "source_marginal", "A", "", c(rep("a", 33), rep("b", 60)), c("a", "b"), kind = "codes")
  out <- pf6_prepare_export(pf_test_internal(rbind(joint, mirror, marginal)))$counts
  expect_false(any(out$table == "source_marginal"))
  expect_identical(out$suppressed[out$table == "joint"], out$suppressed[out$table == "mirror"])
  z <- out[out$table == "joint", ]
  expect_true(all(z$suppressed))
  expect_false(any(c("round_denominator", "age_quality") %in% out$table))
})

pf_test_raw <- function(n = 240L) {
  group <- rep(1:4, length.out = n)
  data.frame(U_PASSCODE = paste0("private_key_", seq_len(n)),
    REGREPORTFIG = c(2, 0, -77, 2)[group], SFREPORTFIG = c(1, 0, -77, 1)[group],
    REGISTRATIONFILE = c(1, 1, 0, 1)[group], SYMPTOMFILE = c(1, 1, 0, 1)[group],
    INDCONF = c(1, 2, -77, 1)[group], INDCONFSF = c(1, -92, -77, 1)[group],
    FEELUN = c(1, -92, -77, 2)[group], SMOKENOW = c(-91, -92, -77, 2)[group],
    U_AGE = c(12, 17.9, 18, 55)[group], AGE = c(12.9, 18.1, NA, 55)[group],
    U_MAIL_GRP = c(3, 2, 1, 1)[group], RESULT = c("private result text", NA, "Detected", "Void")[group],
    FINALRESULT = c("Detected", NA, "Detected", "Void")[group],
    DATE_OF_LAST_ACCESS = as.Date(c("2020-01-01", "2020-01-02", NA, "2020-01-04"))[group],
    DATE_OF_LAST_ACCESSSF = as.Date(c("2020-01-01", NA, NA, "2020-01-04"))[group],
    WHENCOMPLETION = as.Date(c("2020-01-01", NA, NA, "2020-01-04"))[group],
    ABATTEMPT = c(1, 2, -77, 1)[group], ABCOMP = c(1, 2, -77, 1)[group],
    NEWRESULT = c(1, -91, -77, 2)[group], NEWRESULT_2 = c(1, -91, -77, 2)[group])
}

test_that("targeted file checks use only safe states and retain stage/test distinctions", {
  raw <- pf_test_raw()
  x <- pf6_targeted(reactextract::react_files(list(REACT1_R06 = raw, REACT2_S5_R01 = raw)),
    rounds = c("REACT1_R06", "REACT2_S5_R01"), progress = FALSE)
  expect_setequal(unique(x$counts$round_id), c("react1.r06", "react2.r01"))
  expect_identical(pf6_meta(x, "schema"), "participation-targeted-v3")
  expect_match(pf6_meta(x, "missing_code_policy"), "literal_minus77")
  expect_false(any(grepl("private_key|private result text|2020-01|Detected", unlist(x))))
  expect_false(any(c("U_PASSCODE", "SUBJECT_ID", "raw_value") %in% names(x$counts)))
  z <- subset(x$counts, round_id == "react1.r06" & table == "smoking_by_age" & reference == "U_AGE" & reference_state == "16_17" & state == "code_-92")
  expect_equal(z$count, 60)
  z <- subset(x$counts, round_id == "react1.r06" & table == "field_by_stage_marker" & field == "RESULT" & reference == "SFREPORTFIG" & reference_state == "code_-77" & state == "substantive")
  expect_equal(z$count, 60) # source result retained for questionnaire nonresponders
  quality <- subset(x$counts, round_id == "react1.r06" & field == "U_AGE" & field_kind == "age_quality" & state == "fractional_years")
  expect_equal(quality$count, 60)
  expect_true(any(x$counts$table == "mail_by_age"))
  safe <- pf6_prepare_export(x)
  expect_false(any(safe$counts$field_kind == "age_quality"))
  expect_true(any(safe$counts$table == "code77_discordance" & safe$counts$reference == "SFREPORTFIG"))
  expect_false(any(safe$counts$table == "field_by_stage_marker" & safe$counts$field == "FEELUN" & safe$counts$reference == "SFREPORTFIG"))
  expect_true(all(is.na(safe$counts$count) == safe$counts$suppressed))
  path <- tempfile()
  expect_error(pf6_write(x, path), "protected export")
  pf6_write(safe, path)
  checks <- read.csv(file.path(path, "checksums.csv"))
  expect_equal(nrow(checks), 5)
  for (j in seq_len(nrow(checks))) expect_identical(getFromNamespace(".sha256_file", "reactextract")(file.path(path, checks$file[j])), checks$sha256[j])
  expect_error(pf6_write(safe, path), "already exists")
  tampered <- safe
  tampered$counts$count[1] <- 3
  tampered$counts$suppressed[1] <- FALSE
  expect_error(pf6_write(tampered, tempfile()), "Invalid protected")
  # Failed/duplicate observations never trigger a row-position join.
  bad <- raw
  bad$U_PASSCODE[2] <- bad$U_PASSCODE[1]
  failed <- pf6_targeted(reactextract::react_files(list(REACT1_R06 = bad)), rounds = "REACT1_R06", progress = FALSE)
  expect_equal(nrow(failed$counts), 0)
  expect_match(failed$issues$issue, "key_not_unique")
  expect_error(pf6_targeted(reactextract::react_files(list(REACT1_R06 = raw)), rounds = "wrong"), "Unknown")
})

test_that("literal -77 is detected even when early dictionary response lists omit it", {
  dictionary <- reactextract::react_dictionary()
  before <- serialize(dictionary$response_options, NULL)
  q <- dictionary$response_options
  expect_false("-77" %in% q$return_value[q$round_id == "react1.r01" & q$variable == "SMOKENOW"])
  codes <- pf6_public_codes(dictionary, "react1.r01", "SMOKENOW")
  expect_identical(codes, c("-77", "1", "2", "3"))
  expect_identical(serialize(dictionary$response_options, NULL), before)
  expect_length(pf6_public_codes(dictionary, "react1.r01", "NONEXISTENT"), 0L)
  expect_identical(pf6_file_projection(c(-77, 1, 2, 3, -999, NA), "codes", codes),
    c("code_-77", "code_1", "code_2", "code_3", "other_code_not_released", "database_missing"))
  expect_match(pf6_projection("SMOKENOW", "NUMBER", "codes", codes)$sql, '"SMOKENOW" = -77', fixed = TRUE)
  expect_match(pf6_projection("SMOKENOW", "VARCHAR2", "codes", codes)$sql, '"SMOKENOW" = \'-77\'', fixed = TRUE)

  raw <- pf_test_raw(240L)
  raw$INDCONF <- 1
  raw$REGREPORTFIG <- 2
  raw$SFREPORTFIG <- 1
  raw$SMOKENOW <- rep(c(-77, 1, 2, -999), each = 60)
  raw$ABSFREPORTFIG <- 1
  affected <- c("react1.r01", sprintf("react2.r%02d", 1:6))
  result <- pf6_targeted(reactextract::react_files(setNames(rep(list(raw), 7L), affected)),
    rounds = affected, mail_fields = character(), progress = FALSE)
  expect_equal(nrow(result$issues), 0)
  z <- subset(result$counts, round_id == "react1.r01" & table == "code77_discordance" &
    field == "SMOKENOW" & reference == "SFREPORTFIG" & state == "field_only_code77")
  expect_equal(z$count, 60)
  z <- subset(result$counts, round_id == "react1.r01" & table == "source_marginal" &
    field == "SMOKENOW" & state == "other_code_not_released")
  expect_equal(z$count, 60) # -999 is not leaked or mistaken for -77
  safe <- pf6_prepare_export(result)
  expect_true(all(is.na(safe$counts$count) == safe$counts$suppressed))
  expect_false(any(grepl("private_key|-999", unlist(safe))))
  expect_false(any(result$inventory$role == "mail_group_probe"))
})

test_that("Oracle projections are keyed, state-only and recover safe fields after failure", {
  calls <- character()
  source <- list(kind = "oracle", connection = NULL, query_fn = function(con, sql) {
    calls <<- c(calls, sql)
    if (grepl('"MISSING"', sql, fixed = TRUE)) stop("secret database message")
    data.frame(U_PASSCODE = c("b", "a"), P1 = c(1, 0))
  })
  reg <- data.frame(object_name = "SAFE_VIEW", observation_key = "U_PASSCODE")
  s <- data.frame(field = c("SFREPORTFIG", "MISSING"), type = "NUMBER", kind = "codes", alias = c("P1", "P2"), codes = I(list(c("-77", "0", "1"), c("0", "1"))))
  x <- pf6_fetch(source, reg, c("a", "b"), s)
  expect_identical(x$data$P1, c("database_missing", "code_-77"))
  expect_identical(x$failures, "P2")
  expect_true(all(grepl("CASE WHEN", calls, fixed = TRUE)))
  expect_true(all(grepl('"U_PASSCODE"', calls, fixed = TRUE)))
  expect_false(any(grepl("SELECT *", calls, fixed = TRUE)))
  source$query_fn <- function(con, sql) data.frame(U_PASSCODE = c("a", "a"), P1 = c(0, 1))
  expect_identical(pf6_fetch(source, reg, c("a", "b"), s[1, , drop = FALSE])$failures, "P1")
})

test_that("reused comparisons always withhold their matching remainder", {
  d <- data.frame(round_id = "react1.r06", table = "code77_discordance", field = "F", reference = "R",
    state = c("field_only_code77", "reference_only_code77", "matching_remainder_withheld"),
    reference_state = "all", count = c(30, 70, 0), field_kind = "code77_discordance", reference_kind = "comparison_anchor")
  safe <- pf6_prepare_export(pf_test_internal(d, "participation-reexport-v2"))$counts
  expect_true(safe$suppressed[3])
  expect_true(any(safe$suppressed[1:2])) # small withheld remainder gets a positive complement
  expect_false(any(safe$table == "round_denominator"))
})

test_that("complete Oracle-fixture and file pipelines produce identical aggregates", {
  dictionary <- reactextract::react_dictionary()
  raw <- pf_test_raw()
  raw$ABSFREPORTFIG <- 1
  calls <- character()
  query_fn <- function(con, sql) {
    calls <<- c(calls, sql)
    object <- sub(".* FROM ([A-Z0-9_.]+).*", "\\1", sql)
    round <- dictionary$source_registry$round_id[match(object, dictionary$source_registry$object_name)]
    if (grepl("WHERE 1 = 0", sql, fixed = TRUE)) {
      field <- sub('SELECT "([^"]+)".*', "\\1", sql)
      if (!field %in% names(raw)) stop("not_available")
      return(raw[FALSE, field, drop = FALSE])
    }
    reverse <- rev(seq_len(nrow(raw)))
    out <- raw[reverse, "U_PASSCODE", drop = FALSE]
    expressions <- regmatches(sql, gregexpr('CASE WHEN .*? END AS "P[0-9]+"', sql, perl = TRUE))[[1L]]
    for (expression in expressions) {
      field <- sub('CASE WHEN "([^"]+)".*', "\\1", expression)
      alias <- sub('.* AS "([^"]+)"', "\\1", expression)
      if (!field %in% names(raw)) stop("field_unavailable")
      kind <- if (field == "U_MAIL_GRP") "mail" else if (field %in% c("U_AGE", "AGE")) {
        if (grepl("FLOOR", expression, fixed = TRUE)) "age_quality" else "age"
      } else if (field %in% c("DATE_OF_LAST_ACCESS", "DATE_OF_LAST_ACCESSSF", "WHENCOMPLETION")) "presence" else if (field %in% c("RESULT", "FINALRESULT", "ABSFREPORTFIG")) "state" else "codes"
      codes <- if (kind == "codes") pf6_public_codes(dictionary, round, field) else character()
      type <- if (kind == "mail") "VARCHAR2" else dictionary$occurrences$data_type[match(paste(round, field), paste(dictionary$occurrences$round_id, dictionary$occurrences$variable))]
      expected <- pf6_projection(field, type, kind, codes)
      expect_identical(expression, paste(expected$sql, "AS", pv6_quote(alias)))
      states <- pf6_file_projection(raw[[field]], kind, codes)
      out[[alias]] <- (match(states, expected$levels) - 1L)[reverse]
    }
    out
  }
  source <- list(kind = "oracle", connection = NULL, registry = dictionary$source_registry, query_fn = query_fn)
  rounds <- c("REACT1_R01", "REACT1_R06", "REACT2_S5_R01", "REACT2_S5_R06")
  oracle <- pf6_targeted(source, rounds = rounds, batch_size = 5L, progress = FALSE)
  files <- pf6_targeted(reactextract::react_files(setNames(rep(list(raw), length(rounds)), rounds)), rounds = rounds, batch_size = 5L, progress = FALSE)
  expect_identical(oracle$counts, files$counts)
  expect_identical(oracle$issues, files$issues)
  expect_identical(oracle$inventory, files$inventory)
  expect_false(any(grepl("SELECT *", calls, fixed = TRUE)))
  expect_false(any(grepl("SUBJECT_REACT_ID|private_key|private result text", calls)))
})
