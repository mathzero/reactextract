escape_value <- function(value) {
  if (is.na(value)) return("<DATABASE_NULL>")
  encodeString(enc2utf8(as.character(value)), quote = "\"", na.encode = FALSE)
}

unicode_points <- function(value) {
  if (is.na(value)) return(NA_character_)
  points <- utf8ToInt(enc2utf8(as.character(value)))
  if (!length(points)) return("<EMPTY>")
  paste(sprintf("U+%04X", points), collapse = " ")
}

diagnostic_class <- function(value, support) {
  if (is.na(value)) return("database_missing")
  exact <- match(value, support$raw_value)
  if (!is.na(exact)) {
    return(paste0("reviewed_", support$outcome_state[[exact]]))
  }
  if (nzchar(value) && grepl("^[[:space:]]+$", value)) {
    return("whitespace_only_requires_review")
  }
  normalised_for_diagnosis <- tolower(trimws(value))
  if (normalised_for_diagnosis %in% c("detected", "not detected", "void")) {
    return("format_variant_requires_review")
  }
  "unreviewed_value"
}

target_support <- function(dictionary, target) {
  override <- dictionary$synthetic_profile_overrides[
    dictionary$synthetic_profile_overrides$occurrence_id ==
      target$occurrence_id[[1L]] &
      dictionary$synthetic_profile_overrides$review_state == "approved",
    , drop = FALSE
  ]
  if (nrow(override) != 1L ||
      !identical(override$support_id[[1L]], target$support_id[[1L]])) {
    stop(
      "The installed dictionary does not resolve the expected exact support for ",
      target$round_id[[1L]], "/", target$variable[[1L]], ".",
      call. = FALSE
    )
  }
  support <- dictionary$synthetic_public_supports[
    dictionary$synthetic_public_supports$support_id ==
      target$support_id[[1L]] &
      dictionary$synthetic_public_supports$review_state == "approved",
    , drop = FALSE
  ]
  if (!nrow(support) || !"outcome_state" %in% names(support) ||
      anyNA(support$raw_value) || anyDuplicated(support$raw_value) ||
      any(!(support$outcome_state %in% c("detected", "negative", "missing")))) {
    stop("The installed occurrence-specific result support is incomplete.", call. = FALSE)
  }
  support
}

query_target <- function(source, target, support) {
  field <- target$variable[[1L]]
  object <- target$object_name[[1L]]
  sql <- paste0(
    "SELECT RAW_VALUE, CHARACTER_LENGTH, BYTE_LENGTH, ORACLE_DUMP_HEX, ",
    "COUNT(*) AS EXACT_COUNT FROM (SELECT ", field, " AS RAW_VALUE, ",
    "LENGTH(", field, ") AS CHARACTER_LENGTH, ",
    "LENGTHB(", field, ") AS BYTE_LENGTH, ",
    "DUMP(", field, ", 1016) AS ORACLE_DUMP_HEX FROM ", object,
    " WHERE ", field, " IS NOT NULL) ",
    "GROUP BY RAW_VALUE, CHARACTER_LENGTH, BYTE_LENGTH, ORACLE_DUMP_HEX"
  )
  rows <- source$query_fn(source$connection, sql)
  names(rows) <- tolower(names(rows))
  required <- c(
    "raw_value", "character_length", "byte_length",
    "oracle_dump_hex", "exact_count"
  )
  if (!all(required %in% names(rows))) {
    stop(
      "The aggregate Oracle diagnostic did not return its expected columns.",
      call. = FALSE
    )
  }
  rows <- rows[
    !(as.character(rows$raw_value) %in% support$raw_value), , drop = FALSE
  ]
  if (!nrow(rows)) return(data.frame())
  data.frame(
    round_id = target$round_id[[1L]],
    variable = field,
    occurrence_id = target$occurrence_id[[1L]],
    support_id = target$support_id[[1L]],
    escaped_value = vapply(rows$raw_value, escape_value, character(1L)),
    unicode_code_points = vapply(rows$raw_value, unicode_points, character(1L)),
    character_length = suppressWarnings(as.integer(rows$character_length)),
    byte_length = suppressWarnings(as.integer(rows$byte_length)),
    oracle_dump_hex = as.character(rows$oracle_dump_hex),
    diagnostic_class = vapply(
      as.character(rows$raw_value), diagnostic_class, character(1L),
      support = support
    ),
    exact_count = suppressWarnings(as.numeric(rows$exact_count)),
    stringsAsFactors = FALSE
  )
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 2L) {
    stop(
      paste(
        "Usage: Rscript diagnose_lab_result_values.R",
        "CONNECTION_SCRIPT OUTPUT_DIRECTORY"
      ),
      call. = FALSE
    )
  }
  connection_script <- normalizePath(args[[1L]], mustWork = TRUE)
  output <- args[[2L]]
  if (dir.exists(output) && length(list.files(
    output, all.files = TRUE, no.. = TRUE
  ))) {
    stop("The output must be a new or empty directory.", call. = FALSE)
  }
  dir.create(output, recursive = TRUE, showWarnings = FALSE)

  connection_environment <- new.env(parent = globalenv())
  sys.source(connection_script, envir = connection_environment)
  if (!exists("con", envir = connection_environment, inherits = FALSE)) {
    stop("The connection script must create an object named `con`.", call. = FALSE)
  }
  con <- get("con", envir = connection_environment, inherits = FALSE)
  if (!inherits(con, "DBIConnection")) {
    stop("The connection script did not create a DBI connection.", call. = FALSE)
  }
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  source <- reactextract::react_oracle(con)
  dictionary <- reactextract::react_dictionary()

  # These are the only round/field pairs rejected by the checksum-pinned v5
  # repair. Queries aggregate by exact value and never select identifiers or
  # respondent-level rows.
  targets <- data.frame(
    round_id = c("react1.r02", "react1.r11", "react1.r13"),
    object_name = c("REACT1_R02_V", "REACT1_R11_V", "REACT1_R13_V"),
    variable = c("FINALRESULT", "RESULT", "RESULT"),
    occurrence_id = c(
      "occ_3d8dfcbd9554c0afc41ef7e2",
      "occ_654a0c1cc8cdb5d263af8bad",
      "occ_02fe3a74aee39c3150210a30"
    ),
    support_id = c(
      "react1_pcr_lab_result_r02_final_v1",
      "react1_pcr_lab_result_r11_result_v1",
      "react1_pcr_lab_result_r13_result_v1"
    ),
    stringsAsFactors = FALSE
  )
  inventory <- reactextract:::.bind_rows(
    lapply(seq_len(nrow(targets)), function(index) {
      target <- targets[index, , drop = FALSE]
      message(
        "[reactextract] Diagnosing ", target$round_id[[1L]], "/",
        target$variable[[1L]], " using aggregate counts only"
      )
      query_target(source, target, target_support(dictionary, target))
    }),
    data.frame()
  )
  if (!nrow(inventory)) {
    inventory <- data.frame(
      round_id = character(), variable = character(),
      occurrence_id = character(), support_id = character(),
      escaped_value = character(),
      unicode_code_points = character(), character_length = integer(),
      byte_length = integer(), oracle_dump_hex = character(),
      diagnostic_class = character(), exact_count = numeric(),
      stringsAsFactors = FALSE
    )
  }

  enclave_path <- file.path(output, "lab-result-values-enclave-only.csv")
  utils::write.csv(inventory, enclave_path, row.names = FALSE, na = "")

  review <- inventory
  review$suppressed <- review$exact_count < 10
  review$count <- ifelse(
    review$suppressed, NA_real_, round(review$exact_count / 5) * 5
  )
  review$exact_count <- NULL
  review_path <- file.path(output, "lab-result-values-for-review.csv")
  utils::write.csv(review, review_path, row.names = FALSE, na = "")

  notes <- c(
    "REACT laboratory-result value diagnostic",
    "",
    "This diagnostic contains aggregate values only; it never selects identifiers or rows.",
    "lab-result-values-enclave-only.csv contains exact counts and must remain in the enclave.",
    "lab-result-values-for-review.csv suppresses counts below 10 and rounds other counts to 5.",
    "The review file still requires normal enclave disclosure review before it is copied out.",
    "Diagnostic classifications are prompts for human review, not automatic recoding decisions."
    ,
    paste(
      "The installed dictionary supplies occurrence-specific exact supports:",
      paste(targets$support_id, collapse = ", ")
    )
  )
  writeLines(notes, file.path(output, "README.txt"), useBytes = TRUE)

  files <- c(enclave_path, review_path, file.path(output, "README.txt"))
  manifest <- data.frame(
    file = basename(files),
    sha256 = vapply(files, reactextract:::.sha256_file, character(1L)),
    byte_size = as.numeric(file.info(files)$size),
    stringsAsFactors = FALSE
  )
  utils::write.csv(
    manifest, file.path(output, "manifest.csv"), row.names = FALSE, na = ""
  )
  message(
    "[reactextract] Diagnostic written to ",
    normalizePath(output, mustWork = TRUE),
    ". Keep the enclave-only file inside the enclave."
  )
}

main()
