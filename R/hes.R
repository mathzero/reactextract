# Linked Hospital Episode Statistics (HES) -------------------------------------

.hes_tables <- c(apc = "NHSE_HES_ADMITTED_PATIENTS", op = "NHSE_HES_OUTPATIENTS")

#' Hospital Episode Statistics for REACT participants
#'
#' Returns REACT's linked HES admitted patient care (one row per episode) and
#' outpatient (one row per appointment) records for a set of participants. As
#' with [react_extract()], only the source changes between synthetic and real
#' data:
#'
#' - with [react_synthetic()], fictional records are generated for the
#'   participants;
#' - with [react_oracle()], the real `NHSE_HES_ADMITTED_PATIENTS` and
#'   `NHSE_HES_OUTPATIENTS` tables are queried for the participants'
#'   `SUBJECT_ID`s.
#'
#' Both give the same table names, column names, column order and column
#' types (dates as `POSIXct`, numbers as double, codes as character), so code
#' written on synthetic records runs unchanged on the real ones. Records cover
#' April 2015 to March 2023.
#'
#' Synthetic records model each person's admissions and appointments by age
#' and sex, calendar trends including COVID-19 admissions, spells of one or
#' more episodes, day cases, length of stay, in-hospital deaths, diagnoses and
#' procedures by age, sex and admission type, and outpatient courses. Other
#' columns take plausible values independently of each other. See
#' `inst/HES_SYNTHETIC.md` for what is and is not captured. **Synthetic data
#' are for learning and writing code, not for findings.**
#'
#' @param source A source from [react_synthetic()] or [react_oracle()].
#' @param people The participants: an extraction result from
#'   [react_extract()], or a data frame with a `SUBJECT_ID` column. For a
#'   synthetic source, a data frame also needs `sex` and `age` (and may have
#'   `age_date` and `ethnicity`); an extraction result without the
#'   `people-households/demographics` family has those added from `source`.
#' @param tables `"apc"` (admitted patient care), `"op"` (outpatients) or both.
#' @param columns Optional column names to keep. `SUBJECT_ID` is always kept;
#'   each table keeps the requested columns it has.
#' @param seed Random seed for synthetic records; defaults to the source's
#'   seed. The same source, people, seed and `chunk_size` always give the same
#'   records, and the global random number state is left unchanged.
#' @param chunk_size Synthetic people generated at a time; lower it if memory
#'   is short.
#'
#' @return A named list of data frames, named like the REACT tables.
#' @export
react_hes <- function(source, people, tables = c("apc", "op"), columns = NULL,
                      seed = NULL, chunk_size = 20000L) {
  tables <- match.arg(tables, several.ok = TRUE)
  if (!inherits(source, "react_source")) {
    stop("`source` must be created by `react_synthetic()` or `react_oracle()`.", call. = FALSE)
  }
  if (inherits(source, "react_file_source")) {
    stop("HES records come from `react_synthetic()` or `react_oracle()`, not round files.", call. = FALSE)
  }
  profile <- .hes_profile()
  if (!is.null(columns)) {
    columns <- as.character(columns)
    known <- unique(unlist(lapply(profile$schema[tables], `[[`, "column")))
    unknown <- setdiff(columns, known)
    if (length(unknown) > 0) {
      stop("Unknown HES column(s): ", paste(unknown, collapse = ", "), call. = FALSE)
    }
  }

  if (inherits(source, "react_oracle_source")) {
    ids <- .hes_subject_ids(people)
    return(.hes_oracle(source, ids, tables, columns, profile))
  }

  if (inherits(people, "react_extract_result") &&
      !"people_households.demographics.gender" %in% names(people$data)) {
    people <- react_extract(source, families = "people-households/demographics",
                            rounds = unique(people$data$survey_id), progress = FALSE)
  }
  if (is.null(seed)) seed <- source$seed
  .hes_synthetic(.hes_people(people), tables, columns, seed, chunk_size, profile)
}

.hes_subject_ids <- function(people) {
  ids <- if (inherits(people, "react_extract_result")) {
    people$data$SUBJECT_ID
  } else if (is.data.frame(people)) {
    if (!"SUBJECT_ID" %in% names(people)) stop("`people` needs a SUBJECT_ID column.", call. = FALSE)
    people$SUBJECT_ID
  } else {
    people
  }
  ids <- unique(ids[!is.na(ids)])
  if (length(ids) == 0) stop("`people` has no SUBJECT_ID values.", call. = FALSE)
  ids
}

# Real records: the linked tables queried in batches of 1,000 SUBJECT_IDs
# (Oracle's limit for an IN list). Column names come from the profile schema,
# so only known names reach the SQL.
.hes_oracle <- function(source, ids, tables, columns, profile) {
  text <- trimws(format(ids, scientific = FALSE, trim = TRUE))
  if (!all(grepl("^[0-9]+$", text))) {
    stop("SUBJECT_ID values must be whole numbers for the linked HES tables.", call. = FALSE)
  }
  batches <- split(text, ceiling(seq_along(text) / 1000))
  result <- list()
  for (table in tables) {
    keep <- if (is.null(columns)) "*" else {
      schema_columns <- profile$schema[[table]]$column
      paste(schema_columns[schema_columns %in% c("SUBJECT_ID", columns)], collapse = ", ")
    }
    parts <- lapply(batches, function(batch) {
      sql <- sprintf("SELECT %s FROM %s WHERE SUBJECT_ID IN (%s)", keep, .hes_tables[[table]],
                     paste(batch, collapse = ", "))
      source$query_fn(source$connection, sql)
    })
    result[[.hes_tables[[table]]]] <- do.call(rbind, parts)
  }
  result
}

.hes_synthetic <- function(people, tables, columns, seed, chunk_size, profile) {
  if (nrow(people) == 0) stop("`people` has no rows.", call. = FALSE)
  .hes_with_seed(seed, {
    n <- nrow(people)
    unknown_sex <- is.na(people$sex)
    people$sex[unknown_sex] <- sample(c("1", "2"), sum(unknown_sex), replace = TRUE)
    unknown_ethnicity <- is.na(people$ethnicity)
    people$ethnicity[unknown_ethnicity] <- .hes_sample(profile$ethnicity$known, sum(unknown_ethnicity))
    people$female <- as.numeric(people$sex == "2")
    people$birth <- as.numeric(people$age_date) - (people$age + stats::runif(n)) * 365.25

    # correlated frailties: people who are admitted more also attend more
    rho <- profile$frailty_correlation
    z_apc <- stats::rnorm(n)
    z_op <- rho * z_apc + sqrt(1 - rho^2) * stats::rnorm(n)
    k_apc <- exp(profile$rates$apc$par[["log_k"]])
    k_op <- exp(profile$rates$op$par[["log_k"]])
    frailty_apc <- stats::qgamma(stats::pnorm(z_apc), shape = k_apc, rate = k_apc)
    frailty_op <- stats::qgamma(stats::pnorm(z_op), shape = k_op, rate = k_op)

    apc_parts <- list()
    op_parts <- list()
    for (idx in split(seq_len(n), ceiling(seq_len(n) / chunk_size))) {
      p <- people[idx, , drop = FALSE]
      # admitted care always runs: in-hospital deaths end later outpatient activity
      apc <- .hes_apc(p, frailty_apc[idx], profile)
      if ("apc" %in% tables && !is.null(apc$data)) {
        apc_parts[[length(apc_parts) + 1L]] <- .hes_as_table(apc$data, profile$schema$apc)
      }
      if ("op" %in% tables) {
        op <- .hes_op(p, frailty_op[idx], profile, apc$death_day)
        if (!is.null(op)) op_parts[[length(op_parts) + 1L]] <- .hes_as_table(op, profile$schema$op)
      }
    }

    result <- list()
    if ("apc" %in% tables) {
      apc <- .hes_bind(apc_parts, profile$schema$apc, people$SUBJECT_ID)
      apc$EPIKEY <- .hes_unique_keys(nrow(apc))
      result[[.hes_tables[["apc"]]]] <- apc
    }
    if ("op" %in% tables) {
      op <- .hes_bind(op_parts, profile$schema$op, people$SUBJECT_ID)
      op$ATTENDKEY <- .hes_unique_keys(nrow(op))
      result[[.hes_tables[["op"]]]] <- op
    }
    if (!is.null(columns)) {
      result <- lapply(result, function(d) d[names(d) %in% c("SUBJECT_ID", columns)])
    }
    result
  })
}

# Columns in the REACT order and with the types DBI/odbc returns for the Oracle
# tables: DATE as POSIXct (UTC midnight), NUMBER as double, VARCHAR2 as
# character. SUBJECT_ID keeps the type of the IDs supplied.
.hes_as_table <- function(cols, schema) {
  n <- length(cols$SUBJECT_ID)
  out <- vector("list", nrow(schema))
  names(out) <- schema$column
  for (j in seq_len(nrow(schema))) {
    column <- schema$column[j]
    x <- cols[[column]]
    if (is.null(x)) x <- rep(NA, n)
    out[[j]] <- if (column == "SUBJECT_ID") {
      x
    } else {
      switch(schema$type[j],
             DATE = .POSIXct(as.numeric(x) * 86400, tz = "UTC"),
             NUMBER = as.numeric(x),
             as.character(x))
    }
  }
  structure(out, class = "data.frame", row.names = .set_row_names(n))
}

.hes_bind <- function(parts, schema, ids) {
  if (length(parts) == 0) {
    return(.hes_as_table(list(SUBJECT_ID = ids[0]), schema))
  }
  do.call(rbind, parts)
}

.hes_with_seed <- function(seed, expr) {
  seed <- as.integer(seed)
  if (length(seed) != 1L || is.na(seed)) stop("`seed` must be one whole number.", call. = FALSE)
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_kind <- RNGkind()
  on.exit({
    RNGkind(old_kind[1], old_kind[2], old_kind[3])
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  })
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  force(expr)
}
