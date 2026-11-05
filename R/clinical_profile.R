# Clinical Study profile: a checked inventory-kit candidate ----------------------

# The candidate's files, by the name used in the profile object. Only the
# structure files are required; files that are absent or empty give empty
# tables, so candidates from earlier kit versions still read.
.clinical_profile_files <- c(
  sources = "inventory/00_sources.csv",
  source_issues = "inventory/00_source_issues.csv",
  context = "inventory/01_context.csv",
  structure = "inventory/02_structure.csv",
  tables = "inventory/03_tables.csv",
  columns = "inventory/04_columns.csv",
  source_columns = "inventory/04b_source_columns.csv",
  id_sets = "inventory/08_id_sets.csv",
  numeric = "profile/10_numeric.csv",
  categorical = "profile/11_categorical.csv",
  date_summary = "profile/12_date_summary.csv",
  date_months = "profile/13_date_months.csv",
  id_shapes = "profile/14_id_shapes.csv",
  availability = "profile/15_availability.csv",
  availability_patterns = "profile/16_availability_patterns.csv",
  availability_pairs = "profile/17_availability_pairs.csv",
  correlations = "profile/18_correlations.csv",
  visit_pairs = "profile/19_visit_pairs.csv",
  timing = "profile/20_timing.csv",
  omics_platform = "profile/30_omics_platform.csv",
  omics_features = "profile/31_omics_features.csv",
  omics_samples = "profile/32_omics_samples.csv",
  omics_eigen = "profile/33_omics_eigen.csv",
  omics_loadings = "profile/34_omics_loadings.csv",
  omics_r2 = "profile/35_omics_r2.csv",
  omics_visit = "profile/36_omics_visit.csv",
  omics_tokens = "profile/37_omics_tokens.csv"
)

.clinical_quantile_columns <- c("p01", "p05", "p10", "p25", "p50", "p75", "p90", "p95", "p99")
.clinical_quantile_probs <- c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99)
.clinical_missing_codes <- c(-77, -91, -92, -99, -66, -555, -999)

# Columns the generator reads from each file
.clinical_profile_required <- list(
  sources = c("source", "status"),
  context = c("key", "value"),
  structure = c("source", "path", "class", "n_rows", "n_cols"),
  tables = c("source", "table", "visit", "n_rows", "n_cols", "has_persons", "n_persons",
             "n_persons_main", "rpp_1", "rpp_2", "rpp_3_5", "rpp_6_10", "rpp_11_plus"),
  columns = c("source", "table", "position", "column", "class", "role",
              "n_nonmissing", "n_missing", "n_distinct"),
  source_columns = c("source", "table", "position", "column", "class"),
  numeric = c("source", "table", "column", "stratum_var", "stratum", "n", "share_integer",
              "decimals", .clinical_quantile_columns),
  categorical = c("source", "table", "column", "stratum_var", "stratum", "level", "level_order", "n_rows"),
  date_summary = c("source", "table", "column", "n", .clinical_quantile_columns),
  date_months = c("source", "table", "column", "stratum_var", "stratum", "level", "level_order", "n_rows"),
  id_shapes = c("source", "table", "column", "stratum_var", "stratum", "level", "level_order", "n_rows"),
  correlations = c("source", "scope", "var1", "var2", "rho"),
  omics_features = c("source", "platform", "feature", "share_missing", .clinical_quantile_columns),
  omics_eigen = c("source", "platform", "pc", "eigenvalue"),
  omics_loadings = c("source", "platform", "feature")
)

# Kit columns that hold text even when every value looks like a number
.clinical_text_columns <- c(
  "source", "group", "reader", "status", "message", "modified", "key", "value", "path", "class",
  "table", "visit", "column", "role", "stratum_var", "stratum", "level", "set", "set1", "set2",
  "scope", "var1", "var2", "platform", "feature", "stat", "covariate", "token", "person_key", "kit"
)

#' Read a Clinical Study profile
#'
#' Reads the checked output of reactextract's clinical inventory kit
#' (`clinical-inventory-v1.2` or later): the candidate folder holding
#' `inventory/` and `profile/`. [react_clinical()] and
#' [react_clinical_omics()] generate synthetic Clinical Study data from it.
#'
#' No profile derived from REACT aggregates is bundled with the package yet.
#' Until one is approved and checksum-pinned, the candidate is read from a
#' local folder, by default the one named by the environment variable
#' `REACTEXTRACT_CLINICAL_PROFILE`. Don't copy the candidate into a project
#' or commit it. See `inst/CLINICAL_SYNTHETIC.md`.
#'
#' @param path The candidate folder.
#' @return A `react_clinical_profile` object: a list of the kit's tables, the
#'   anchors, the kit version that wrote the candidate (from
#'   `00_sources.csv`), the folder path and the candidate's manifest
#'   checksums.
#' @export
react_clinical_profile <- function(path = Sys.getenv("REACTEXTRACT_CLINICAL_PROFILE")) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("No approved Clinical Study profile is bundled yet. Set REACTEXTRACT_CLINICAL_PROFILE ",
         "to a released clinical inventory candidate folder (the one holding inventory/ and ",
         "profile/), or pass `path`.", call. = FALSE)
  }
  base <- .clinical_read_candidate(path)
  kit <- if ("kit" %in% names(base$sources)) sort(unique(stats::na.omit(base$sources$kit))) else character()
  version <- if (length(kit) == 0L) "unknown" else paste(kit, collapse = "+")

  context <- base$context
  anchor_rows <- startsWith(context$key, "anchor:")
  anchors <- stats::setNames(context$value[anchor_rows], sub("^anchor:", "", context$key[anchor_rows]))
  structure(
    c(
      list(version = version, status = "local_candidate", path = normalizePath(path), anchors = anchors),
      base
    ),
    class = "react_clinical_profile"
  )
}

.clinical_read_candidate <- function(path) {
  if (!dir.exists(path)) stop("Clinical profile folder not found: ", path, call. = FALSE)
  manifest_path <- file.path(path, "manifest.csv")
  manifest <- data.frame(file = character(), md5 = character(), stringsAsFactors = FALSE)
  if (file.exists(manifest_path)) {
    listed <- .clinical_read_csv(manifest_path)
    if (!all(c("file", "md5") %in% names(listed))) {
      stop("The candidate's manifest.csv needs `file` and `md5` columns.", call. = FALSE)
    }
    observed <- unname(tools::md5sum(file.path(path, listed$file)))
    changed <- listed$file[is.na(observed) | observed != listed$md5]
    if (length(changed) > 0L) {
      stop("Clinical profile file(s) missing or changed since the candidate was written: ",
           paste(changed, collapse = ", "), call. = FALSE)
    }
    manifest <- data.frame(file = file.path(basename(normalizePath(path)), listed$file),
                           md5 = listed$md5, stringsAsFactors = FALSE)
  }
  out <- list(manifest = manifest)
  for (name in names(.clinical_profile_files)) {
    file <- file.path(path, .clinical_profile_files[[name]])
    required <- .clinical_profile_required[[name]]
    if (!file.exists(file)) {
      if (name %in% c("structure", "tables", "columns")) {
        stop("The clinical profile folder has no ", .clinical_profile_files[[name]], ".", call. = FALSE)
      }
      out[[name]] <- .clinical_empty_frame(required)
      next
    }
    data <- .clinical_read_csv(file)
    if (nrow(data) == 0L) data <- .clinical_empty_frame(union(required, names(data)[nzchar(names(data))]))
    missing_cols <- setdiff(required, names(data))
    if (length(missing_cols) > 0L) {
      stop(.clinical_profile_files[[name]], " is missing column(s): ",
           paste(missing_cols, collapse = ", "), call. = FALSE)
    }
    out[[name]] <- data
  }
  out
}

# Every cell is read as text, blanks (suppressed cells) become NA, and columns
# whose values are all numbers or all TRUE/FALSE are converted.
.clinical_read_csv <- function(path) {
  # the kit writes a file with no rows or columns as a single ""
  first <- readLines(path, n = 2L, warn = FALSE)
  if (all(trimws(first) %in% c("", "\"\""))) return(data.frame())
  data <- utils::read.csv(path, colClasses = "character", na.strings = "", check.names = FALSE,
                          stringsAsFactors = FALSE, encoding = "UTF-8")
  for (name in setdiff(names(data), .clinical_text_columns)) {
    x <- data[[name]]
    present <- x[!is.na(x)]
    if (length(present) == 0L) {
      data[[name]] <- as.numeric(x)
    } else if (all(present %in% c("TRUE", "FALSE"))) {
      data[[name]] <- as.logical(x)
    } else {
      number <- suppressWarnings(as.numeric(present))
      if (!anyNA(number)) data[[name]] <- as.numeric(x)
    }
  }
  data
}

.clinical_empty_frame <- function(columns) {
  columns <- columns[nzchar(columns)]
  data <- as.data.frame(stats::setNames(rep(list(character()), length(columns)), columns),
                        stringsAsFactors = FALSE, check.names = FALSE)
  numeric_cols <- setdiff(columns, .clinical_text_columns)
  data[numeric_cols] <- lapply(data[numeric_cols], as.numeric)
  data
}

.clinical_check_profile <- function(profile) {
  if (!inherits(profile, "react_clinical_profile")) {
    stop("`profile` must come from `react_clinical_profile()`.", call. = FALSE)
  }
  profile
}
