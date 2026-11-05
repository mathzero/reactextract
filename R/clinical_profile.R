# Clinical Study profile: a checked inventory-kit candidate ----------------------

# The candidate's files, by the name used in the profile object. Only the
# structure files are required; files that are absent or empty give empty
# tables, so candidates from earlier kit versions still read.
.clinical_profile_files <- c(
  source_status = "inventory/00_sources.csv",
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
  source_status = c("source", "status"),
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
#' Returns the profile that [react_clinical()] and [react_clinical_omics()]
#' generate synthetic Clinical Study data from: by default the profile bundled
#' with the package (`clinical-profile-v1`), built from the checked output of
#' reactextract's clinical inventory kit. The bundled profile is used only
#' when its approval record says it is formally approved for release and its
#' SHA-256 matches the one pinned in the package.
#'
#' For development, `path` (by default the environment variable
#' `REACTEXTRACT_CLINICAL_PROFILE`) can instead name a local candidate folder
#' written by the kit (`clinical-inventory-v1.2` or later, holding
#' `inventory/` and `profile/`). Don't copy a candidate into a project or
#' commit it. See `inst/CLINICAL_SYNTHETIC.md`.
#'
#' @param path A local candidate folder, or `""` for the bundled profile.
#' @param refresh Re-read and re-verify the bundled profile.
#' @return A `react_clinical_profile` object: the generator's tables, the
#'   anchors, the profile and kit versions, and the input files' checksums
#'   (`sources`).
#' @export
react_clinical_profile <- function(path = Sys.getenv("REACTEXTRACT_CLINICAL_PROFILE"), refresh = FALSE) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    return(.clinical_bundled_profile(refresh))
  }
  base <- .clinical_read_candidate(path)
  status <- base$source_status
  kit <- if ("kit" %in% names(status)) sort(unique(stats::na.omit(status$kit))) else character()
  kit <- if (length(kit) == 0L) "unknown" else paste(kit, collapse = "+")

  context <- base$context
  anchor_rows <- startsWith(context$key, "anchor:")
  anchors <- stats::setNames(context$value[anchor_rows], sub("^anchor:", "", context$key[anchor_rows]))
  structure(
    c(
      list(version = kit, kit = kit, status = "local_candidate", path = normalizePath(path), anchors = anchors),
      base
    ),
    class = "react_clinical_profile"
  )
}

# Checksum of the bundled Clinical Study profile; its approval is recorded in
# inst/extdata/clinical-profile-approval.csv
.clinical_profile_sha256 <- "ac78e27a67c33630d14c28a1f5e29ae94efb18ced711af0821db45233d03eaf0"

# The bundled profile, used only when it is present, its checksum matches the
# pinned one and its approval record says formally_approved
.clinical_bundled_profile <- function(refresh = FALSE,
                                      path = system.file("extdata", "clinical-profile.rds", package = "reactextract"),
                                      approval_path = system.file("extdata", "clinical-profile-approval.csv",
                                                                  package = "reactextract"),
                                      pinned = .clinical_profile_sha256) {
  if (!isTRUE(refresh) && !is.null(.reactextract_env$clinical_profile)) {
    return(.reactextract_env$clinical_profile)
  }
  development <- paste0("For development, set REACTEXTRACT_CLINICAL_PROFILE to a local clinical ",
                        "inventory candidate folder, or pass `path`.")
  if (!nzchar(path)) {
    stop("No Clinical Study profile is bundled with this version of reactextract. ", development, call. = FALSE)
  }
  observed <- .sha256_file(path)
  if (!identical(observed, pinned)) {
    stop("Clinical Study profile checksum mismatch. Expected ", pinned, "; observed ", observed, ". ",
         development, call. = FALSE)
  }
  approval <- if (nzchar(approval_path)) .read_literal_csv(approval_path) else data.frame()
  if (nrow(approval) != 1L || !all(c("profile_sha256", "approval_status") %in% names(approval)) ||
      approval$profile_sha256[[1L]] != pinned) {
    stop("The bundled Clinical Study profile's approval record is invalid.", call. = FALSE)
  }
  if (approval$approval_status[[1L]] != "formally_approved") {
    stop("The bundled Clinical Study profile isn't approved for release yet (status: ",
         approval$approval_status[[1L]], "). ", development, call. = FALSE)
  }
  .reactextract_env$clinical_profile <- readRDS(path)
  .reactextract_env$clinical_profile
}

# The release profile built from a candidate (scripts/clinical/): only what
# the generator reads. Kept are the wrangled list, the omics sources the
# generator makes, and, from combined_data_set.rds, the anchors' counts and
# the availability patterns. Columns the generator doesn't read are dropped,
# suppressed level counts become zero (they are never reconstructed), and the
# kit's folder listing, environment, provenance, ID sets, source issues,
# per-flag availability, component R2 and tokens are left out. `sources`
# records the candidate's files and checksums.
.clinical_profile_release <- function(profile, version, sources) {
  .clinical_check_profile(profile)
  generated <- c(.clinical_release_source, .clinical_omics_sources(profile))
  anchor_source <- unique(.clinical_anchor_rows(profile, unname(profile$anchors))$source)
  q <- .clinical_quantile_columns
  pick <- function(data, columns, keep = data$source %in% generated) {
    data <- data[keep, intersect(columns, names(data)), drop = FALSE]
    rownames(data) <- NULL
    data
  }
  levels <- function(data, keep = data$source %in% generated) {
    data <- pick(data, c("source", "table", "column", "stratum_var", "stratum", "level", "level_order", "n_rows"),
                 keep)
    data$n_rows[is.na(data$n_rows)] <- 0
    data
  }
  codes <- paste0("n_code_", sub("-", "m", .clinical_missing_codes))
  categorical <- profile$categorical
  anchor_rows <- categorical$source %in% anchor_source & categorical$column %in% profile$anchors
  patterns <- profile$availability_patterns
  out <- list(
    version = version,
    kit = profile$kit,
    status = "release",
    built = format(Sys.Date()),
    sources = sources,
    anchors = profile$anchors,
    source_status = pick(profile$source_status, c("source", "group", "status", "kit")),
    structure = pick(profile$structure, c("source", "path", "class", "n_rows", "n_cols")),
    tables = pick(profile$tables, c("source", "table", "visit", "n_rows", "n_cols", "has_persons", "person_level",
                                    "n_persons", "n_persons_main", "profile_level", "rpp_1", "rpp_2", "rpp_3_5",
                                    "rpp_6_10", "rpp_11_plus")),
    columns = pick(profile$columns, c("source", "table", "position", "column", "class", "role", "n_nonmissing",
                                      "n_missing", "n_distinct")),
    source_columns = pick(profile$source_columns, c("source", "table", "position", "column", "class")),
    numeric = pick(profile$numeric, c("source", "table", "column", "stratum_var", "stratum", "n",
                                      "share_integer", "decimals", q, codes),
                   profile$numeric$source %in% generated & profile$numeric$column != ".rows_per_person"),
    categorical = levels(categorical, (categorical$source %in% generated & !categorical$source %in% anchor_source) |
                           anchor_rows),
    availability_patterns = levels(patterns, startsWith(patterns$table, "availability_patterns:")),
    date_summary = pick(profile$date_summary, c("source", "table", "column", "n", "n_placeholder", q)),
    date_months = levels(profile$date_months),
    id_shapes = levels(profile$id_shapes),
    correlations = pick(profile$correlations, c("source", "scope", "var1", "var2", "rho")),
    visit_pairs = pick(profile$visit_pairs, c("source", "family", "column", "rho")),
    timing = pick(profile$timing, c("source", "label", "from", "to", q)),
    omics_platform = pick(profile$omics_platform, c("source", "platform", "n_samples_source", "n_samples_profiled",
                                                    "n_samples_t0", "n_samples_t1", "n_features", "transform",
                                                    "layout", "value_column", "feature_column", "sample_column",
                                                    "split")),
    omics_features = pick(profile$omics_features, c("source", "platform", "feature", "share_missing", q)),
    omics_samples = pick(profile$omics_samples, c("source", "platform", "stat", q),
                         profile$omics_samples$source %in% generated & profile$omics_samples$stat == "library_size"),
    omics_eigen = pick(profile$omics_eigen, c("source", "platform", "pc", "eigenvalue")),
    omics_loadings = pick(profile$omics_loadings, c("source", "platform", "feature", paste0("pc", 1:10))),
    omics_visit = pick(profile$omics_visit, c("source", "platform", "feature", "rho"))
  )
  structure(out, class = "react_clinical_profile")
}

.clinical_read_candidate <- function(path) {
  if (!dir.exists(path)) stop("Clinical profile folder not found: ", path, call. = FALSE)
  manifest_path <- file.path(path, "manifest.csv")
  manifest <- data.frame(file = character(), origin = character(), md5 = character(), stringsAsFactors = FALSE)
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
    manifest <- data.frame(file = listed$file, origin = "clinical inventory candidate (local only)",
                           md5 = listed$md5, stringsAsFactors = FALSE)
  }
  out <- list(sources = manifest)
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

# Replace the levels of categorical columns with neutral labels, from a
# format such as "Site %02d", wherever the columns appear (every table and
# stratum). Levels are numbered by descending overall count, then by their
# total across strata, so the neutral labels keep no order from the original
# ones; the original labels appear nowhere in the result. Used by the build
# for labels the disclosure review asked to coarsen.
.clinical_neutral_levels <- function(profile, formats) {
  rows <- profile$categorical
  key <- paste(rows$source, rows$table, rows$column, sep = "\r")
  for (column in names(formats)) {
    for (k in unique(key[rows$column == column])) {
      at <- key == k
      overall <- rows[at & rows$stratum_var == "all", , drop = FALSE]
      levels <- unique(rows$level[at])
      count <- overall$n_rows[match(levels, overall$level)]
      count[is.na(count)] <- 0
      total <- vapply(levels, function(l) sum(rows$n_rows[at & rows$level == l], na.rm = TRUE), numeric(1L))
      ranked <- levels[order(-count, -total)]
      neutral <- sprintf(formats[[column]], seq_along(ranked))
      rows$level[at] <- neutral[match(rows$level[at], ranked)]
      rows$level_order[at] <- match(rows$level[at], neutral)
    }
  }
  profile$categorical <- rows
  profile
}

.clinical_check_profile <- function(profile) {
  if (!inherits(profile, "react_clinical_profile")) {
    stop("`profile` must come from `react_clinical_profile()`.", call. = FALSE)
  }
  profile
}
