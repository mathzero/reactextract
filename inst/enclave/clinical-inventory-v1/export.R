# Disclosure controls for the clinical inventory. ci_prepare_export() reads the
# INTERNAL result and writes the candidate proposed for output checking, in two
# folders so the checker can release the inventory before the profile:
#   inventory/  sources, structure, tables, columns, folder listing, environment
#   profile/    distributions, correlations, timing, availability, omics
# Rules (react-hub docs/conventions.md, as in reactextract's earlier kits):
# - counts below 10 are suppressed, zeros included; where one hidden cell could
#   be recovered from its margin a second cell is hidden; other counts are
#   rounded to 5;
# - shares are blanked when the count they imply (either side) is 1 to 9;
# - no minima, maxima, means or SDs (one extreme value can dominate a mean or
#   SD); a quantile needs 30 values and 10 on each side, and the 10% trimmed
#   mean needs 100 values;
# - category labels appear only when at least 10 people share them; free text,
#   identifiers and withheld columns never contribute values;
# - tables with fewer than 50 people (or rows) are exported as structure only.
# These controls prepare a candidate; they are not output-checking approval.

ci_round <- function(n) ci_round_base * round(as.numeric(n) / ci_round_base)

ci_protect <- function(n) {
  n <- as.numeric(n)
  out <- ci_round(n)
  out[is.na(n) | n < ci_min_cell] <- NA
  out
}

ci_protect_share <- function(share, denominator, digits = 3L) {
  share <- as.numeric(share)
  denominator <- as.numeric(denominator)
  k <- round(share * denominator)
  bad <- (k > 0 & k < ci_min_cell) | (denominator - k > 0 & denominator - k < ci_min_cell) |
    !is.finite(denominator) | denominator < ci_min_cell
  out <- round(share, digits)
  out[is.na(bad) | bad] <- NA
  out
}

# quantiles, means and shape statistics for frames built by ci_numeric_row()
ci_protect_numeric <- function(f, n_col = "n", digits = 3L) {
  if (is.null(f) || !nrow(f)) return(f)
  n <- as.numeric(f[[n_col]])
  enough <- !is.na(n) & n >= ci_min_numeric_n
  for (q in grep("^p[0-9]{2}$", names(f), value = TRUE)) {
    p <- as.numeric(substring(q, 2L)) / 100
    ok <- enough & n * p >= ci_min_cell & n * (1 - p) >= ci_min_cell
    v <- as.numeric(f[[q]])
    f[[q]] <- ifelse(ok, if (is.na(digits)) round(v) else signif(v, digits), NA)
  }
  # a 10% trimmed mean ignores the 10 or more most extreme values at each end
  if ("mean_trimmed" %in% names(f)) {
    f$mean_trimmed <- ifelse(!is.na(n) & n * 0.1 >= ci_min_cell, signif(as.numeric(f$mean_trimmed), 4L), NA)
  }
  f <- f[setdiff(names(f), c("mean", "sd", "skewness", "mean_t", "sd_t", "skewness_t"))]
  if ("decimals" %in% names(f)) f$decimals <- ifelse(enough, f$decimals, NA)
  for (col in intersect(c("share_integer", "share_zero", "share_at_min", "share_same"), names(f))) {
    f[[col]] <- ci_protect_share(f[[col]], n)
  }
  for (col in c(n_col, grep("^n_(zero|negative|code_)", names(f), value = TRUE))) f[[col]] <- ci_protect(f[[col]])
  f
}

# Hide one more cell in any partition with exactly one hidden cell, until stable.
ci_complement <- function(supp, value, groups) {
  groups <- groups[lengths(groups) > 1L]
  repeat {
    hidden <- vapply(groups, function(g) sum(supp[g]), numeric(1))
    changed <- FALSE
    for (g in groups[hidden == 1]) {
      if (sum(supp[g]) != 1) next
      cand <- g[!supp[g]]
      if (!length(cand)) next
      supp[cand[which.min(value[cand])]] <- TRUE
      changed <- TRUE
    }
    if (!changed) break
  }
  supp
}

# Categorical levels, months, ID shapes, availability and patterns. Partitions
# are the levels within one stratum, and each level across one anchor's strata
# together with its overall count.
ci_protect_categorical <- function(f) {
  if (is.null(f) || !nrow(f)) return(f)
  base <- ifelse(is.na(f$n_persons), f$n_rows, f$n_persons)
  primary <- is.na(base) | base < ci_min_cell
  part <- paste(f$source, f$table, f$column, f$kind, f$stratum_var, f$stratum, sep = "\r")
  groups <- split(seq_len(nrow(f)), part)
  lk <- paste(f$source, f$table, f$column, f$kind, f$level, sep = "\r")
  all_rows <- which(f$stratum_var == "all")
  for (a in setdiff(unique(f$stratum_var), "all")) {
    ia <- which(f$stratum_var == a)
    idx <- c(ia, all_rows[lk[all_rows] %in% lk[ia]])
    groups <- c(groups, split(idx, lk[idx]))
  }
  supp <- ci_complement(primary, base, unname(groups))
  # a label is shown only if its overall count reaches the threshold
  hide <- lk %in% lk[f$stratum_var == "all" & primary] | (f$stratum_var != "all" & !lk %in% lk[all_rows])
  f$level[hide] <- "<suppressed>"
  f$n_rows <- ifelse(supp, NA, ci_round(f$n_rows))
  f$n_persons <- ifelse(supp | is.na(f$n_persons), NA, ci_round(f$n_persons))
  f[!hide, , drop = FALSE]
}

ci_drop_structure_only <- function(f, small, table_col = "table") {
  if (is.null(f) || !nrow(f) || !length(small)) return(f)
  f[!paste(f$source, f[[table_col]], sep = "\r") %in% small, , drop = FALSE]
}

ci_write <- function(f, dir, name) {
  if (is.null(f)) f <- data.frame()
  path <- file.path(dir, name)
  utils::write.csv(f, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  ci_frame(file = file.path(basename(dir), name), rows = nrow(f), md5 = unname(tools::md5sum(path)))
}

# final guard: every count column holds blanks or multiples of 5 of at least 10
ci_check_counts <- function(f, name, exempt = character()) {
  if (is.null(f) || !nrow(f)) return(invisible(TRUE))
  cols <- grep("^(n|n_.*|rpp_.*)$", names(f), value = TRUE)
  # n_distinct is protected by role above: exact only for up to 50 category levels
  cols <- setdiff(cols, c("n_cols", "n_features", "n_features_profiled", "n_features_source", "n_files", "n_distinct", exempt))
  for (col in cols) {
    v <- suppressWarnings(as.numeric(f[[col]]))
    bad <- !is.na(v) & (v < ci_min_cell | v %% ci_round_base != 0)
    if (any(bad)) stop("Unprotected count in ", name, ": ", col)
  }
  invisible(TRUE)
}

# Keep the rows of the named sources. Run-level frames (context, environment,
# folder listing) are kept; provenance only with its own source.
ci_select_sources <- function(fr, sources) {
  unknown <- setdiff(sources, fr$sources$source)
  if (length(unknown)) stop("Not in the INTERNAL result: ", paste(unknown, collapse = ", "))
  out <- lapply(names(fr), function(k) {
    f <- fr[[k]]
    if (is.data.frame(f) && "source" %in% names(f)) return(f[f$source %in% sources, , drop = FALSE])
    if (k == "provenance" && !"provenance" %in% sources) return(NULL)
    f
  })
  stats::setNames(out, names(fr))
}

#' Write the disclosure-controlled candidate from an INTERNAL inventory.
#' With `sources`, the candidate holds only those sources (a supplement to an
#' earlier candidate), with the run's context, environment and folder listing.
ci_prepare_export <- function(internal = Sys.getenv("REACT_CI_INTERNAL", "clinical-inventory-INTERNAL"),
                              output = "clinical-inventory-v1.2-candidate", sources = NULL) {
  x <- readRDS(file.path(internal, "clinical-inventory-INTERNAL.rds"))
  if (!identical(x$version, ci_version)) stop("The INTERNAL result is from a different kit version.")
  if (dir.exists(output) && length(list.files(output, all.files = TRUE, no.. = TRUE))) {
    stop("Choose a new, empty output folder; candidates are never overwritten.")
  }
  inv_dir <- file.path(output, "inventory")
  pro_dir <- file.path(output, "profile")
  dir.create(inv_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(pro_dir, recursive = TRUE, showWarnings = FALSE)
  fr <- x$frames
  if (!is.null(sources)) fr <- ci_select_sources(fr, sources)
  get <- function(k) fr[[k]]

  tables <- get("tables")
  small <- if (is.null(tables)) character() else
    paste(tables$source, tables$table, sep = "\r")[tables$profile_level != "full"]
  if (!is.null(tables)) {
    for (col in c("n_rows", "n_rows_source", "n_persons", "n_persons_main", "n_rows_no_person",
      grep("^rpp_", names(tables), value = TRUE))) tables[[col]] <- ci_protect(tables[[col]])
  }
  columns <- get("columns")
  if (!is.null(columns)) {
    levels_ok <- columns$role %in% c("categorical", "coded_numeric") & columns$n_distinct <= ci_max_levels
    columns$n_distinct <- ifelse(levels_ok, columns$n_distinct, ci_protect(columns$n_distinct))
    columns$n_nonmissing <- ci_protect(columns$n_nonmissing)
    columns$n_missing <- ci_protect(columns$n_missing)
    if ("n_recoded" %in% names(columns)) columns$n_recoded <- ci_protect(columns$n_recoded)
  }
  structure <- get("structure")
  if (!is.null(structure)) structure$n_rows <- ci_protect(structure$n_rows)
  discovery <- get("discovery")
  if (!is.null(discovery)) {
    discovery$folder <- ci_mask_name(discovery$folder)
    discovery$entry <- ci_mask_name(discovery$entry)
    collapsed <- discovery$kind == "files"
    discovery$n_files[collapsed] <- ci_round(discovery$n_files[collapsed])
    approx <- ifelse(discovery$n_files[collapsed] > 0, paste0("~", discovery$n_files[collapsed]), "under 5")
    discovery$entry[collapsed] <- paste0("<", approx, " ", sub("^<[0-9]+ ", "", discovery$entry[collapsed]))
  }
  id_sets <- get("id_sets")
  if (!is.null(id_sets)) { id_sets$n <- ci_protect(id_sets$n); id_sets$n_main <- ci_protect(id_sets$n_main) }
  id_overlaps <- get("id_overlaps")
  if (!is.null(id_overlaps)) id_overlaps$n_both <- ci_protect(id_overlaps$n_both)

  numeric <- ci_protect_numeric(ci_drop_structure_only(get("numeric"), small))
  categorical <- ci_protect_categorical(ci_drop_structure_only(get("categorical"), small))
  date_summary <- ci_drop_structure_only(get("date_summary"), small)
  if (!is.null(date_summary) && nrow(date_summary)) {
    # quantiles rest on the dates that are not placeholders (v1 rows: all dates)
    n <- if ("n_quantile" %in% names(date_summary)) ifelse(is.na(date_summary$n_quantile), date_summary$n,
      date_summary$n_quantile) else date_summary$n
    for (q in grep("^p[0-9]{2}$", names(date_summary), value = TRUE)) {
      p <- as.numeric(substring(q, 2L)) / 100
      ok <- n >= ci_min_numeric_n & n * p >= ci_min_cell & n * (1 - p) >= ci_min_cell
      date_summary[[q]] <- ifelse(ok, format(as.Date(date_summary[[q]]), "%Y-%m"), NA)
    }
    date_summary$n_quantile <- NULL
    for (col in intersect(c("n", "n_placeholder", "n_before_2019_12", "n_after_run_date"), names(date_summary))) {
      date_summary[[col]] <- ci_protect(date_summary[[col]])
    }
  }
  correlations <- get("correlations")
  if (!is.null(correlations) && nrow(correlations)) {
    correlations <- correlations[!paste(correlations$source, correlations$scope, sep = "\r") %in% small &
      correlations$n >= ci_min_cor_n, , drop = FALSE]
    correlations$rho <- round(correlations$rho, 2L)
    correlations$n <- ci_protect(correlations$n)
  }
  visit_pairs <- get("visit_pairs")
  if (!is.null(visit_pairs) && nrow(visit_pairs)) {
    visit_pairs$rho <- round(visit_pairs$rho, 2L)
    visit_pairs <- ci_protect_numeric(visit_pairs)
  }
  timing <- ci_protect_numeric(get("timing"), digits = NA)
  availability_pairs <- get("availability_pairs")
  if (!is.null(availability_pairs)) availability_pairs$n_both <- ci_protect(availability_pairs$n_both)

  platform <- get("omics_platform")
  notes <- get("omics_platform_notes")
  if (!is.null(platform) && nrow(platform)) {
    if (!is.null(notes)) platform <- merge(platform, notes, by = c("source", "platform"), all.x = TRUE, sort = FALSE)
    cells <- platform$n_samples_profiled * platform$n_features
    platform$share_missing <- ci_protect_share(platform$share_missing, cells)
    platform$share_nonpositive <- ci_protect_share(platform$share_nonpositive, cells)
    for (col in c("n_samples_source", "n_samples", "n_samples_mapped", "n_samples_profiled", "n_persons",
      "n_persons_main", "n_samples_t0", "n_samples_t1", "n_samples_visit_unknown", "n_duplicate_cells")) {
      if (col %in% names(platform)) platform[[col]] <- ci_protect(platform[[col]])
    }
  }
  features <- get("omics_features")
  if (!is.null(features) && nrow(features)) {
    features$share_missing <- ci_protect_share(features$share_missing, features$n_total)
    features <- ci_protect_numeric(features)
    features$n_total <- ci_protect(features$n_total)
  }
  omics_samples <- ci_protect_numeric(get("omics_samples"))
  eigen <- get("omics_eigen")
  if (!is.null(eigen) && nrow(eigen)) {
    eigen$eigenvalue <- signif(eigen$eigenvalue, 4L)
    eigen$share <- round(eigen$share, 4L)
    eigen$cumulative <- round(eigen$cumulative, 4L)
    eigen$n_samples <- ci_protect(eigen$n_samples)
  }
  loadings <- get("omics_loadings")
  if (!is.null(loadings)) for (col in grep("^pc[0-9]+$", names(loadings), value = TRUE)) loadings[[col]] <- round(loadings[[col]], 3L)
  r2 <- get("omics_r2")
  if (!is.null(r2) && nrow(r2)) {
    r2 <- r2[r2$n >= ci_min_cor_n, , drop = FALSE]
    r2$r2 <- round(r2$r2, 3L)
    r2$n <- ci_protect(r2$n)
  }
  omics_visit <- get("omics_visit")
  if (!is.null(omics_visit) && nrow(omics_visit)) {
    omics_visit <- omics_visit[omics_visit$n >= ci_min_numeric_n, , drop = FALSE]
    omics_visit$rho <- round(omics_visit$rho, 2L)
    omics_visit$n <- ci_protect(omics_visit$n)
  }
  tokens <- get("omics_tokens")
  if (!is.null(tokens) && nrow(tokens)) {
    tokens <- stats::aggregate(n ~ source + platform + token, data = tokens, FUN = sum)
    tokens <- tokens[tokens$n >= ci_min_cell & nchar(tokens$token) <= 30L & !grepl("[0-9]{5,}", tokens$token), , drop = FALSE]
    tokens$n <- ci_protect(tokens$n)
  }

  pick <- function(kind) if (is.null(categorical)) NULL else
    categorical[categorical$kind == kind, setdiff(names(categorical), "kind"), drop = FALSE]
  inventory <- list(
    "00_sources.csv" = get("sources"), "00_source_issues.csv" = if (is.null(get("issues"))) ci_frame(source = character(), step = character(),
      message = character()) else get("issues"), "01_context.csv" = get("context"), "02_structure.csv" = structure,
    "03_tables.csv" = tables, "04_columns.csv" = columns, "04b_source_columns.csv" = get("source_columns"),
    "05_discovery.csv" = discovery,
    "06_environment.csv" = get("environment"), "07_provenance.csv" = get("provenance"),
    "08_id_sets.csv" = id_sets, "09_id_overlaps.csv" = id_overlaps)
  profile <- list(
    "10_numeric.csv" = numeric, "11_categorical.csv" = pick("level"), "12_date_summary.csv" = date_summary,
    "13_date_months.csv" = pick("month"), "14_id_shapes.csv" = pick("shape"),
    "15_availability.csv" = pick("availability"), "16_availability_patterns.csv" = pick("pattern"),
    "17_availability_pairs.csv" = availability_pairs, "18_correlations.csv" = correlations,
    "19_visit_pairs.csv" = visit_pairs, "20_timing.csv" = timing,
    "30_omics_platform.csv" = platform, "31_omics_features.csv" = features, "32_omics_samples.csv" = omics_samples,
    "33_omics_eigen.csv" = eigen, "34_omics_loadings.csv" = loadings, "35_omics_r2.csv" = r2,
    "36_omics_visit.csv" = omics_visit, "37_omics_tokens.csv" = tokens)
  for (name in names(inventory)) ci_check_counts(inventory[[name]], name)
  for (name in names(profile)) ci_check_counts(profile[[name]], name)
  manifest <- ci_bind(c(lapply(names(inventory), function(n) ci_write(inventory[[n]], inv_dir, n)),
    lapply(names(profile), function(n) ci_write(profile[[n]], pro_dir, n))))
  utils::write.csv(manifest, file.path(output, "manifest.csv"), row.names = FALSE)
  scope <- if (is.null(sources)) "All sources." else c(
    paste0("Supplement: only these sources: ", paste(sources, collapse = ", "), "."),
    "Their rows replace those of the same sources in the earlier candidate; other sources are unchanged.")
  writeLines(c(paste0("Clinical cohort inventory candidate (", ci_version, ")"),
    paste0("INTERNAL run created: ", x$created, "; candidate written: ", format(Sys.time(), "%Y-%m-%d %H:%M")),
    scope, "", "Blank count cells are suppressed (fewer than 10, or hidden so that a suppressed cell cannot be recovered).",
    "Counts are rounded to 5. Quantiles need 30 values and 10 on each side. No minima, maxima, means or SDs.",
    "Category labels appear only when shared by at least 10 people.",
    "inventory/ can be reviewed and released on its own; profile/ holds the distributions.",
    "These controls prepare a candidate; they are not output-checking approval. See REVIEWER.md in the kit."),
    file.path(output, "README.txt"))
  message("Candidate written to ", output, ". Request output checking before transferring it.")
  invisible(manifest)
}
