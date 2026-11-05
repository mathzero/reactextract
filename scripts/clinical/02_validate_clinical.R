# 02_validate_clinical.R --------------------------------------------------------
# Compare synthetic Clinical Study data with what the built profile holds, and
# write scripts/clinical/VALIDATION.md.
#
# VALIDATION.md quotes figures from the profile, so, like the profile, it is
# committed only once the profile is approved for release.
#
# Run from the package root after 01_build_clinical_profile.R:
#   Rscript --no-init-file scripts/clinical/02_validate_clinical.R

rm(list = ls(all = TRUE))
suppressMessages(pkgload::load_all(".", quiet = TRUE))
profile_path <- "inst/extdata/clinical-profile.rds"
out_path <- "scripts/clinical/VALIDATION.md"
if (!identical(.sha256_file(profile_path), .clinical_profile_sha256)) {
  stop("Pin the built profile's SHA-256 in R/clinical_profile.R first.", call. = FALSE)
}
profile <- readRDS(profile_path)
seed <- 20261105L
n_omics <- 3000L

source <- react_synthetic(n_per_round = 1000L, seed = seed)
timing <- system.time(gelc <- react_clinical(source, profile = profile))
omics_timing <- system.time(omics <- react_clinical_omics(source, profile = profile, n = n_omics,
                                                          sources = setdiff(.clinical_omics_sources(profile),
                                                                            c("transcriptomics_t0", "transcriptomics_t1"))))
leaves <- .clinical_flatten(gelc)
tables <- leaves[vapply(leaves, is.data.frame, logical(1L))]
wrangled <- function(data) data[data$source == "wrangled", , drop = FALSE]
fmt <- function(x, digits = 3) formatC(x, digits = digits, format = "f")
lines <- character()
add <- function(...) lines <<- c(lines, paste0(...))


# 1. Structure ------------------------------------------------------------------------
columns <- wrangled(profile$columns)
schema <- .clinical_tables(profile)
structure_rows <- do.call(rbind, lapply(names(tables), function(path) {
  data <- tables[[path]]
  expected <- columns[columns$table == path, ]
  expected <- expected[order(expected$position), ]
  classes <- unname(vapply(data, function(x) class(x)[[1L]], character(1L)))
  s <- schema[schema$table == path, ]
  data.frame(table = path, rows = nrow(data), profile_rows = s$n_rows, columns = ncol(data),
             names_ok = identical(names(data), expected$column), classes_ok = identical(classes, expected$class),
             stringsAsFactors = FALSE)
}))


# 2. Categorical shares (overall, among answered rows) ----------------------------------
categorical <- wrangled(profile$categorical)
categorical <- categorical[categorical$stratum_var == "all" & categorical$n_rows > 0, ]
tvd <- do.call(rbind, lapply(split(categorical, paste(categorical$table, categorical$column, sep = "\r")), function(part) {
  data <- tables[[part$table[[1L]]]]
  if (is.null(data) || !part$column[[1L]] %in% names(data)) return(NULL)
  value <- as.character(data[[part$column[[1L]]]])
  value <- value[!is.na(value)]
  if (length(value) < 50L) return(NULL)
  expected <- part$n_rows / sum(part$n_rows)
  observed <- as.numeric(table(factor(value, levels = part$level))) / length(value)
  data.frame(table = part$table[[1L]], column = part$column[[1L]], tvd = sum(abs(observed - expected)) / 2,
             outside = mean(!value %in% part$level), stringsAsFactors = FALSE)
}))


# 3. Numeric quantiles (scaled by the profile's p10-p90 range) --------------------------
numeric <- wrangled(profile$numeric)
numeric <- numeric[numeric$stratum_var == "all" & !is.na(numeric$p10) & !is.na(numeric$p90) & numeric$p90 > numeric$p10, ]
quantile_rows <- do.call(rbind, lapply(seq_len(nrow(numeric)), function(i) {
  row <- numeric[i, ]
  data <- tables[[row$table]]
  role <- columns$role[columns$table == row$table & columns$column == row$column]
  if (is.null(data) || !row$column %in% names(data) || !identical(role, "continuous")) return(NULL)
  value <- as.numeric(data[[row$column]])
  value <- value[!is.na(value) & !value %in% .clinical_missing_codes]
  if (length(value) < 50L) return(NULL)
  q <- stats::quantile(value, c(0.1, 0.5, 0.9), names = FALSE)
  data.frame(table = row$table, column = row$column,
             error = max(abs(q - c(row$p10, row$p50, row$p90))) / (row$p90 - row$p10), stringsAsFactors = FALSE)
}))


# 4. Missingness -------------------------------------------------------------------------
missing_rows <- do.call(rbind, lapply(seq_len(nrow(columns)), function(i) {
  row <- columns[i, ]
  data <- tables[[row$table]]
  total <- sum(c(row$n_nonmissing, row$n_missing), na.rm = TRUE)
  if (is.null(data) || nrow(data) < 50L || total == 0 || row$role %in% c("withheld", "identifier")) return(NULL)
  expected <- if (is.na(row$n_missing)) 0 else row$n_missing / total
  data.frame(table = row$table, column = row$column, difference = abs(mean(is.na(data[[row$column]])) - expected),
             stringsAsFactors = FALSE)
}))


# 5. Rank correlations within tables, between visits, and timing --------------------------
correlations <- wrangled(profile$correlations)
set.seed(seed)
sampled <- correlations[sample.int(nrow(correlations), min(2000L, nrow(correlations))), ]
corr_rows <- do.call(rbind, lapply(seq_len(nrow(sampled)), function(i) {
  row <- sampled[i, ]
  data <- tables[[row$scope]]
  if (is.null(data) || !all(c(row$var1, row$var2) %in% names(data))) return(NULL)
  a <- suppressWarnings(as.numeric(data[[row$var1]]))
  b <- suppressWarnings(as.numeric(data[[row$var2]]))
  ok <- !is.na(a) & !is.na(b) & !a %in% .clinical_missing_codes & !b %in% .clinical_missing_codes
  if (sum(ok) < 50L || stats::sd(a[ok]) == 0 || stats::sd(b[ok]) == 0) return(NULL)
  data.frame(profile = row$rho, synthetic = stats::cor(a[ok], b[ok], method = "spearman"))
}))

pairs <- .clinical_visit_pairs(profile, schema)
visit_rows <- do.call(rbind, lapply(names(pairs), function(path) {
  later <- tables[[path]]
  earlier <- tables[[pairs[[path]]$t0]]
  if (is.null(later) || is.null(earlier)) return(NULL)
  both <- merge(as.data.frame(earlier), as.data.frame(later), by = "subject_id", suffixes = c(".t0", ".t1"))
  do.call(rbind, lapply(names(pairs[[path]]$rho), function(column) {
    a <- suppressWarnings(as.numeric(both[[paste0(column, ".t0")]]))
    b <- suppressWarnings(as.numeric(both[[paste0(column, ".t1")]]))
    ok <- !is.na(a) & !is.na(b) & !a %in% .clinical_missing_codes & !b %in% .clinical_missing_codes
    if (sum(ok) < 50L || stats::sd(a[ok]) == 0 || stats::sd(b[ok]) == 0) return(NULL)
    data.frame(table = path, column = column, profile = pairs[[path]]$rho[[column]],
               synthetic = stats::cor(a[ok], b[ok], method = "spearman"))
  }))
}))

timing_rows <- do.call(rbind, lapply(seq_len(nrow(profile$timing)), function(i) {
  row <- profile$timing[i, ]
  resolve <- function(key) {
    table <- sub(":[^:]*$", "", key)
    column <- sub("^.*:", "", key)
    if (!table %in% names(tables)) {
      main <- names(tables)[names(tables) %in% schema$table[schema$branch == ""]]
      holders <- main[vapply(tables[main], function(d) column %in% names(d), logical(1L))]
      if (length(holders) == 0L) return(NULL)
      table <- holders[[1L]]
    }
    data <- as.data.frame(tables[[table]])
    data.frame(subject_id = data$subject_id, date = as.Date(data[[column]]))
  }
  from <- resolve(row$from)
  to <- resolve(row$to)
  if (is.null(from) || is.null(to)) return(NULL)
  both <- merge(from[!duplicated(from$subject_id), ], to[!duplicated(to$subject_id), ], by = "subject_id")
  real <- !is.na(both$date.x) & !is.na(both$date.y) & both$date.x > as.Date("1900-01-01") &
    both$date.y > as.Date("1900-01-01") & both$date.x < as.Date("9999-01-01") & both$date.y < as.Date("9999-01-01")
  days <- as.numeric(both$date.y - both$date.x)[real]
  if (length(days) < 50L) return(NULL)
  data.frame(label = row$label, profile = paste(row$p10, row$p50, row$p90, sep = " / "),
             synthetic = paste(stats::quantile(days, c(0.1, 0.5, 0.9), names = FALSE, type = 1), collapse = " / "))
}))


# 6. Omics: first component's share and feature medians ----------------------------------
omics_rows <- do.call(rbind, lapply(seq_len(nrow(profile$omics_platform)), function(i) {
  platform <- profile$omics_platform[i, ]
  if (platform$layout != "wide" || is.null(omics[[platform$source]])) return(NULL)
  data <- omics[[platform$source]]
  if (!is.data.frame(data)) data <- data[[which(endsWith(tolower(names(data)), tolower(sub(":.*", "", platform$platform))))[1L]]]
  features <- profile$omics_features[profile$omics_features$source == platform$source &
                                       profile$omics_features$platform == platform$platform, ]
  features <- features[features$feature %in% names(data) & !is.na(features$p50), ]
  if (nrow(features) < 3L) return(NULL)
  values <- as.matrix(as.data.frame(data)[features$feature])
  scores <- apply(values, 2L, function(x) stats::qnorm((rank(x, na.last = "keep") - 0.5) / sum(!is.na(x))))
  scores[is.na(scores)] <- 0
  scores <- scores[, apply(scores, 2L, stats::sd) > 0, drop = FALSE]
  if (ncol(scores) < 3L) return(NULL)
  share <- prop.table(eigen(stats::cor(scores), symmetric = TRUE, only.values = TRUE)$values)[[1L]]
  eigen_rows <- profile$omics_eigen[profile$omics_eigen$source == platform$source &
                                      profile$omics_eigen$platform == platform$platform, ]
  expected <- if (nrow(eigen_rows)) eigen_rows$eigenvalue[eigen_rows$pc == 1L] / nrow(features) else NA
  medians <- apply(values, 2L, stats::median, na.rm = TRUE)
  spread <- features$p90 - features$p10
  error <- abs(medians - features$p50) / ifelse(spread > 0, spread, NA)
  data.frame(platform = platform$platform, samples = nrow(values), features = nrow(features),
             pc1_profile = expected, pc1_synthetic = share, median_error = stats::median(error, na.rm = TRUE))
}))


# 7. Write ---------------------------------------------------------------------------------
add("# Synthetic Clinical Study validation")
add("")
add("Generated ", format(Sys.Date(), "%d %B %Y"), " with reactextract ", as.character(utils::packageVersion("reactextract")),
    " (", profile$version, ", built from ", profile$kit, ") for the synthetic people of `react_synthetic(n_per_round = 1000L, seed = ",
    seed, ")`. The wrangled list (", length(tables), " tables) took ", round(timing[["elapsed"]]), " seconds; the omics files (n = ",
    n_omics, ", RNA-seq left out) ", round(omics_timing[["elapsed"]]), " seconds.")
add("")
add("Every figure compares synthetic output with what the profile itself holds. **Synthetic data are for writing code, not ",
    "for findings.**")
add("")
add("## Structure")
add("")
add("- Tables whose column names and order match the inventory: ", sum(structure_rows$names_ok), " of ", nrow(structure_rows), ".")
add("- Tables whose column classes match: ", sum(structure_rows$classes_ok), " of ", nrow(structure_rows), ".")
off <- structure_rows[!is.na(structure_rows$profile_rows) & structure_rows$profile_rows > 0 &
                        abs(structure_rows$rows / structure_rows$profile_rows - 1) > 0.05, ]
add("- Tables whose row count differs from the profile's by more than 5%: ", nrow(off),
    if (nrow(off)) paste0(" (", paste(off$table, collapse = ", "), ": population tables cover the synthetic population, ",
                          "and tables with more people than registered are capped)") else "", ".")
add("")
add("## Distributions")
add("")
add("| Measure | Columns | Median | 90th percentile | Share above 0.10 |")
add("|---|---|---|---|---|")
add("| Categorical: total variation distance from the profile's shares | ", nrow(tvd), " | ", fmt(stats::median(tvd$tvd)),
    " | ", fmt(stats::quantile(tvd$tvd, 0.9)), " | ", fmt(mean(tvd$tvd > 0.1)), " |")
add("| Continuous: largest p10/p50/p90 error, as a share of the profile's p10-p90 range | ", nrow(quantile_rows), " | ",
    fmt(stats::median(quantile_rows$error)), " | ", fmt(stats::quantile(quantile_rows$error, 0.9)), " | ",
    fmt(mean(quantile_rows$error > 0.1)), " |")
add("| Missingness: absolute difference in share missing | ", nrow(missing_rows), " | ", fmt(stats::median(missing_rows$difference)),
    " | ", fmt(stats::quantile(missing_rows$difference, 0.9)), " | ", fmt(mean(missing_rows$difference > 0.1)), " |")
add("")
add("Categorical values outside the profile's levels: ", fmt(mean(tvd$outside), 4), " of answered values (anchor columns ",
    "hold the person's anchor label).")
add("")
population <- schema$table[schema$population]
others <- tvd[!tvd$table %in% population, ]
worst <- utils::head(others[order(-others$tvd), ], 8)
add("Largest categorical differences outside population tables: ",
    paste(sprintf("`%s$%s` (%s)", worst$table, worst$column, fmt(worst$tvd, 2)), collapse = ", "),
    ". Anchored columns move with the cohort's anchor mix, and columns with few answers are noisy. Population tables (",
    paste(population, collapse = ", "), ") cover the whole synthetic population, in which the cohort is a far larger ",
    "share than in REACT, so their overall shares lean towards registrants'.")
add("")
add("## Dependencies")
add("")
add("| Measure | Pairs | Mean absolute difference | Correlation with the profile |")
add("|---|---|---|---|")
add("| Within-table rank correlations (sample of ", nrow(sampled), ") | ", nrow(corr_rows), " | ",
    fmt(mean(abs(corr_rows$synthetic - corr_rows$profile))), " | ", fmt(stats::cor(corr_rows$synthetic, corr_rows$profile)), " |")
if (!is.null(visit_rows) && nrow(visit_rows) > 0L) {
  add("| Baseline to follow-up rank correlations | ", nrow(visit_rows), " | ",
      fmt(mean(abs(visit_rows$synthetic - visit_rows$profile))), " | ", fmt(stats::cor(visit_rows$synthetic, visit_rows$profile)), " |")
}
add("")
if (!is.null(timing_rows) && nrow(timing_rows) > 0L) {
  add("Intervals between dates in days (p10 / p50 / p90):")
  add("")
  add("| Interval | Profile | Synthetic |")
  add("|---|---|---|")
  for (i in seq_len(nrow(timing_rows))) add("| ", timing_rows$label[[i]], " | ", timing_rows$profile[[i]], " | ", timing_rows$synthetic[[i]], " |")
  add("")
}
if (!is.null(omics_rows) && nrow(omics_rows) > 0L) {
  add("## Omics (wide tables)")
  add("")
  add("| Platform | Samples | Features | First component's share: profile | synthetic | Median feature median error |")
  add("|---|---|---|---|---|---|")
  for (i in seq_len(nrow(omics_rows))) {
    r <- omics_rows[i, ]
    add("| ", r$platform, " | ", r$samples, " | ", r$features, " | ", fmt(r$pc1_profile), " | ", fmt(r$pc1_synthetic), " | ",
        fmt(r$median_error), " |")
  }
  add("")
  add("Feature median error is the absolute difference from the profile's median, as a share of its p10-p90 range.")
}
writeLines(lines, out_path)
message("Wrote ", out_path)
