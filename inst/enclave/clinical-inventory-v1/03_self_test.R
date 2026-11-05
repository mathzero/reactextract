# Runs the whole kit on invented data in a temporary folder and checks the
# candidate: every count protected, and no invented identifier or planted
# sentinel (free text, rare level, extreme value, one-person month) exported.
# It also checks the v1.1 repairs on the layouts that stopped the first
# enclave run, the v1.2 additions, and checkpoint reuse. Reads no real data.
# Run it locally and once in the enclave before 01.
source("00_load.R")
source("fixtures.R")
root <- file.path(Sys.getenv("CI_SELF_TEST_DIR", tempdir()), paste0("ci-self-test-", format(Sys.time(), "%Y%m%d%H%M%S")))
fx <- ci_fixture_tree(root)
internal <- file.path(root, "INTERNAL")
candidate <- file.path(root, "candidate")
ci_run(fx$saved_root, fx$data_root, fx$project_root, internal = internal, progress = FALSE)
ci_prepare_export(internal, candidate)

xlsx <- requireNamespace("openxlsx", quietly = TRUE)
read <- function(dir, name, base = candidate) {
  path <- file.path(base, dir, name)
  tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
}
src <- read("inventory", "00_sources.csv")
# without a workbook reader the manifests are not read, so RNA-seq samples stay unlinked
expected_missing <- if (xlsx) character() else c("transcriptomics_manifests", "autoantibodies")
expected_partial <- if (xlsx) character() else c("transcriptomics_t0", "transcriptomics_t1")
not_ok <- src$source[src$status != "ok" & !src$source %in% expected_missing &
  !(src$status == "partial" & src$source %in% expected_partial)]
if (length(not_ok)) stop("Sources not profiled: ", paste(not_ok, " (", src$message[match(not_ok, src$source)], ")", collapse = "; "))
issues <- read("inventory", "00_source_issues.csv")
issues <- issues[!(issues$source %in% expected_partial & issues$step == "manifest_map"), , drop = FALSE]
if (nrow(issues)) stop("Steps failed: ", paste(issues$source, issues$step, issues$message, collapse = "; "))

files <- list.files(candidate, pattern = "[.]csv$", recursive = TRUE, full.names = TRUE)
text <- unlist(lapply(files, readLines, warn = FALSE, encoding = "UTF-8"))
tokens <- unique(unlist(strsplit(text, "[,\"]")))
leaked <- intersect(fx$ids, tokens)
if (length(leaked)) stop("Invented identifiers in the candidate: ", paste(utils::head(leaked), collapse = ", "))
prefixed <- grep("(GCCQ|LCRQ|PZ|PX|U25Q|COGQ|LIPQ|AABQ|VITTQ|RNAQ|WQ|LPQ|UPW)[0-9]{4,}", text, value = TRUE)
if (length(prefixed)) stop("Identifier-like values in the candidate: ", utils::head(prefixed, 1))
for (s in ci_fixture_sentinels) if (any(grepl(s, text, fixed = TRUE))) stop("Sentinel exported: ", s)
for (f in files) {
  d <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
  ci_check_counts(d, basename(f))
  if (any(names(d) %in% c("min", "max", "mean", "sd", "skewness", "n_quantile"))) {
    stop("Minimum, maximum, mean, SD or quantile count in ", basename(f))
  }
}

tables <- read("inventory", "03_tables.csv")
nonreact <- tables[grepl("^non_reacters", tables$table), ]
if (!nrow(nonreact) || any(nonreact$profile_level != "structure_only")) stop("Small tables were not limited to structure.")
numeric <- read("profile", "10_numeric.csv")
if (any(grepl("^non_reacters", numeric$table))) stop("Distributions exported for a structure-only table.")
if (!any(numeric$column == "weight" & numeric$stratum_var == "lc_group")) stop("Anchored clinical profiles missing.")
if (!"onset_to_registration" %in% read("profile", "20_timing.csv")$label) stop("Timing summaries missing.")
if (!nrow(read("profile", "18_correlations.csv"))) stop("Correlations missing.")
if (!nrow(read("profile", "15_availability.csv"))) stop("Availability counts missing.")
if (!nrow(read("profile", "19_visit_pairs.csv"))) stop("Visit pairs missing.")

# v1.1: Windows-1252 free text, births, text dates and placeholder dates
columns <- read("inventory", "04_columns.csv")
role <- function(table, column) columns$role[columns$table == table & columns$column == column][1]
if (!identical(role("original_react_data", "add_comm"), "free_text")) stop("Windows-1252 comments not read as free text.")
if (!all(is.finite(columns$n_recoded[columns$column == "add_comm"]))) stop("Recoded text not counted.")
if (identical(role("health_survey/survey_t0", "evbirth"), "withheld")) stop("evbirth withheld as a birth date.")
if (!identical(role("registration_data", "symptoms_date_other"), "date")) stop("Dates held as text not read as dates.")
# v1.2: every date up to 1900 counts as a placeholder and stays out of the quantiles
dates <- read("profile", "12_date_summary.csv")
raw <- dates[dates$column == "react_infection_date_raw", ]
if (!nrow(raw) || !identical(as.numeric(raw$n_placeholder[1]), 40)) stop("Placeholder dates not counted.")
early <- unlist(raw[grep("^p[0-9]{2}$", names(raw))])
if (any(!is.na(early) & nzchar(early) & early < "1900")) stop("Placeholder dates in the date quantiles.")

# v1.1: proteomics layouts, metabolomics names, RNA-seq mapping, new sources
platform <- read("profile", "30_omics_platform.csv")
need <- c("olink", "alamar:npq", "alamar:npq_normalised", "lcms", "lcms_long:value", "lcms_long:value_normalised",
  "allplats:Olink", "allplats:Alamar", "allplats:LC-MS", "metabolomics_rpos", "metabolomics_rneg",
  "metabolomics_xcms", "lipids", "rna_seq")
if (!all(need %in% platform$platform)) stop("Omics platforms missing: ", paste(setdiff(need, platform$platform), collapse = ", "))
features <- read("profile", "31_omics_features.csv")
if (sum(features$platform == "alamar:npq") != 30L) stop("Alamar features not found from protein_cleaned.")
rna <- platform[platform$platform == "rna_seq", ]
if (xlsx && (any(is.na(rna$n_samples_mapped)) || !any(read("profile", "35_omics_r2.csv")$platform == "rna_seq"))) {
  stop("RNA-seq samples not mapped to people through the manifests.")
}
if (!any(tables$source == "apoe4" & tables$person_key == "IID via genomics")) stop("APOE4 not linked through the genotyping wells.")
# v1.2: lookup and QC tables profiled as tables; long tables keep their column order
if (!all(c("prot_names_lookup", "qc_log") %in% tables$table[tables$source == "proteomics"])) {
  stop("Proteomics lookup and QC tables not profiled.")
}
order <- read("inventory", "04b_source_columns.csv")
alamar_cols <- order$column[order$table == "alamar"][order(order$position[order$table == "alamar"])]
if (!identical(alamar_cols, names(readRDS(file.path(fx$saved_root, "ge_lc", "ge_lc_omics_objects",
  "olink_alamar_lcms_combined.rds"))$df_alamar))) stop("Source column order of long omics tables not recorded.")
discovery <- read("inventory", "05_discovery.csv")
collapsed <- discovery[discovery$kind == "files", ]
if (!nrow(collapsed) || !all(startsWith(collapsed$entry, ifelse(collapsed$n_files > 0,
  paste0("<~", collapsed$n_files, " "), "<under 5 ")))) stop("Folder listing labels do not match their counts.")

# XCMS files may instead hold features in rows, with m/z and retention time
xcms_rows <- file.path(root, "xcms_features_in_rows.csv")
xf <- data.frame(mzmed = 100 + seq_len(40) * 2.5, rtmed = 30 + seq_len(40))
for (j in 1:60) xf[[paste0("S", j)]] <- round(exp(stats::rnorm(40, 9)), 1)
utils::write.csv(xf, xcms_rows, row.names = FALSE)
res <- ci_new_result()
ci_read_xcms(res, list(id = "xcms_rows", path = xcms_rows), list())
xr <- ci_finish(res)
if (is.null(xr$omics_platform) || xr$omics_platform$n_samples != 60 || xr$omics_platform$n_features != 40) {
  stop("XCMS features-in-rows layout not read.")
}

# Checkpoints: a failed one, or one from an earlier kit version, is profiled
# again; a finished one is reused; a partial run keeps every other source; an
# export can be limited to some sources
stamp <- function(id) readRDS(file.path(internal, paste0("source_", id, ".rds")))$record$seconds
cp_file <- file.path(internal, "source_lipids.rds")
cp <- readRDS(cp_file)
cp$record$status <- "error"
saveRDS(cp, cp_file)
cp_file <- file.path(internal, "source_apoe4.rds")
cp <- readRDS(cp_file)
cp$kit <- "clinical-inventory-v1.1"
cp$record$seconds <- -1
saveRDS(cp, cp_file)
cp_file <- file.path(internal, "source_cognitron_1.rds")
cp <- readRDS(cp_file)
cp$record$seconds <- -1
saveRDS(cp, cp_file)
again <- ci_run(fx$saved_root, fx$data_root, fx$project_root, internal = internal,
  sources = c("lipids", "proteomics", "apoe4", "cognitron_1"), rerun = "proteomics", progress = FALSE, discover = FALSE)
if (!identical(again$frames$sources$status[again$frames$sources$source == "lipids"], "ok")) stop("A failed checkpoint was reused.")
if (identical(stamp("apoe4"), -1)) stop("A checkpoint from an earlier kit version was reused.")
if (!identical(stamp("cognitron_1"), -1)) stop("A finished checkpoint was profiled again.")
if (!setequal(again$frames$sources$source, src$source)) stop("A partial run dropped other sources.")
some <- c("lipids", "proteomics")
subset_dir <- file.path(root, "subset")
ci_prepare_export(internal, subset_dir, sources = some)
if (!setequal(read("inventory", "00_sources.csv", subset_dir)$source, some)) stop("A limited export holds other sources.")
if (any(!read("profile", "10_numeric.csv", subset_dir)$source %in% some)) stop("Limited export rows from other sources.")
message("Self-test passed: ", length(files), " candidate files, ", nrow(src), " sources",
  if (!xlsx) " (workbook sources skipped: openxlsx not installed)" else "", ". Files in ", root)
