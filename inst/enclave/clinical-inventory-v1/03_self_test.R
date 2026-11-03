# Runs the whole kit on invented data in a temporary folder and checks the
# candidate: every count protected, and no invented identifier or planted
# sentinel (free text, rare level, extreme value, one-person month) exported.
# Reads no real data. Run it locally and once in the enclave before 01.
source("00_load.R")
source("fixtures.R")
root <- file.path(Sys.getenv("CI_SELF_TEST_DIR", tempdir()), paste0("ci-self-test-", format(Sys.time(), "%Y%m%d%H%M%S")))
fx <- ci_fixture_tree(root)
internal <- file.path(root, "INTERNAL")
candidate <- file.path(root, "candidate")
ci_run(fx$saved_root, fx$data_root, fx$project_root, internal = internal, progress = FALSE)
ci_prepare_export(internal, candidate)

xlsx <- requireNamespace("openxlsx", quietly = TRUE)
src <- utils::read.csv(file.path(candidate, "inventory", "00_sources.csv"), stringsAsFactors = FALSE)
expected_missing <- if (xlsx) character() else c("transcriptomics_manifests", "autoantibodies")
not_ok <- src$source[src$status != "ok" & !src$source %in% expected_missing]
if (length(not_ok)) stop("Sources not profiled: ", paste(not_ok, " (", src$message[match(not_ok, src$source)], ")", collapse = "; "))

files <- list.files(candidate, pattern = "[.]csv$", recursive = TRUE, full.names = TRUE)
text <- unlist(lapply(files, readLines, warn = FALSE, encoding = "UTF-8"))
tokens <- unique(unlist(strsplit(text, "[,\"]")))
leaked <- intersect(fx$ids, tokens)
if (length(leaked)) stop("Invented identifiers in the candidate: ", paste(utils::head(leaked), collapse = ", "))
prefixed <- grep("(GCCQ|LCRQ|PZ|PX|U25Q|COGQ|LIPQ|AABQ|VITTQ|WQ|LPQ|UPW)[0-9]{4,}", text, value = TRUE)
if (length(prefixed)) stop("Identifier-like values in the candidate: ", utils::head(prefixed, 1))
for (s in ci_fixture_sentinels) if (any(grepl(s, text, fixed = TRUE))) stop("Sentinel exported: ", s)
for (f in files) {
  d <- utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  ci_check_counts(d, basename(f))
  if (any(names(d) %in% c("min", "max", "mean", "sd", "skewness"))) stop("Minimum, maximum, mean or SD in ", basename(f))
}

read <- function(dir, name) utils::read.csv(file.path(candidate, dir, name), stringsAsFactors = FALSE, check.names = FALSE)
tables <- read("inventory", "03_tables.csv")
nonreact <- tables[grepl("^non_reacters", tables$table), ]
if (!nrow(nonreact) || any(nonreact$profile_level != "structure_only")) stop("Small tables were not limited to structure.")
numeric <- read("profile", "10_numeric.csv")
if (any(grepl("^non_reacters", numeric$table))) stop("Distributions exported for a structure-only table.")
if (!any(numeric$column == "weight" & numeric$stratum_var == "lc_group")) stop("Anchored clinical profiles missing.")
platforms <- read("profile", "30_omics_platform.csv")$platform
if (!all(c("olink", "alamar", "lcms", "metabolomics_rpos", "lipids", "rna_seq") %in% platforms)) stop("Omics platforms missing.")
if (!"onset_to_registration" %in% read("profile", "20_timing.csv")$label) stop("Timing summaries missing.")
if (!nrow(read("profile", "18_correlations.csv"))) stop("Correlations missing.")
if (!nrow(read("profile", "15_availability.csv"))) stop("Availability counts missing.")
message("Self-test passed: ", length(files), " candidate files, ", nrow(src), " sources",
  if (!xlsx) " (workbook sources skipped: openxlsx not installed)" else "", ". Files in ", root)
