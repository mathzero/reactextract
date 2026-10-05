# Rscript --vanilla scripts/build_participation_followup_kit.R [fresh-output-directory]
# Run at the reactextract repository root. No enclave results are read or copied.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L) stop("Supply at most one new output directory.")
source("R/dictionary.R")
kit <- "inst/enclave/participation-v6-followup"
files <- c("00_load.R", "01_reuse_saved_results.R", "02_run_targeted_checks.R", "03_export_saved_checks.R", "04_check_missing_code_fix.R",
  "followup.R", "diagnostic-v1.R", "README.md", "REVIEWER.md", "LICENSE.md")
if (!all(file.exists(file.path(kit, files)))) stop("Incomplete follow-up source kit.")
output <- if (length(args)) args[1L] else "dist/reactextract-participation-v6-followup-kit-v3"
archive <- paste0(output, ".zip")
if (file.exists(output) || dir.exists(output) || file.exists(archive)) stop("Choose a new output path; existing kits are not overwritten.")
checks <- data.frame(file = files, sha256 = vapply(file.path(kit, files), .sha256_file, character(1)))
utils::write.csv(checks, file.path(kit, "kit-checksums.csv"), row.names = FALSE)
dir.create(output, recursive = TRUE)
if (!all(file.copy(file.path(kit, c(files, "kit-checksums.csv")), output))) stop("Could not copy complete kit.")
output <- normalizePath(output, mustWork = TRUE)
previous <- setwd(dirname(output))
on.exit(setwd(previous), add = TRUE)
status <- utils::zip(paste0(basename(output), ".zip"), file.path(basename(output), c(files, "kit-checksums.csv")))
if (!identical(status, 0L)) stop("Zip creation failed.")
message("Follow-up kit: ", output, ".zip")
