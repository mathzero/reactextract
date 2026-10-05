# Run from the unzipped kit directory. No database access in this file.
if (!requireNamespace("reactextract", quietly = TRUE)) stop("Install reactextract from the existing offline bundle first.")
kit_files <- c("00_load.R", "01_reuse_saved_results.R", "02_run_targeted_checks.R",
  "03_export_saved_checks.R", "04_check_missing_code_fix.R", "followup.R", "diagnostic-v1.R", "README.md", "REVIEWER.md", "LICENSE.md")
kit_hashes <- utils::read.csv("kit-checksums.csv", colClasses = "character", na.strings = NULL)
if (!identical(names(kit_hashes), c("file", "sha256")) || anyDuplicated(kit_hashes$file) ||
    !setequal(kit_hashes$file, kit_files)) stop("Incomplete kit checksum list. Copy the full unzipped kit.")
for (i in seq_len(nrow(kit_hashes))) {
  if (!file.exists(kit_hashes$file[i]) || !identical(getFromNamespace(".sha256_file", "reactextract")(kit_hashes$file[i]), kit_hashes$sha256[i]))
    stop("Kit checksum mismatch: ", kit_hashes$file[i], ". Copy the full unedited kit again.")
}
source("diagnostic-v1.R")
source("followup.R")
invisible(pf6_check_dictionary())
