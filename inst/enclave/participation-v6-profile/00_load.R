# Run from the kit directory. This verifies files; it does not query the database.
if (!requireNamespace("reactextract", quietly = TRUE) || !requireNamespace("digest", quietly = TRUE)) stop("Install reactextract and digest from the existing offline bundle first.")
kit_files <- c("00_load.R", "01_run_profile.R", "02_prepare_export.R", "profile.R", "followup-policy.R",
  "README.md", "REVIEWER.md", "LICENSE.md", "contract/checksums.csv")
hashes <- utils::read.csv("kit-checksums.csv", colClasses = "character", na.strings = character())
if (!identical(names(hashes), c("file", "sha256")) || anyDuplicated(hashes$file) || !setequal(hashes$file, kit_files)) stop("Incomplete kit checksums. Copy the complete unedited kit.")
for (i in seq_len(nrow(hashes))) if (!file.exists(hashes$file[i]) ||
    !identical(getFromNamespace(".sha256_file", "reactextract")(hashes$file[i]), hashes$sha256[i])) stop("Kit checksum mismatch: ", hashes$file[i])
source("followup-policy.R")
source("profile.R")
invisible(pc6_contract("contract"))
message("Kit verified. The installed v5 package and generator have not been changed.")
