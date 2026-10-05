# Open your usual connection as con. This script never opens or closes it.
source("00_load.R")
export_path <- "react-participation-targeted-v3"
internal_path <- "INTERNAL_ONLY/targeted-aggregate-checks-v3.rds"
if (file.exists(export_path)) stop("The targeted export already exists. Do not repeat the database run.")
if (file.exists(internal_path)) stop("Saved aggregate checks already exist. Run source('03_export_saved_checks.R') to resume the export without querying again.")
if (!exists("con", inherits = TRUE)) stop("Open your usual enclave database connection as con, then run this script again.")
participation_targeted <- pf6_targeted(reactextract::react_oracle(con), rounds = "all", progress = TRUE)
# Save immediately, BEFORE export preparation. A failed export must not cost a
# second database run. This contains unsuppressed aggregates, never respondent rows.
dir.create("INTERNAL_ONLY", showWarnings = FALSE)
saveRDS(participation_targeted, internal_path)
writeLines("ENCLAVE ONLY. Unsuppressed aggregate results, not an approved export. Never copy this folder outside the enclave.", "INTERNAL_ONLY/DO_NOT_EXPORT.txt")
print(participation_targeted$issues, row.names = FALSE)
participation_targeted_export <- pf6_prepare_export(participation_targeted)
pf6_write(participation_targeted_export, export_path)
message("Step 2 complete. See REVIEWER.md before requesting permission to copy either protected export out.")
