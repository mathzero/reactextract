# No database access. Requires the ORIGINAL unsuppressed aggregate object in R.
source("00_load.R")
if (!exists("participation_diagnostic", inherits = TRUE)) stop(
  "The original participation_diagnostic object is not in this R session. Restore it from an enclave-only RDS/workspace if saved. Do not read the exported CSVs as a substitute. If it is lost, skip Step 1 and run Step 2; do not repeat the full run yet.")
export_path <- "react-participation-reexport-v2"
internal_path <- "INTERNAL_ONLY/reused-aggregate-checks.rds"
if (file.exists(export_path) || file.exists(internal_path)) stop("Step 1 results already exist. Keep them; see README for resuming exports without querying again.")
participation_reused <- pf6_reuse(participation_diagnostic)
dir.create("INTERNAL_ONLY", showWarnings = FALSE)
saveRDS(participation_reused, internal_path)
writeLines("ENCLAVE ONLY. Unsuppressed aggregate results, not an approved export. Never copy this folder outside the enclave.", "INTERNAL_ONLY/DO_NOT_EXPORT.txt")
participation_reexport <- pf6_prepare_export(participation_reused)
pf6_write(participation_reexport, export_path)
message("Step 1 complete: no database queries were made. Do not export INTERNAL_ONLY or the original R object.")
