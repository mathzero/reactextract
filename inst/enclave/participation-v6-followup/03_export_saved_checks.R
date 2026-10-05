# Recovery after an interrupted export. Does not access the database.
source("00_load.R")
sources <- c("INTERNAL_ONLY/reused-aggregate-checks.rds", "INTERNAL_ONLY/targeted-aggregate-checks.rds",
  "INTERNAL_ONLY/targeted-aggregate-checks-v3.rds", "INTERNAL_ONLY/targeted-missing-code-fix-v3.rds")
outputs <- c("react-participation-reexport-v2", "react-participation-targeted-v2",
  "react-participation-targeted-v3", "react-participation-targeted-v3-supplement")
found <- FALSE
for (i in seq_along(sources)) {
  if (!file.exists(sources[i])) next
  found <- TRUE
  if (file.exists(outputs[i])) {
    message("Keeping existing export: ", outputs[i])
    next
  }
  restored <- readRDS(sources[i])
  if (!identical(pf6_meta(restored, "dictionary_manifest_sha256"), pf6_dictionary_hash)) stop("Saved result has the wrong dictionary.")
  pf6_write(pf6_prepare_export(restored), outputs[i])
}
if (!found) stop("No saved follow-up aggregates found. This recovery script cannot restore lost results or unsuppress CSVs.")
