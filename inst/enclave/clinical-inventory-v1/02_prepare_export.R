# No data are read: this uses the saved INTERNAL aggregates only. It writes
# the disclosure-controlled candidate for output checking, covering every
# source. It stops if any source was profiled by an earlier kit version, so
# the candidate comes from one run of this kit.
source("00_load.R")
internal <- Sys.getenv("REACT_CI_INTERNAL", "clinical-inventory-INTERNAL")
done <- readRDS(file.path(internal, "clinical-inventory-INTERNAL.rds"))$frames$sources
old <- done$source[done$kit != ci_version]
if (length(old)) stop("Profiled by an earlier kit version: ", paste(old, collapse = ", "),
  ". Run 01_run_inventory.R to completion first.")
ci_prepare_export(internal, "clinical-inventory-v1.2-candidate")
message("Request output checking for clinical-inventory-v1.2-candidate (inventory/ may be released first).")
