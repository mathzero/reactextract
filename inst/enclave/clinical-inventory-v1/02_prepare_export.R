# No data are read: this uses the saved INTERNAL aggregates only. It writes
# the disclosure-controlled v1.1 supplement for output checking: the nine
# sources profiled by 01_run_inventory.R, which replace those sources in the
# first candidate.
source("00_load.R")
ci_prepare_export(Sys.getenv("REACT_CI_INTERNAL", "clinical-inventory-INTERNAL"),
  "clinical-inventory-v1.1-supplement-candidate", sources = ci_supplement_sources)
message("Request output checking for clinical-inventory-v1.1-supplement-candidate (inventory/ may be released first).")
