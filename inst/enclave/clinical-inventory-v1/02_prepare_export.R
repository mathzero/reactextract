# No data are read: this uses the saved INTERNAL aggregates only. It writes
# the disclosure-controlled candidate for output checking.
source("00_load.R")
ci_prepare_export("clinical-inventory-INTERNAL", "clinical-inventory-v1-candidate")
message("Request output checking for clinical-inventory-v1-candidate (inventory/ may be released first).")
