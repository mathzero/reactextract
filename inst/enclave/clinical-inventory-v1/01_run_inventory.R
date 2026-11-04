# Full run: profiles every source afresh. Results are checkpointed per source
# in clinical-inventory-INTERNAL, which stays in the enclave. Checkpoints from
# earlier kit versions are never reused; if this run stops, running it again
# resumes after the sources it finished. No database connection. Paths
# default to the wrangling pipeline's enclave layout; set REACT_SAVED_ROOT,
# REACT_DATA_ROOT and REACT_GELC_PROJECT (the folder holding
# subject_ids_main_cohort.rds) to change them.
source("00_load.R")
inventory <- ci_run(
  saved_root = Sys.getenv("REACT_SAVED_ROOT", "D:/saved_objects"),
  data_root = Sys.getenv("REACT_DATA_ROOT", "//se-sandisk-vm01/react_data03$"),
  project_root = Sys.getenv("REACT_GELC_PROJECT", ""),
  internal = Sys.getenv("REACT_CI_INTERNAL", "clinical-inventory-INTERNAL")
)
print(inventory$frames$sources[, c("source", "status", "seconds")], row.names = FALSE)
message("Next: source(\"02_prepare_export.R\"). Never transfer the INTERNAL folder.")
