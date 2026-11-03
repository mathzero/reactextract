# v1.1 supplement run. Profiles the nine sources in ci_supplement_sources
# (sources.R): the seven that failed in the first enclave run and the two
# added in v1.1. Every other source is taken from the first run's
# checkpoints in clinical-inventory-INTERNAL, which stays in the enclave.
# No database connection. Paths default to the wrangling pipeline's enclave
# layout; set REACT_SAVED_ROOT, REACT_DATA_ROOT and REACT_GELC_PROJECT (the
# folder holding subject_ids_main_cohort.rds) to change them, and
# REACT_CI_INTERNAL if the first run's INTERNAL folder is not in this folder.
source("00_load.R")
internal <- Sys.getenv("REACT_CI_INTERNAL", "clinical-inventory-INTERNAL")
if (!length(list.files(internal, pattern = "^source_.*[.]rds$"))) {
  message("No checkpoints from the first run in ", internal, ": the supplement will cover its nine sources only.")
}
inventory <- ci_run(
  saved_root = Sys.getenv("REACT_SAVED_ROOT", "D:/saved_objects"),
  data_root = Sys.getenv("REACT_DATA_ROOT", "//se-sandisk-vm01/react_data03$"),
  project_root = Sys.getenv("REACT_GELC_PROJECT", ""),
  internal = internal,
  sources = ci_supplement_sources,
  rerun = ci_supplement_sources
)
done <- inventory$frames$sources
print(done[done$source %in% ci_supplement_sources, c("source", "status", "seconds")], row.names = FALSE)
message("Next: source(\"02_prepare_export.R\"). Never transfer ", internal, ".")
