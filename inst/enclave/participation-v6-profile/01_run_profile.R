# Open your usual read-only Oracle connection as `con`, then source this file.
# No passwords or connection details belong in this script.
source("00_load.R")
if (!exists("con", inherits = TRUE)) stop("Open your usual Oracle connection as con first.")
# For an interruption, rerun against the SAME database snapshot. Completed
# aggregate round checkpoints are reused automatically; the interrupted round
# starts again. For a changed database, choose a NEW checkpoint directory.
conditional_profile <- pc6_run(
  reactextract::react_oracle(con),
  source_label = "participation-v6-capture-2026-09",
  checkpoint = "participation-v6-INTERNAL",
  rounds = "all",
  batch_size = 30L,
  resume = TRUE,
  progress = TRUE
)
message("Complete. Keep participation-v6-INTERNAL inside the enclave. Next run 02_prepare_export.R.")
# The caller owns the connection; this script deliberately does not close it.
