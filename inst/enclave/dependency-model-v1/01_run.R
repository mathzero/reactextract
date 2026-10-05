# First create your usual caller-managed connection `con`. No credentials here.
source("00_load.R")
if (!exists("con")) stop("Create your usual database connection called con first.")
source_data <- reactextract::react_oracle(con)

# Set a non-secret label for the fixed data snapshot. Keep it unchanged when
# resuming that snapshot; use a fresh folder/label if the underlying data change.
if (!exists("dependency_snapshot_label") || !is.character(dependency_snapshot_label) || !nzchar(dependency_snapshot_label))
  stop("Set dependency_snapshot_label in the console first, as shown in README.md. Do not edit checksummed code files.")
dm_run(source_data, source_label = dependency_snapshot_label)
