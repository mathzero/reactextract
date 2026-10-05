# Set previous_output and output to enclave folder paths, and create con first.
if (!exists("con") || !exists("previous_output") || !exists("output"))
  stop("Create con and set previous_output and output before sourcing this script.")
source("00_load.R")
original_settings <- readRDS(file.path(previous_output, "configuration.rds"))
dm_run(reactextract::react_oracle(con),
  source_label = original_settings$source_label,
  output = output, previous_output = previous_output,
  rounds = original_settings$rounds, batch_size = original_settings$batch_size,
  seed = original_settings$seed, synthetic_n = original_settings$synthetic_n)
