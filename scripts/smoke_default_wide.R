#!/usr/bin/env Rscript

# Full-size public-data smoke test for the normal researcher workflow.
suppressPackageStartupMessages(library(reactextract))

started <- proc.time()[["elapsed"]]
result <- react_extract(
  react_synthetic(n_per_round = 1000L, seed = 1L),
  output = "wide",
  progress = TRUE
)

stopifnot(
  identical(names(result), c(
    "data", "raw_data", "observations", "column_dictionary", "issues", "manifest"
  )),
  nrow(result$data) == 25000L,
  nrow(result$raw_data) == 25000L,
  nrow(result$observations) == 25000L,
  !"raw_values" %in% names(result),
  !"harmonised_values" %in% names(result)
)

print(result)
message(sprintf(
  "Default 25-round wide smoke test passed in %.1f seconds.",
  proc.time()[["elapsed"]] - started
))
