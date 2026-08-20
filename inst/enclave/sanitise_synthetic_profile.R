args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop(
    "Usage: Rscript sanitise_synthetic_profile.R INPUT_PROFILE OUTPUT_PROFILE",
    call. = FALSE
  )
}

input <- normalizePath(args[[1L]], mustWork = TRUE)
output <- args[[2L]]
profile <- reactextract::react_read_profile(input)
sanitised <- reactextract::react_sanitise_profile(profile)
reactextract::react_write_profile(sanitised, output)
message(
  "Sanitised profile written to ", normalizePath(output, mustWork = TRUE),
  ". It still requires normal disclosure approval."
)
