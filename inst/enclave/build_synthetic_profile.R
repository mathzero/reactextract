main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1L || length(args) > 2L) {
    stop(
      "Usage: Rscript build_synthetic_profile.R CONNECTION_SCRIPT [OUTPUT_DIRECTORY]",
      call. = FALSE
    )
  }
  connection_script <- normalizePath(args[[1L]], mustWork = TRUE)
  output <- if (length(args) == 2L) args[[2L]] else "react-synthetic-profile-v1"

  connection_environment <- new.env(parent = globalenv())
  sys.source(connection_script, envir = connection_environment)
  if (!exists("con", envir = connection_environment, inherits = FALSE)) {
    stop("The connection script must create an object named `con`.", call. = FALSE)
  }
  con <- get("con", envir = connection_environment, inherits = FALSE)
  if (!inherits(con, "DBIConnection")) {
    stop("The connection script did not create a DBI connection.", call. = FALSE)
  }
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

  profile <- reactextract::react_profile_source(
    reactextract::react_oracle(con),
    rounds = "all",
    progress = TRUE
  )
  safe_profile <- reactextract::react_prepare_profile_export(profile)
  reactextract::react_write_profile(safe_profile, output)
  message(
    "Profile written to ", normalizePath(output, mustWork = TRUE),
    ". Keep it inside the enclave until normal disclosure review is complete."
  )
  invisible(safe_profile)
}

main()
