main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 3L) {
    stop(
      paste(
        "Usage: Rscript reprofile_categorical_fields.R CONNECTION_SCRIPT",
        "EXISTING_PROFILE OUTPUT_DIRECTORY"
      ),
      call. = FALSE
    )
  }
  connection_script <- normalizePath(args[[1L]], mustWork = TRUE)
  existing_path <- normalizePath(args[[2L]], mustWork = TRUE)
  output <- args[[3L]]

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

  existing <- reactextract::react_read_profile(existing_path)
  dictionary <- reactextract::react_dictionary()
  current_specs <- dictionary$synthetic_profile_specs[
    dictionary$synthetic_profile_specs$review_state == "approved",
    , drop = FALSE
  ]
  old_specs <- existing$profile_specs
  if (!setequal(old_specs$occurrence_id, current_specs$occurrence_id)) {
    stop("The existing profile and current dictionary contain different fields.", call. = FALSE)
  }
  old_specs <- old_specs[
    match(current_specs$occurrence_id, old_specs$occurrence_id),
    , drop = FALSE
  ]
  core <- c(
    "profile_kind", "generation_action", "support_source", "bin_spec_id",
    "routing_rule_id"
  )
  changed <- Reduce(`|`, lapply(core, function(column) {
    old_specs[[column]] != current_specs[[column]]
  }))
  repair_ids <- current_specs$occurrence_id[changed]
  if (length(repair_ids) != 168L ||
      !all(current_specs$profile_kind[changed] == "categorical") ||
      !all(
        current_specs$support_source[changed] ==
          "inferred_public_response_domain"
      )) {
    stop(
      "Expected exactly 168 approved categorical corrections; refusing a different repair scope.",
      call. = FALSE
    )
  }
  occurrences <- dictionary$occurrences[
    dictionary$occurrences$occurrence_id %in% repair_ids,
    , drop = FALSE
  ]
  repair_rounds <- dictionary$rounds$round_id[
    dictionary$rounds$round_id %in% occurrences$round_id
  ]

  message(
    "[reactextract] Re-profiling 168 categorical fields across ",
    length(repair_rounds), " rounds."
  )
  repair <- reactextract::react_profile_source(
    reactextract::react_oracle(con),
    rounds = repair_rounds,
    occurrence_ids = repair_ids,
    include_routing = FALSE,
    include_overall = FALSE,
    progress = TRUE
  )
  repaired <- reactextract::react_repair_profile(existing, repair)
  reactextract::react_write_profile(repaired, output)
  message(
    "Corrected profile written to ", normalizePath(output, mustWork = TRUE),
    ". It still requires normal disclosure approval."
  )
  invisible(repaired)
}

main()
