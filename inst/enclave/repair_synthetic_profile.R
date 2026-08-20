main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 3L) {
    stop(
      paste(
        "Usage: Rscript repair_synthetic_profile.R CONNECTION_SCRIPT",
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
  specs <- dictionary$synthetic_profile_specs
  occurrences <- dictionary$occurrences

  date_ids <- specs$occurrence_id[
    specs$review_state == "approved" &
      specs$profile_kind == "date" &
      specs$generation_action == "empirical"
  ]
  repair_codes <- c(
    "field_query_failed", "field_presence_query_failed"
  )
  issue_rows <- existing$issues[existing$issues$code %in% repair_codes, , drop = FALSE]
  issue_key <- paste(issue_rows$round_id, issue_rows$variable, sep = "\r")
  occurrence_key <- paste(occurrences$round_id, occurrences$variable, sep = "\r")
  issue_ids <- occurrences$occurrence_id[occurrence_key %in% issue_key]
  changed_ids <- character()
  if (!identical(
    unname(reactextract::react_dictionary_version()$manifest_sha256),
    unname(existing$metadata$value[
      existing$metadata$key == "dictionary_manifest_sha256"
    ])
  )) {
    old_specs <- existing$profile_specs
    current_specs <- specs[specs$review_state == "approved", , drop = FALSE]
    if (!setequal(old_specs$occurrence_id, current_specs$occurrence_id)) {
      stop(
        "The new dictionary changed the included occurrence set; run a complete profile instead.",
        call. = FALSE
      )
    }
    old_specs <- old_specs[
      match(current_specs$occurrence_id, old_specs$occurrence_id), , drop = FALSE
    ]
    core <- c(
      "profile_kind", "generation_action", "support_source", "bin_spec_id",
      "routing_rule_id"
    )
    changed <- Reduce(`|`, lapply(core, function(column) {
      old_specs[[column]] != current_specs[[column]]
    }))
    safe_changed <- changed &
      current_specs$profile_kind != "identifier" &
      !(current_specs$generation_action %in% c("synthetic_identifier", "excluded"))
    changed_ids <- current_specs$occurrence_id[safe_changed]
  }
  repair_ids <- unique(c(date_ids, issue_ids, changed_ids))
  repair_ids <- repair_ids[repair_ids %in% specs$occurrence_id[specs$review_state == "approved"]]
  repair_rounds <- unique(occurrences$round_id[match(repair_ids, occurrences$occurrence_id)])

  message(
    "[reactextract] Repairing ", length(repair_ids), " exact fields across ",
    length(repair_rounds), " rounds. The existing routing results will be retained.",
    if (length(changed_ids)) {
      paste0(
        " ", length(changed_ids), " fields use the corrected profiling policy; ",
        "all empirical date fields also receive the administrative-code fix."
      )
    } else ""
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
    "Repaired profile written to ", normalizePath(output, mustWork = TRUE),
    ". It still requires normal disclosure approval."
  )
  invisible(repaired)
}

main()
