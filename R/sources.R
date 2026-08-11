.validate_registry <- function(registry) {
  required <- c(
    "round_id", "object_name", "observation_key", "passcode_field",
    "crosswalk_object", "crosswalk_passcode_field", "crosswalk_subject_field"
  )
  if (!is.data.frame(registry) || !identical(names(registry), required)) {
    stop("The source registry does not match the reactextract registry schema.", call. = FALSE)
  }
  if (nrow(registry) != 25L || anyDuplicated(registry$round_id)) {
    stop("The source registry must contain one row for each of the 25 rounds.", call. = FALSE)
  }
  identifiers <- unlist(registry[setdiff(required, "round_id")], use.names = FALSE)
  if (any(!grepl("^[A-Z][A-Z0-9_.]*$", identifiers))) {
    stop("The source registry contains an unsafe database identifier.", call. = FALSE)
  }
  registry
}

.resolve_round_name <- function(name, rounds) {
  if (name %in% rounds$round_id) {
    return(name)
  }
  match_index <- match(name, rounds$survey_id)
  if (!is.na(match_index)) {
    return(rounds$round_id[[match_index]])
  }
  NA_character_
}

.discover_round_files <- function(path, rounds) {
  candidates <- list.files(
    path,
    pattern = "[.](rds|RDS|csv|CSV|parquet)$",
    full.names = TRUE
  )
  if (length(candidates) == 0L) {
    return(stats::setNames(character(), character()))
  }
  stems <- tools::file_path_sans_ext(basename(candidates))
  resolved <- vapply(stems, .resolve_round_name, character(1), rounds = rounds)
  keep <- !is.na(resolved)
  candidates <- candidates[keep]
  resolved <- resolved[keep]
  if (anyDuplicated(resolved)) {
    stop("More than one round file resolves to the same round ID.", call. = FALSE)
  }
  stats::setNames(candidates, resolved)
}

.normalise_file_map <- function(path, rounds) {
  if (is.list(path) && !is.data.frame(path)) {
    if (is.null(names(path)) || any(!nzchar(names(path)))) {
      stop("An in-memory round list must be named with round or survey IDs.", call. = FALSE)
    }
    resolved <- vapply(names(path), .resolve_round_name, character(1), rounds = rounds)
    if (anyNA(resolved) || anyDuplicated(resolved)) {
      stop("The in-memory round list contains an unknown or duplicate round.", call. = FALSE)
    }
    names(path) <- resolved
    return(path)
  }
  if (is.character(path) && length(path) == 1L && dir.exists(path)) {
    return(.discover_round_files(path, rounds))
  }
  if (is.character(path) && !is.null(names(path)) && all(nzchar(names(path)))) {
    resolved <- vapply(names(path), .resolve_round_name, character(1), rounds = rounds)
    if (anyNA(resolved) || anyDuplicated(resolved)) {
      stop("The file map contains an unknown or duplicate round.", call. = FALSE)
    }
    names(path) <- resolved
    return(path)
  }
  stop(
    "`path` must be a directory, a named vector of round files, or a named list of data frames.",
    call. = FALSE
  )
}

#' Create a REACT round-file source
#'
#' @param path A directory, a named vector of file paths, or a named list of
#'   round data frames. Names may be round IDs or survey IDs.
#' @param crosswalk Optional crosswalk data frame or RDS/CSV/Parquet path.
#' @return A source object for [react_extract()].
#' @export
react_files <- function(path, crosswalk = NULL) {
  rounds <- react_dictionary()$rounds
  structure(
    list(
      kind = "files",
      rounds = .normalise_file_map(path, rounds),
      crosswalk = crosswalk
    ),
    class = c("react_file_source", "react_source")
  )
}

.new_oracle_source <- function(connection, registry, batch_size, query_fn) {
  structure(
    list(
      kind = "oracle",
      connection = connection,
      registry = .validate_registry(registry),
      batch_size = batch_size,
      query_fn = query_fn
    ),
    class = c("react_oracle_source", "react_source")
  )
}

#' Create a caller-managed Oracle source
#'
#' @param connection An open DBI connection. The caller remains responsible for
#'   authentication and disconnection.
#' @param registry Optional 25-row source registry overriding the bundled object
#'   names.
#' @param batch_size Maximum number of requested variables per keyed query.
#' @return A source object for [react_extract()].
#' @export
react_oracle <- function(connection, registry = NULL, batch_size = 200L) {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Oracle extraction requires the optional `DBI` package.", call. = FALSE)
  }
  if (!inherits(connection, "DBIConnection")) {
    stop("`connection` must be an open DBI connection.", call. = FALSE)
  }
  batch_size <- as.integer(batch_size)
  if (length(batch_size) != 1L || is.na(batch_size) || batch_size < 1L) {
    stop("`batch_size` must be one positive integer.", call. = FALSE)
  }
  if (is.null(registry)) {
    registry <- react_dictionary()$source_registry
  }
  .new_oracle_source(
    connection,
    registry,
    batch_size,
    function(con, sql) DBI::dbGetQuery(con, sql)
  )
}

.read_data_file <- function(path) {
  extension <- tolower(tools::file_ext(path))
  if (extension == "rds") {
    data <- readRDS(path)
  } else if (extension == "csv") {
    data <- utils::read.csv(
      path,
      check.names = FALSE,
      stringsAsFactors = FALSE,
      na.strings = character(),
      strip.white = FALSE
    )
  } else if (extension == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Parquet input requires the optional `arrow` package.", call. = FALSE)
    }
    data <- as.data.frame(arrow::read_parquet(path), stringsAsFactors = FALSE)
  } else {
    stop("Unsupported round-file extension: ", extension, ".", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("Round files must contain a data frame.", call. = FALSE)
  }
  data
}

.read_crosswalk_source <- function(crosswalk) {
  if (is.null(crosswalk)) {
    return(NULL)
  }
  if (is.data.frame(crosswalk)) {
    return(crosswalk)
  }
  if (is.character(crosswalk) && length(crosswalk) == 1L && file.exists(crosswalk)) {
    return(.read_data_file(crosswalk))
  }
  stop("`crosswalk` must be a data frame or an existing RDS/CSV/Parquet file.", call. = FALSE)
}
