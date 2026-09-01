.dictionary_manifest_sha256 <- "28d03054e4b284cd44a040cf473991c441184739e84a7f6235392b1142a79236"
.dictionary_archive_sha256 <- "e3c3d44f30e71bf0203cd2edf9c73e4fd47240e486f25622b657ca7884873233"

.sha256_file <- function(path) {
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(path, algo = "sha256", serialize = FALSE, file = TRUE))
  }
  if (nzchar(Sys.which("sha256sum"))) {
    output <- system2("sha256sum", path, stdout = TRUE, stderr = TRUE)
    return(strsplit(output[[1]], "[[:space:]]+")[[1]][[1]])
  }
  if (nzchar(Sys.which("shasum"))) {
    output <- system2("shasum", c("-a", "256", path), stdout = TRUE, stderr = TRUE)
    return(strsplit(output[[1]], "[[:space:]]+")[[1]][[1]])
  }
  if (.Platform$OS.type == "windows" && nzchar(Sys.which("certutil"))) {
    output <- system2(
      "certutil",
      c("-hashfile", shQuote(path), "SHA256"),
      stdout = TRUE,
      stderr = TRUE
    )
    candidate <- gsub("[[:space:]]", "", output)
    candidate <- candidate[grepl("^[0-9A-Fa-f]{64}$", candidate)]
    if (length(candidate) == 1L) {
      return(tolower(candidate))
    }
  }
  stop(
    "Cannot verify the bundled dictionary: install `digest` or provide a system SHA-256 utility.",
    call. = FALSE
  )
}

.read_literal_csv <- function(path) {
  utils::read.csv(
    path,
    header = TRUE,
    quote = "\"",
    sep = ",",
    colClasses = "character",
    na.strings = character(),
    strip.white = FALSE,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8"
  )
}

.dictionary_directory <- function() {
  override <- getOption("reactextract.dictionary_path", NULL)
  if (!is.null(override)) {
    return(normalizePath(override, mustWork = TRUE))
  }
  archive <- system.file("extdata", "dictionary.tar.gz", package = "reactextract")
  if (!nzchar(archive)) {
    stop("The installed reactextract dictionary bundle is missing.", call. = FALSE)
  }
  archive_hash <- .sha256_file(archive)
  if (!identical(archive_hash, .dictionary_archive_sha256)) {
    stop(
      "Dictionary archive SHA-256 mismatch. Expected ",
      .dictionary_archive_sha256,
      "; observed ",
      archive_hash,
      ".",
      call. = FALSE
    )
  }
  path <- file.path(tempdir(), paste0("reactextract-dictionary-", archive_hash))
  if (!file.exists(file.path(path, "manifest.csv"))) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    utils::untar(archive, exdir = path)
  }
  normalizePath(path, mustWork = TRUE)
}

.verify_dictionary_bundle <- function(path) {
  manifest_path <- file.path(path, "manifest.csv")
  if (!file.exists(manifest_path)) {
    stop("The bundled dictionary manifest is missing.", call. = FALSE)
  }
  observed_manifest <- .sha256_file(manifest_path)
  if (!identical(observed_manifest, .dictionary_manifest_sha256)) {
    stop(
      "Dictionary manifest SHA-256 mismatch. Expected ",
      .dictionary_manifest_sha256,
      "; observed ",
      observed_manifest,
      ".",
      call. = FALSE
    )
  }
  manifest <- .read_literal_csv(manifest_path)
  for (i in seq_len(nrow(manifest))) {
    file_path <- file.path(path, manifest$file[[i]])
    if (!file.exists(file_path)) {
      stop("Dictionary bundle file is missing: ", manifest$file[[i]], ".", call. = FALSE)
    }
    observed_size <- as.character(file.info(file_path)$size)
    if (!identical(observed_size, manifest$byte_size[[i]])) {
      stop("Dictionary bundle byte-size mismatch for ", manifest$file[[i]], ".", call. = FALSE)
    }
    observed_hash <- .sha256_file(file_path)
    if (!identical(observed_hash, manifest$sha256[[i]])) {
      stop("Dictionary bundle SHA-256 mismatch for ", manifest$file[[i]], ".", call. = FALSE)
    }
  }
  manifest
}

#' Load the pinned REACT extraction dictionary
#'
#' The installed bundle is verified against its SHA-256 manifest before it is
#' used. There is no runtime download or automatic update.
#'
#' @param refresh Reload and re-verify the installed bundle.
#' @return A named list of literal-character metadata data frames.
#' @export
react_dictionary <- function(refresh = FALSE) {
  if (!isTRUE(refresh) && !is.null(.reactextract_env$dictionary)) {
    return(.reactextract_env$dictionary)
  }
  path <- .dictionary_directory()
  manifest <- .verify_dictionary_bundle(path)
  table_files <- manifest$file[manifest$media_type == "text/csv" & manifest$file != "manifest.csv"]
  tables <- lapply(table_files, function(file) .read_literal_csv(file.path(path, file)))
  names(tables) <- sub("[.]csv$", "", table_files)
  tables$manifest <- manifest
  tables$bundle_path <- path
  .reactextract_env$dictionary <- tables
  tables
}

#' Report the pinned dictionary release
#'
#' @return A one-row data frame containing the release and manifest SHA-256.
#' @export
react_dictionary_version <- function() {
  dictionary <- react_dictionary()
  metadata <- stats::setNames(
    dictionary$bundle_metadata$value,
    dictionary$bundle_metadata$key
  )
  data.frame(
    dictionary_release = unname(metadata[["dictionary_release"]]),
    bundle_schema_version = unname(metadata[["bundle_schema_version"]]),
    manifest_sha256 = .dictionary_manifest_sha256,
    stringsAsFactors = FALSE
  )
}

#' Open the reviewed harmonisation notes
#'
#' The notes are copied unchanged from the pinned `react_wiki` dictionary
#' release. They explain approved joins, changed source names, response-code
#' decisions, and cases deliberately kept separate.
#'
#' @param open Open the Markdown document in the current R session's file
#'   viewer. Set to `FALSE` to return its installed path without opening it.
#' @return Invisibly, the local path to `HARMONISATION_NOTES.md`.
#' @export
react_harmonisation_notes <- function(open = interactive()) {
  path <- file.path(.dictionary_directory(), "HARMONISATION_NOTES.md")
  if (!file.exists(path)) {
    stop("The pinned dictionary has no harmonisation notes document.", call. = FALSE)
  }
  if (isTRUE(open)) {
    file.show(path, title = "REACT harmonisation notes and decisions")
  }
  invisible(normalizePath(path, mustWork = TRUE))
}

#' Open the synthetic-data methods and safety notes
#'
#' @param open Open the Markdown document in the current R session's viewer.
#' @return Invisibly, a local path containing the pinned dictionary methods and
#'   the current checksum-specific profile approval record.
#' @export
react_synthetic_notes <- function(open = interactive()) {
  methods_path <- file.path(.dictionary_directory(), "SYNTHETIC_DATA_NOTES.md")
  if (!file.exists(methods_path)) {
    stop("The pinned dictionary has no synthetic-data notes document.", call. = FALSE)
  }
  approval_path <- system.file(
    "extdata", "SYNTHETIC_PROFILE_APPROVAL.md",
    package = "reactextract"
  )
  path <- methods_path
  if (nzchar(approval_path) && file.exists(approval_path)) {
    path <- file.path(tempdir(), "reactextract-SYNTHETIC_DATA_NOTES.md")
    writeLines(
      c(
        readLines(methods_path, warn = FALSE, encoding = "UTF-8"),
        "",
        readLines(approval_path, warn = FALSE, encoding = "UTF-8")
      ),
      path,
      useBytes = TRUE
    )
  }
  if (isTRUE(open)) file.show(path, title = "REACT synthetic-data methods and limits")
  invisible(normalizePath(path, mustWork = TRUE))
}

#' Inspect reviewed grouped harmonisation decisions
#'
#' @param concepts Optional exact concept IDs. When omitted, all reviewed
#'   grouped decisions are returned.
#' @return A list with `groups` (one row per decision) and `inputs` (the exact
#'   round-specific source fields used by those decisions).
#' @export
react_harmonisation_decisions <- function(concepts = NULL) {
  dictionary <- react_dictionary()
  groups <- dictionary$harmonisation_groups
  inputs <- dictionary$harmonisation_inputs
  if (is.null(groups) || is.null(inputs)) {
    stop("The pinned dictionary has no grouped harmonisation decisions.", call. = FALSE)
  }
  if (!is.null(concepts)) {
    if (!is.character(concepts) || anyNA(concepts) || any(!nzchar(concepts))) {
      stop("`concepts` must contain non-empty exact concept IDs.", call. = FALSE)
    }
    unknown <- setdiff(concepts, dictionary$concepts$concept_id)
    if (length(unknown) > 0L) {
      stop("Unknown concept ID: ", paste(unknown, collapse = ", "), ".", call. = FALSE)
    }
    groups <- groups[groups$concept_id %in% concepts, , drop = FALSE]
    inputs <- inputs[inputs$decision_id %in% groups$decision_id, , drop = FALSE]
  }
  list(groups = groups, inputs = inputs)
}

#' Return source coding for reviewed concepts
#'
#' This lookup is generated by `react_wiki` from the literal response options.
#' It standardises only documented display labels and administrative missingness;
#' ambiguous and unrecognised codes remain explicit. It is not a scientific
#' harmonisation layer and requires no runtime download.
#'
#' @param concepts One or more exact concept IDs.
#' @return A literal-character data frame keyed by concept, round, raw field,
#'   and raw value.
#' @export
react_concept_coding <- function(concepts) {
  if (!is.character(concepts) || length(concepts) == 0L || anyNA(concepts) ||
      any(!nzchar(concepts))) {
    stop("`concepts` must contain one or more exact concept IDs.", call. = FALSE)
  }
  dictionary <- react_dictionary()
  unknown <- setdiff(concepts, dictionary$concepts$concept_id)
  if (length(unknown) > 0L) {
    stop("Unknown concept ID: ", paste(unknown, collapse = ", "), ".", call. = FALSE)
  }
  lookup <- dictionary$concept_value_lookup
  if (is.null(lookup)) {
    stop("The pinned dictionary has no source-coding lookup.", call. = FALSE)
  }
  lookup[lookup$concept_id %in% concepts, , drop = FALSE]
}

.family_key <- function(taxonomy_id) {
  parts <- strsplit(taxonomy_id, ".", fixed = TRUE)[[1]]
  parts <- gsub("_", "-", parts, fixed = TRUE)
  paste(parts, collapse = "/")
}

#' List extractable topic families
#'
#' @return The reviewed taxonomy with a human-readable `family` selector.
#' @export
react_families <- function() {
  taxonomy <- react_dictionary()$taxonomy
  taxonomy$family <- vapply(taxonomy$taxonomy_id, .family_key, character(1))
  taxonomy[c("family", names(taxonomy))]
}
