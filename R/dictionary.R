.dictionary_manifest_sha256 <- "7f6731181c136eaa555041a80c55077446ecfaa49f459b7fb105ba1fc4ea4ad9"
.dictionary_archive_sha256 <- "46eabde854f4cb2b977379743cb66ef2c79789ea3f8706ead4e5e09d15355066"

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
