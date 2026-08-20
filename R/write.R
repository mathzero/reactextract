#' Write an extraction result without reshaping it
#'
#' @param result A result returned by [react_extract()].
#' @param path Output RDS file or Parquet directory.
#' @param format `rds` or `parquet`.
#' @return The normalised output path, invisibly.
#' @export
react_write <- function(result, path, format = c("rds", "parquet")) {
  if (!is.list(result) || !all(
    c(
      "data", "raw_data", "observations", "raw_values",
      "harmonised_values", "column_dictionary", "issues", "manifest"
    ) %in% names(result)
  )) {
    stop("`result` must be returned by `react_extract()`.", call. = FALSE)
  }
  format <- match.arg(format)
  path <- .single_string(path, "path")
  if (format == "rds") {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(result, path, version = 3)
    return(invisible(normalizePath(path, mustWork = TRUE)))
  }
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Parquet output requires the optional `arrow` package.", call. = FALSE)
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  tables <- c(
    "data", "raw_data", "observations", "raw_values",
    "harmonised_values", "column_dictionary", "issues", "manifest"
  )
  for (name in tables) {
    arrow::write_parquet(result[[name]], file.path(path, paste0(name, ".parquet")))
  }
  invisible(normalizePath(path, mustWork = TRUE))
}
