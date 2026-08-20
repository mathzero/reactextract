#' Write an extraction result without reshaping it
#'
#' @param result A result returned by [react_extract()].
#' @param path Output RDS file or Parquet directory.
#' @param format `rds` or `parquet`.
#' @return The normalised output path, invisibly.
#' @export
react_write <- function(result, path, format = c("rds", "parquet")) {
  common <- c("observations", "column_dictionary", "issues", "manifest")
  wide <- c("data", "raw_data")
  long <- c("raw_values", "harmonised_values")
  has_wide <- wide %in% names(result)
  has_long <- long %in% names(result)
  if (!is.list(result) || !all(common %in% names(result)) ||
      (!all(has_wide) && !all(has_long)) ||
      (any(has_wide) && !all(has_wide)) ||
      (any(has_long) && !all(has_long))) {
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
  tables <- c(wide, "observations", long, "column_dictionary", "issues", "manifest")
  tables <- tables[tables %in% names(result)]
  for (name in tables) {
    arrow::write_parquet(result[[name]], file.path(path, paste0(name, ".parquet")))
  }
  invisible(normalizePath(path, mustWork = TRUE))
}
