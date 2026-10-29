#' Shorten cleaned column names for analysis
#'
#' Cleaned columns are named by their full concept ID, for example
#' `people_households.demographics.gender`, and multi-field concepts add
#' `__field__` and the exact raw field, for example
#' `people_households.demographics.age__field__U_AGE`. This helper drops the
#' topic prefix and joins any raw field with a single underscore, giving
#' `gender` and `age_U_AGE`. Identifier columns and columns you have added
#' yourself are left unchanged. If a shortened name would clash with another
#' column, the original name is kept for that column.
#'
#' Use the original names (or [react_search()]) to look a column up in
#' `result$column_dictionary`.
#'
#' @param x The cleaned `data` table from [react_extract()], or the extraction
#'   result itself.
#' @return A data frame identical to the cleaned table except for its names.
#' @examples
#' result <- react_extract(
#'   react_synthetic(n_per_round = c(REACT1_R10 = 20L), seed = 1L),
#'   families = "people-households/demographics",
#'   rounds = "REACT1_R10",
#'   progress = FALSE
#' )
#' names(react_short_names(result))
#' @export
react_short_names <- function(x) {
  if (inherits(x, "react_extract_result") ||
      (is.list(x) && !is.data.frame(x) && is.data.frame(x$data))) {
    x <- x$data
  }
  if (!is.data.frame(x)) {
    stop("`x` must be `result$data` or the extraction result.", call. = FALSE)
  }
  original <- names(x)
  taxonomy <- react_dictionary()$taxonomy$taxonomy_id
  taxonomy <- taxonomy[order(-nchar(taxonomy))]
  short <- vapply(original, function(name) {
    prefix <- taxonomy[startsWith(name, paste0(taxonomy, "."))]
    if (length(prefix) == 0L) return(name)
    sub("__field__", "_", substring(name, nchar(prefix[[1L]]) + 2L), fixed = TRUE)
  }, character(1), USE.NAMES = FALSE)
  clash <- short != original &
    (duplicated(short) | duplicated(short, fromLast = TRUE) | short %in% original)
  short[clash] <- original[clash]
  names(x) <- short
  x
}
