.reactextract_env <- new.env(parent = emptyenv())

.reactextract_supports_r <- function(version = getRversion()) {
  if (length(version) != 1L || is.na(version)) return(FALSE)
  utils::compareVersion(as.character(version), "4.4.0") >= 0L
}

.onLoad <- function(libname, pkgname) {
  version <- getRversion()
  if (!.reactextract_supports_r(version)) {
    stop(
      "reactextract requires R 4.4.0 or later; found R ",
      as.character(version),
      ".",
      call. = FALSE
    )
  }
}
