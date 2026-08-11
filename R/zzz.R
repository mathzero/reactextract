.reactextract_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  version <- getRversion()
  if (version < "4.4.0" || version >= "4.5.0") {
    stop(
      "reactextract 0.1.x supports R 4.4.x only; found R ",
      as.character(version),
      ".",
      call. = FALSE
    )
  }
}
