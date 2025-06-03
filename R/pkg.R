#' @keywords internal
"_PACKAGE"

#' @import shiny
#' @import blockr.core
NULL

pkg_name <- function(env = parent.frame()) {
  utils::packageName(env)
}

pkg_file <- function(...) {
  system.file(..., package = pkg_name())
}
