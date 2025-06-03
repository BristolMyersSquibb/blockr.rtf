register_rtf_blocks <- function() { # nocov start
  register_blocks(
    c(
      "new_rtf_block"
    ),
    name = c(
      "RTF file parser"
    ),
    description = c(
      "Read and parse RTF file to ARD data.frame"
    ),
    category = c(
      "data"
    ),
    package = pkg_name(),
    overwrite = TRUE
  )
}

.onLoad <- function(libname, pkgname) {

  register_rtf_blocks()

  invisible(NULL)
} # nocov end
