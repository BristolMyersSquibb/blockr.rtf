register_rtf_blocks <- function() {
  # nocov start
  register_blocks(
    c(
      "new_rtf_block",
      "new_rtf_to_df_block",
      "new_topline_block",
      "new_card_block"
    ),
    name = c(
      "Generic RTF file parser",
      "RTF to dataframe parser",
      "RTF ARD parser for example data",
      "RTF CARD parser for example data"
    ),
    description = c(
      "Read and parse RTF file to ARD data.frame",
      "Read and parse RTF file to data.frame",
      "Parse specific tables to ARD data.frame",
      "Parse specific tables to CARD"
    ),
    category = c(
      "data",
      "data",
      "data",
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
