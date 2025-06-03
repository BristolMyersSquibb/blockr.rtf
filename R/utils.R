list_rtf_files <- function() {

	res <- list.files(
    pkg_file("extdata"),
    pattern = "\\.rtf$",
    full.names = TRUE
  )

  names(res) <- basename(res)

  res
}
