list_rtf_files <- function(dir = pkg_file("extdata")) {

	res <- list.files(
    dir,
    pattern = "\\.rtf$",
    full.names = TRUE
  )

  names(res) <- basename(res)

  res
}

