#' Read and parse RTF files
#'
#' This block allows to make read and parse an RTF file to an ARD formatted
#' `data.frame`.
#'
#' @param file File name
#' @param ... Forwarded to [new_block()]
#'
#' @rdname rtf
#' @export
new_rtf_block <- function(file = character(), ...) {

  if (length(file)) {
    stopifnot(
      length(file) == 1L,
      is.character(file),
      file %in% list_rtf_files()
    )
  }
	
  new_data_block(
    function(id) {
      moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = reactive(
              bquote(artful::rtf_to_ard(.(file)), list(file = input$file))
            ),
            state = list(file = reactive(input$file))
          )
        }
      )
    },
    function(id) {
      selectInput(
        inputId = NS(id, "file"),
        label = "File",
        choices = list_rtf_files(),
        selected = file
      )
    },
    class = "rtf_block",
    ...
  )
}
