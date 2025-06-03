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

          addResourcePath(
            prefix = "rtf_previews",
            directoryPath = pkg_file("extdata")
          )

          observeEvent(
            input$preview,
            {
              req(input$file)

              file <- basename(input$file)

              showModal(
                modalDialog(
                  title = paste("PDF preview of", file),
                  tags$embed(
                    src = paste0("rtf_previews/", sub("\\.rtf$", ".pdf", file)),
                    type = "application/pdf",
                    width = "100%",
                    height = "500px"
                  ),
                  size = "xl",
                  easyClose = TRUE
                )
              )
            }
          )

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
      tagList(
        selectInput(
          inputId = NS(id, "file"),
          label = "File",
          choices = list_rtf_files(),
          selected = file
        ),
        actionButton(
          NS(id, "preview"),
          "Preview",
          class = "btn-primary"
        )
      )
    },
    class = "rtf_block",
    ...
  )
}
