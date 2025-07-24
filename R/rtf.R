#' Read and parse RTF files
#'
#' This block allows to make read and parse an RTF file to an ARD formatted
#' `data.frame`.
#'
#' @param file File name (in [base::basename()] sense)
#' @param directory Directory where `file` is located
#' @param parser RTF parser to use
#' @param ... Forwarded to [new_block()]
#'
#' @rdname rtf
#' @export
new_rtf_block <- function(
  file = character(),
  directory = blockr_option("rtf_dir", pkg_file("extdata")),
  parser = blockr_option("rtf_parser", "artful"),
  ...) {

  stopifnot(dir.exists(directory))

  files <- list_rtf_files(directory)

  if (length(file)) {

    stopifnot(
      length(file) == 1L,
      is.character(file),
      file %in% names(files)
    )

    file <- files[file]
  }

  has_previews <- any(
    file.exists(sub("\\.rtf$", ".pdf", files))
  )
	
  new_data_block(
    function(id) {
      moduleServer(
        id,
        function(input, output, session) {

          conds <- reactiveValues(
            error = character(),
            warning = character(),
            message = character()
          )

          if (!length(files)) {
            conds$error <- paste0(
              "No .rtf files found under \"", directory, "\""
            )
          }

          if (has_previews) {

            addResourcePath(
              prefix = "rtf_previews",
              directoryPath = directory
            )

            observeEvent(
              input$file,
              {
                if (length(conds$warning)) {
                  conds$warning <- character()
                }
              }
            )

            observeEvent(
              input$preview,
              {
                req(input$file)

                file <- basename(input$file)
                prev <- sub("\\.rtf$", ".pdf", file)

                if (file.exists(file.path(directory, prev))) {

                  showModal(
                    modalDialog(
                      title = paste("PDF preview of", file),
                      tags$embed(
                        src = paste0("rtf_previews/", prev),
                        type = "application/pdf",
                        width = "100%",
                        height = "500px"
                      ),
                      size = "xl",
                      easyClose = TRUE
                    )
                  )

                } else {
                  conds$warning <- paste(
                    "No preview available for ", file
                  )
                }
              }
            )
          }

          list(
            expr = reactive(
              bquote(
                switch(
                  match.arg(parser, c("artful", "stateful")),
                  artful = {
                    suppressMessages(
                      artful::rtf_to_ard(.(file))
                    )
                  },
                  stateful = {
                    stateful::state_table_to_ard(
                      stateful::parse_rtf_table_states(.(file))
                    )
                  }
                ),
                list(file = input$file)
              )
            ),
            state = list(
              file = reactive(input$file),
              directory = directory,
              parser = parser
            ),
            cond = conds
          )
        }
      )
    },
    function(id) {
      tagList(
        selectInput(
          inputId = NS(id, "file"),
          label = "File",
          choices = files,
          selected = file
        ),
        if (has_previews) {
          actionButton(
            NS(id, "preview"),
            "Preview",
            class = "btn-primary"
          )
        }
      )
    },
    class = "rtf_block",
    ...
  )
}
