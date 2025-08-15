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
  directory = blockr_option(
    "topline_dir",
    system.file("extdata", "examples", package = "artful")
  ),
  parser = blockr_option("rtf_parser", "artful"),
  ...) {

  if (length(file)) {
    stopifnot(length(file) == 1L, is.character(file))
  }

  new_data_block(
    function(id) {
      moduleServer(
        id,
        function(input, output, session) {

          root <- c(files = directory)

          conds <- reactiveValues(
            error = character(),
            warning = character(),
            message = character()
          )

          if (!dir.exists(directory)) {
            conds$error <- paste0(
              "Directory \"", directory, "\" does not exists."
            )
          } else if (!file %in% list.files(directory)) {
            conds$warning <- paste0(
              "No file \"", file, "\" found under \"", directory, "\"."
            )
          }

          sel <- reactiveVal(file)

          shinyFiles::shinyFileChoose(input, "file", roots = root)

          cur <- reactive(
            shinyFiles::parseFilePaths(root, input$file)$datapath
          )

          observeEvent(
            cur(),
            {
              req(cur())
              conds$warning <- character()
              sel(basename(cur()))
            }
          )

          list(
            expr = switch(
              match.arg(parser, c("artful", "stateful")),
              artful = reactive(
                bquote(
                  artful::rtf_to_ard(.(file)),
                  list(file = file.path(directory, sel()))
                )
              ),
              stateful = reactive(
                bquote(
                  stateful::state_table_to_ard(
                    stateful::parse_rtf_table_states(.(file))
                  ),
                  list(file = file.path(directory, sel()))
                )
              )
            ),
            state = list(
              file = sel,
              directory = directory,
              parser = parser
            ),
            cond = conds
          )
        }
      )
    },
    function(id) {
      shinyFiles::shinyFilesButton(
        NS(id, "file"),
        label = "File select",
        title = "Please select a file",
        multiple = FALSE
      )
    },
    class = "rtf_block",
    ...
  )
}
