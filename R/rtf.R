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
  ...
) {
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
              "Directory \"",
              directory,
              "\" does not exists."
            )
          } else if (length(file) && !file %in% list.files(directory)) {
            conds$warning <- paste0(
              "No file \"",
              file,
              "\" found under \"",
              directory,
              "\"."
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


#' Read and parse an RTF file into a dataframe
#'
#' This block reads and parses an RTF file into a `data.frame` object. The
#' function aims to return a copy of the original RTF table with as little data
#' preprocessing performed as possible (i.e., only essential steps such as
#' removing headers, footers, empty columns, etc., are performed). Unlike
#' `new_rtf_block()`, this function does not attempt to produce an ARD. Instead,
#' this step is off-loaded to other functions.
#'
#' @param file File name (in [base::basename()] sense)
#' @param directory Directory where `file` is located
#' @param indentation_to_vars Logical; if `TRUE` (default), converts indentation
#'   in the RTF table to explicit variables by calling
#'   [artful::indentation_to_variables()].
#' @param ... Forwarded to [new_block()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' blockr.core::serve(new_rtf_to_df_block(file = "rt-ae-ae1.rtf"))
#' }
new_rtf_to_df_block <- function(
  file = character(),
  directory = blockr_option(
    "topline_dir",
    system.file("extdata", "examples", package = "artful")
  ),
  indentation_to_vars = TRUE,
  ...
) {
  if (length(file)) {
    stopifnot(length(file) == 1L, is.character(file))
  }

  ui <- function(id) {
    tagList(
      shinyFiles::shinyFilesButton(
        NS(id, "file"),
        label = "File select",
        title = "Please select a file",
        multiple = FALSE
      ),
      checkboxInput(
        NS(id, "indentation_to_vars"),
        "Convert indentation to variables",
        indentation_to_vars
      )
    )
  }

  server <- function(id) {
    moduleServer(
      id,
      function(input, output, session) {

        conds <- reactiveValues(
          error = character(),
          warning = character(),
          message = character()
        )

        if (!dir.exists(directory)) {

          conds$error <- paste0(
            "Directory \"",
            directory,
            "\" does not exists."
          )

        } else {

          directory <- normalizePath(directory)

          if (length(file) && !file %in% list.files(directory)) {
            conds$warning <- paste0(
              "No file \"",
              file,
              "\" found under \"",
              directory,
              "\"."
            )
          }
        }

        root <- c(files = directory)

        sel <- reactiveVal(file)
        r_indentation_to_vars <- reactiveVal(indentation_to_vars)

        shinyFiles::shinyFileChoose(input, "file", roots = root)

        observeEvent(input$indentation_to_vars, {
          r_indentation_to_vars(input$indentation_to_vars)
        })

        cur <- reactive(
          shinyFiles::parseFilePaths(root, input$file)$datapath
        )

        observeEvent(
          cur(),
          {
            req(cur())
            conds$warning <- character()
            sel(sub(paste0("^", directory, "/"), "", normalizePath(cur())))
          }
        )

        list(
          expr = reactive(
            bquote(
              {
                df <- artful::rtf_to_df(.(file))
                if (.(indentation_to_vars)) {
                  df <- artful::indentation_to_variables(df)
                }
                df
              },
              list(
                file = file.path(directory, sel()),
                indentation_to_vars = r_indentation_to_vars()
              )
            )
          ),
          state = list(
            file = sel,
            directory = directory,
            indentation_to_vars = r_indentation_to_vars
          ),
          cond = conds
        )
      }
    )
  }

  new_data_block(
    ui = ui,
    server = server,
    class = "rtf_to_df_block",
    ...
  )
}
