#' Read and parse Topline demo RTF files
#'
#' This block allows to make read and parse specific RTF files shared as part
#' of an example topline slide deck.
#'
#' @param table Table name
#' @param directory Directory where corresponding table is located
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_topline_block <- function(
  file = character(),
  directory = blockr_option(
    "topline_dir",
    system.file("extdata", "examples", package = "artful")
  ),
  ...) {

  fun_env <- new.env()

  source(
    system.file("scripts", "topline-demo-app.R", package = "artful"),
    local = fun_env
  )

  funs <- ls(envir = fun_env, pattern = "^rt_")

  files <- set_names(
    paste0(gsub("_", "-", funs), ".rtf"),
    funs
  )

  names(funs) <- unname(files)

  if (length(file)) {

    stopifnot(
      length(file) == 1L,
      is.character(file),
      file %in% files
    )
  }

  new_data_block(
    function(id) {
      moduleServer(
        id,
        function(input, output, session) {

          root <- c(topline = directory)

          conds <- reactiveValues(
            error = character(),
            warning = character(),
            message = character()
          )

          if (!dir.exists(directory)) {
            conds$error <- paste0(
              "Directory \"", directory, "\" does not exists."
            )
          } else if (!all(file.exists(file.path(directory, files)))) {
            conds$error <- paste0(
              "Expected files are missing from \"", directory, "\"."
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
              if (basename(cur()) %in% files) {
                conds$warning <- character()
                sel(basename(cur()))
              } else {
                conds$warning <- paste0("Cannot process this file.")
              }
            }
          )

          list(
            expr = reactive(
              bquote(
                .(fun)(.(file)),
                list(
                  fun = get(funs[sel()], envir = fun_env, mode = "function",
                            inherits = FALSE),
                  file = file.path(directory, sel())
                )
              )
            ),
            state = list(
              file = sel,
              directory = directory
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
    class = "topline_block",
    ...
  )
}
