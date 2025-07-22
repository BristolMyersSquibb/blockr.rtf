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
  table = character(),
  directory = blockr_option(
    "rtf_dir",
    system.file("extdata", "examples", package = "artful")
  ),
  ...) {

  fun_env <- new.env()

  source(
    system.file("scripts", "topline-slide-tables.R", package = "artful"),
    local = fun_env
  )

  funs <- ls(envir = fun_env, pattern = "^rt_")

  files <- set_names(
    file.path(directory, paste0(gsub("_", "-", funs), ".rtf")),
    funs
  )

  if (length(table)) {

    stopifnot(
      length(table) == 1L,
      is.character(table),
      table %in% funs
    )
  }

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

          if (!dir.exists(directory)) {
            conds$error <- paste0(
              "Directory \"", directory, "\" does not exists."
            )
          } else if (!all(file.exists(files))) {
            conds$error <- paste0(
              "Expected files are missing from \"", directory, "\"."
            )
          }

          list(
            expr = reactive(
              bquote(
                .(fun)(.(file)),
                list(
                  fun = get(input$table, envir = fun_env, mode = "function",
                            inherits = FALSE),
                  file = files[input$table]
                )
              )
            ),
            state = list(
              table = reactive(input$table),
              directory = directory
            ),
            cond = conds
          )
        }
      )
    },
    function(id) {
      tagList(
        selectInput(
          inputId = NS(id, "table"),
          label = "Table",
          choices = funs,
          selected = table
        )
      )
    },
    class = "topline_block",
    ...
  )
}
