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
#' @rdname rtfz
#' @export
new_rtf_block <- function(
  file = character(),
  directory = blockr_option("rtf_dir", pkg_file("extdata")),
  parser = blockr_option("rtf_parser", "artful"),
  ...
) {
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
              "No .rtf files found under \"",
              directory,
              "\""
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
                    "No preview available for ",
                    file
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

#' Convert RTF to PDF and display as image
#'
#' This block converts an RTF file to PDF using artful::rtf_to_pdf() and
#' displays the PDF as an image in a temporary file.
#'
#' @param file File name (in [base::basename()] sense)
#' @param directory Directory where `file` is located
#' @param soffice_path Path to LibreOffice soffice executable
#' @param ... Forwarded to [new_block()]
#'
#' @rdname rtfz
#' @export
new_rtf_to_pdf_block <- function(
  file = character(),
  directory = blockr_option("rtf_dir", pkg_file("extdata")),
  soffice_path = "/Applications/LibreOffice.app/Contents/MacOS/soffice",
  ...
) {
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
              "No .rtf files found under \"",
              directory,
              "\""
            )
          }

          pdf_reactive <- reactive({
            req(input$file)

            rtf_path <- input$file
            temp_pdf <- tempfile(fileext = ".pdf")

            tryCatch(
              {
                artful::rtf_to_pdf(
                  rtf_path = rtf_path,
                  pdf_path = temp_pdf,
                  soffice_path = soffice_path
                )
                temp_pdf
              },
              error = function(e) {
                conds$error <- paste("Error generating PDF:", e$message)
                NULL
              }
            )
          })

          output$pdf_display <- renderUI({
            pdf_path <- pdf_reactive()
            if (!is.null(pdf_path) && file.exists(pdf_path)) {
              tags$embed(
                src = pdf_path,
                type = "application/pdf",
                width = "100%",
                height = "600px"
              )
            } else {
              tags$p("PDF not available")
            }
          })

          list(
            expr = reactive({
              pdf_path <- pdf_reactive()
              bquote({
                temp_pdf <- tempfile(fileext = ".pdf")
                artful::rtf_to_pdf(
                  rtf_path = .(input$file),
                  pdf_path = temp_pdf,
                  soffice_path = .(soffice_path)
                )
                temp_pdf
              })
            }),
            state = list(
              file = reactive(input$file),
              directory = directory,
              pdf_path = pdf_reactive
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
        uiOutput(NS(id, "pdf_display"))
      )
    },
    class = "rtf_to_pdf_block",
    ...
  )
}
