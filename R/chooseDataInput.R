#' Module for choosing the data.
#'
#' @param id Module id.
#' @param label Button label.
#' @param icon Button icon.
#' @param ... Additional arguments for the action button.
#' @noRd
#'
#' @importFrom htmltools tagList tags singleton
#' @importForm shiny NS actionButton icon
chooseDataInput <- function(id, label = "Data", icon = "database", ...) {
  ns <- NS(id)

  if(is.character(icon)) icon <- icon(icon)

  tagList(
    actionButton(inputId = ns("changeData"),
                 label = label,
                 icon = icon,
                 ...)
  )
}

#' The server that goes along with chooseDataInput module.
#'
#' @param input Standard `shiny` server argument.
#' @param output Standard `shiny` server argument.
#' @param session Standard `shiny` server argument.
#' @param dataModule Data module to use.
#' @param data A `data.frame` to use by default.
#' @param name Object's name to use for `data`.
#' @param selectVars Display module to select variables.
#'
#' @noRd
#'
#' @importFrom shiny showModal modalDialog observeEvent reactiveValues callModule observe icon
#' @importFrom htmltools tags HTML
chooseDataInputServer <- function(input, output, session,
                                  dataModule = c("GlobalEnv"),
                                  data = NULL,
                                  name = NULL,
                                  selectVars = TRUE) {

  dataModule <- match.arg(dataModule)
  datModUI <- switch(
    dataModule,
    "GlobalEnv" = dataInput,
    "ImportFile" = dataImportInput
  )
  datModServer <- switch(
    dataModule,
    "GlobalEnv" = dataInputServer,
    "ImportFile" = dataImportInputServer
  )

  ns <- session$ns
  return_data <- reactiveValues(data = data, name = name)

  datModUI(
    id = ns("chooseData"),
    selectVars = selectVars
  )

  observeEvent(input$changeData, {
    datModUI(
      id = ns("chooseData"),
      selectVars = selectVars
    )
  })

  return_data <- callModule(
    module = datModServer,
    id = "chooseData",
    data = data,
    name = name
  )

  return(return_data)
}
