#' Module to choose a `data.frame` from the global environment.
#'
#' @param id Module id.
#' @param selectVars Display module to select variables.
#'   Defaults to `TRUE`.
#'
#' @noRd
#'
#' @importFrom htmltools tagList tags HTML
#' @importFrom shiny NS actionButton icon
#' @importFrom shinyWidgets pickerInput
dataInput <- function(id, selectVars = TRUE) {
  ns <- NS(id)

  # get list of data.frames
  dfs <- search_obj(what = "data.frame")
  if (is.null(dfs)) {
    dfs <- data(package = "tidyr",
                envir = environment())$results[, "Item"]
  }

  # get metadata on dfs
  info_dfs <- lapply(
    X = dfs,
    FUN = function(x) {
      tmp <- get_df(x)
      sprintf("%d obs. of %d variables", nrow(tmp), ncol(tmp))
    }
  )
  info_dfs <- unlist(info_dfs)

  # create shiny taglist
  tagList(
    # useShinyUtils(),
    tags$script(
      sprintf("Shiny.onInputChange('%s', %f);",
              ns("dataGlobalEnv"),
              as.numeric(Sys.time()))
    ),
    # tags$h2("Select a dataset"),
    pickerInput(
      inputId = ns("data"),
      label = "Choose a data set:",
      choices = dfs,
      width = "100%",
      options = list(title = "List of data sets..."),
      choicesOpt = list(subtext = info_dfs)
    ),
    tags$div(
      id = ns("placeholder-result-import"),
      tags$div(
        id = ns("result-import"),
        class = "alert alert-info",
        tags$b("No data selected!"),
        "Select a data set from drop-down menu."
      )
    ),
    tags$div(
      style = if(!isTRUE(selectVars)) "display: none;",
      tags$br(),
      selectVarsInput(id = ns("selected"))
    ),
    tags$br(),
    actionButton(
      inputId = ns("validate"),
      label = "Validate selected data",
      icon = icon("arrow-circle-right"),
      width = "100%",
      disabled = "disabled",
      class = "btn-primary"
    )
  )
}

#' The server that goes along with dataInput module.
#'
#' @param input Standard `shiny` server argument.
#' @param output Standard `shiny` server argument.
#' @param session Standard `shiny` server argument.
#' @param data Default `data.frame` to use.
#' @param name Default `name` to use.
#'
#' @noRd
#'
#' @importFrom shiny reactiveValues observeEvent req removeUI insertUI callModule
dataInputServer <- function(input, output, session,
                            data = NULL, name = NULL) {
  ns <- session$ns
  jns <- function(x) paste0("#", ns(x))

  imported_data <- reactiveValues(data = data, name = name)
  tmp_name <- reactiveValues(name = name)
  select_data <- reactiveValues(data = NULL,
                                name = NULL,
                                timestamp = Sys.time())

  observeEvent(input$dataGlobalEnv, {
    imported_data$data <- NULL
    imported_data$name <- NULL
  })

  observeEvent(input$data, {
    req(input$data)
    imported <- try(get_df(input$data), silent = TRUE)
    if ("try-error" %in% class(imported) || nrow(imported) < 1) {
      toggleInput(inputId = ns("validate"), enable = FALSE)
      removeUI(selector = jns("result-import"))
      insertUI(
        selector = jns("placeholder-result-import"),
        ui = tags$div(
          id = ns("result-import"),
          class = "alert alert-danger",
          tags$b("Ooops"),
          "Something went wrong!"
        )
      )
      select_data$data <- NULL
      tmp_name$name <- NULL
      select_data$timestamp <- Sys.time()
    } else {
      toggleInput(inputId = ns("validate"),
                  enable = TRUE)
      removeUI(selector = jns("result-import"))
      insertUI(
        selector = jns("placeholder-result-import"),
        ui = tags$div(
          id = ns("result-import"),
          class = "alert alert-success",
          tags$b("Success"),
          sprintf("%s obs. of %s variables imported.",
                  nrow(imported), ncol(imported))
        )
      )
      select_data$data <- imported
      tmp_name$name <- input$data
      select_data$timestamp <- Sys.time()
    }
  }, ignoreInit = TRUE)

  sv <- callModule(module = selectVarsInputServer,
                   id = "selected",
                   data = select_data)

  observeEvent(sv$selected_vars, {
    if (length(sv$selected_vars) > 0) {
      toggleInput(inputId = ns("validate"), enable = TRUE)
    } else {
      toggleInput(inputId = ns("validate"), enable = FALSE)
    }
  }, ignoreNULL = FALSE)

  return(imported_data)
}
