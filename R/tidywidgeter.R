tidywidgeter <- function(data = NULL) {

  res_data <- get_data(data, name = deparse(substitute(data)))

  rv <- reactiveValues(
    data = res_data$tidy_data,
    name = res_data$tidy_data_name
  )

  ui <- miniPage(

    # title bar
    gadgetTitleBar(
      "Transform data",
      left = miniTitleBarCancelButton(),
      right = miniTitleBarButton("done", "Done", primary = TRUE)
    ),

    # option to tab between
    miniTabstripPanel(
      # data selector
      miniTabPanel("Data", icon = icon("table"),
        miniContentPanel(
          DT::dataTableOutput("userdata")
        )
      ),
      # pivot longer
      miniTabPanel("Pivot Longer", icon = icon("arrows-v"),
        miniContentPanel(
          DT::dataTableOutput("longer")
        )
      ),
      # pivot wider
      miniTabPanel("Pivot Wider", icon = icon("arrows-h"),
        miniContentPanel(
          DT::dataTableOutput("wider")
        )
      ),
      # export code
      miniTabPanel("Export", icon = icon("laptop-code"),
        miniContentPanel(
          textOutput("code")
        )
      )
    )
  )

  server <- function(input, output, session) {

    output$userdata <- DT::renderDataTable({
      rv$data
    })

    # make longer
    rv_longer <- reactive(
      rv$data %>%
        pivot_longer(-religion,
                     names_to = "income",
                     values_to = "count")
    )

    output$longer <- DT::renderDataTable({
      rv_longer()
    })

    # make wider
    rv_wider <- reactive(rv$data)

    output$wider <- DT::renderDataTable({
      rv_wider()
    })

    # generate code
    output$code <- renderText({
      "hello"
    })

    # listen for done events
    observeEvent(input$done, {
      stopApp()
    })
  }

  # use the dialog viewer
  viewer <- dialogViewer("Tidywidget", width = 700, height = 400)
  runGadget(app = ui,
            server = server,
            viewer = viewer)
}

