# UI Component
counting_ui <- function(id) {
  ns <- NS(id)

  tabPanel("Counting",
           sidebarLayout(
             sidebarPanel(
               h4("Select your method of counting"),
               selectInput(ns("method"), "", choices = c("Rsubread", "MAGeCK", "WEHI")),
               actionButton(ns("guidecounts"), "Get the guide counts"),
             ),
             mainPanel(
               conditionalPanel(condition = sprintf("input['%s'] > 0", ns("guidecounts")),
                                uiOutput(ns("dynamic_ui"))
               )
             )
           )
  )
}

# Server Component----
counting_server <- function(id, data_upload_input) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    button_clicked <- reactiveVal(FALSE)
    my_counts <- reactiveVal(NULL)

    observeEvent(input$guidecounts, {
      button_clicked(TRUE)
      if (input$method == "Rsubread") {
        output$status <- renderText("Creating rsubread directory under path...")
        observeEvent(data_upload_input$viewguides, {
          req(data_upload_input$uploadguides)
          print("View guides button clicked in counting module.")
          print("File uploaded.")
          print(data_upload_input$uploadguides$datapath)
          print(head(read.table(data_upload_input$uploadguides$datapath, check.names = FALSE, sep = ",")))
        })
      } else if (input$method == "MAGeCK" || input$method == "WEHI") {
        output$status <- renderText("This is under construction.")
      }
    })

    observeEvent(input$method, {
      button_clicked(FALSE)
      my_counts(NULL)
    })

    observeEvent(input$guidecounts, {
      button_clicked(TRUE)
      if (input$method == "Rsubread") {
        output$status <- renderText("Creating rsubread directory under path...")
        observeEvent(data_upload_input$viewguides,{
          print(data_upload_input$uploadguides$datapath)
          print(head(read.table(data_upload_input$uploadguides$datapath, check.names = FALSE, sep = ",")))
        })
      } else if (input$method == "MAGeCK" || input$method == "WEHI") {
        output$status <- renderText("This is under construction.")
      }
      })

    count_data <- reactive({
      req(button_clicked())
      if (input$method == "Rsubread") {
        my_counts()
      } else {
        data.frame()
      }
    })

    output$count_table <- renderDT({
      req(button_clicked())
      datatable(count_data())
    })

    output$dynamic_ui <- renderUI({
      if (button_clicked()) {
        tagList(
          DT::dataTableOutput(ns("count_table")),
          downloadButton(ns("download"), "Download"),
          textOutput(ns("status"))
        )
      }
    })

    output$download <- downloadHandler(
      filename = function() {
        paste0(input$method, "-guide-counts-", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(button_clicked())
        vroom::vroom_write(count_data(), file)
      }
    )
  })
}
