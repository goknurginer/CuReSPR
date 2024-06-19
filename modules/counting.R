# UI Component
counting_ui <- function(id) {
  ns <- NS(id)

  tabPanel("Counting",
           sidebarLayout(
             sidebarPanel(
               h4("Select your method of counting"),
               selectInput(ns("counting"), "", choices = c("Rsubread", "MAGeCK", "WEHI")),
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
counting_server <- function(id, path) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    button_clicked <- reactiveVal(FALSE)

    observeEvent(input$counting, {
      button_clicked(FALSE)
    })

    observeEvent(input$guidecounts, {
      button_clicked(TRUE)
    })

    output$dynamic_ui <- renderUI({
      if (button_clicked()) {
        tagList(
          DT::dataTableOutput(ns("count_table")),
          downloadButton(ns("download"), "Download")
        )
      }
    })

    count_data <- reactive({
      file_path <- switch(input$counting,
                          "Rsubread" = paste0(path,"/rsubread/counts_rsubread.tsv"),
                          "MAGeCK" = paste0(path,"/mageck/counts_mageck.csv"),
                          "WEHI" = paste0(path,"/wehi/counts_wehi.csv"))
      data <- read.table(file_path, sep = "\t", header = TRUE)
      datatable(data)
    })

    output$count_table <- renderDT({
      req(button_clicked())
      count_data()
    })

    output$download <- downloadHandler(
      filename = function() {
        paste0(input$counting, "-guide-counts-", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(button_clicked())
        vroom::vroom_write(count_data(), file)
      }
    )
  })
}
