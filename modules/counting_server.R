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
                          "Rsubread" = paste0(path,"/rsubread/counts_rsubread.csv"),
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
