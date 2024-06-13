data_upload_server <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    myData <- reactiveVal(data.frame(Fastq = character(), Size = numeric(), Group = character()))

    observeEvent(input$nextdatatable, {
      test <- sapply(1:nrow(input$upload), function(i) {
        as.character(selectInput(ns(paste0("sel", i)), "", choices = unique(req(values())), width = "100px"))
      })
      newEntry <- data.frame(Fastq = input$upload[, 1],
                             Size = input$upload[, 2],
                             Group = test,
                             stringsAsFactors = FALSE)
      myData(rbind(myData(), newEntry))
    })

    base_data <- reactive({
      if (as.logical(input[['paired']])) {
        myData() %>% add_column(FastqPair = rep(1, nrow(myData())))
      } else {
        myData()
      }
    })

    v <- reactive({
      result <- base_data()
      if (as.logical(input[['tech']])) {
        result <- result %>% add_column(TechRep = rep(1, nrow(myData())))
      }
      if (as.logical(input[['bio']])) {
        result <- result %>% add_column(BioRep = rep(1, nrow(myData())))
      }
      result
    })

    output$dataTable <- DT::renderDataTable(
      v(),
      editable = TRUE,
      escape = FALSE,
      selection = 'none',
      server = FALSE,
      options = list(dom = 't', paging = FALSE, ordering = FALSE),
      callback = JS("table.rows().every(function(i, tab, row) {
        var $this = $(this.node());
        $this.attr('id', this.data()[0]);
        $this.addClass('shiny-input-container');
      });
      Shiny.unbindAll(table.table().node());
      Shiny.bindAll(table.table().node());")
    )

    output$sel <- renderPrint({
      str(sapply(1:nrow(myData()), function(i) input[[paste0("sel",i)]]))
    })

    values <- reactive({
      unlist(lapply(1:n(), function(i) input[[paste0("group", i)]]))
    })

    n <- reactive({
      input$num
    })

    output$groupnames <- renderUI({
      groupnames <- lapply(1:n(), function(i) {
        textInput(ns(paste0("group", i)), label = paste0("Group ", i))
      })
      do.call(tagList, groupnames)
    })

    output$groups <- renderText({
      req(values())
    })

    observeEvent(input$gotocounting, {
      updateTabsetPanel(parent_session, "inTabset", selected = "Counting")
    })
  })
}
