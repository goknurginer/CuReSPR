# UI Component----
data_upload_ui <- function(id) {
  ns <- NS(id)

  tabPanel("Data Upload",
           sidebarLayout(
             sidebarPanel(
               h3("Enter the details of the experimental design"),
               helpText("First, define the number of comparison groups.
          Then, upload your fastq files and assign each fastq file to its group.
          Specify any biological or technical replicates, if they exist, and indicate whether the files are paired-end or single-end.
          Review and submit them for the counting step."),
               hr(),
               h4("Set group numbers"),
               helpText("Enter number of groups in your experiment."),
               numericInput(ns("num"), label = "", value = 0, min = 0),
               actionButton(ns("nextnum"), "Next"),
               conditionalPanel(condition = sprintf("input['%s'] > 0", ns("nextnum")),
                                hr(),
                                h4("Assign group names"),
                                helpText("Enter names for each group."),
                                uiOutput(ns("groupnames")),
                                actionButton(ns("nextupload"), "Next"),
                                hr()
               ),
               conditionalPanel(condition = sprintf("input['%s'] > 0", ns("nextupload")),
                                p("Groups are ", textOutput(ns("groups"), inline = TRUE)),
                                hr(),
                                h3("Enter the details of sgRNA"),
                                helpText("Please upload the guide library and specify if you want to provide the count matrix or do you want guide counts to be computed from raw reads, i.e. from a fatsq files, etc."),
                                hr(),
                                helpText("Enter number of groups in your experiment."),
                                h4("Upload guide RNA library"),
                                fileInput(ns("uploadguides"), label = "", accept = c('.tsv', '.csv', '.txt'), multiple = TRUE),
                                actionButton(ns("viewguides"), "View guide library"),
                                hr(),
                                radioButtons(ns("count_matrix_yes_no"), label = "Do you have a count matrix?", choices = list("Yes" = "yes", "No" = "no"), selected = ""),
                                conditionalPanel(condition = sprintf("input['%s'] == 'yes'", ns("count_matrix_yes_no")),
                                                 h4("Upload count matrix"),
                                                 fileInput(ns("uploadcounts"), label = "", accept = c('.tsv', '.csv', '.txt'), multiple = TRUE),
                                                 actionButton(ns("viewcounts"), "View count matrix")
                                ),
                                conditionalPanel(condition = sprintf("input['%s'] == 'no'", ns("count_matrix_yes_no")),
                                                 h4("Upload fastq files"),
                                                 fileInput(ns("upload"), label = "", accept = c('.fastq', 'fastq.gz'), multiple = TRUE),
                                                 checkboxInput(ns("paired"), label = "Fastq files are paired-end"),
                                                 checkboxInput(ns("tech"), "There are technical replicates"),
                                                 checkboxInput(ns("bio"), "There are biological replicates"),
                                                 actionButton(ns("viewfiles"), "View file details")
                                ),

               )
             ),
             mainPanel(
               conditionalPanel(condition = sprintf("input['%s'] > 0", ns("viewguides")),
                                hr(),
                                h4("Guide library"),
                                DT::dataTableOutput(ns("dataTableGuides"))
               ),
               conditionalPanel(condition = sprintf("input['%s'] == 'no' && input['%s'] > 0", ns("count_matrix_yes_no"), ns("viewfiles")),
                                hr(),
                                h4("Enter sample details"),
                                helpText("Please enter the details about the samples in the following table."),
                                DT::dataTableOutput(ns("dataTableFiles")),
                                verbatimTextOutput(ns('sel')),
                                actionButton(ns("gotocounting"), "Go to counting")
               ),
               conditionalPanel(condition = sprintf("input['%s'] == 'yes' && input['%s'] > 0", ns("count_matrix_yes_no"), ns("viewcounts")),
                                hr(),
                                h4("Count matrix"),
                                DT::dataTableOutput(ns("dataTableCounts")),
                                actionButton(ns("gotopreprocessing"), "Go to preprocessing")
               )
             )
           )
  )
}

# Server Component----

data_upload_server <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    myData <- reactiveVal(data.frame(Fastq = character(), Size = numeric(), Group = character()))

    observeEvent(input$viewfiles, {
      req(input$upload)
      test <- sapply(1:nrow(input$upload), function(i) {
        as.character(selectInput(ns(paste0("sel", i)), "", choices = unique(req(values())), width = "100px"))
      })
      newEntry <- data.frame(Fastq = input$upload[, 1],
                             Size = input$upload[, 2],
                             Group = test,
                             stringsAsFactors = FALSE)
      myData(newEntry)
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

    output$dataTableFiles <- DT::renderDataTable(
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

    output$dataTableCounts <- DT::renderDataTable({
      req(input$uploadcounts)

      # Extract file extension
      file_ext <- tools::file_ext(input$uploadcounts$name)

      # Read file based on extension and detect separator if necessary
      df <- switch(file_ext,
                   "csv" = read.table(input$uploadcounts$datapath, check.names = FALSE, sep = ","),
                   "tsv" = read.table(input$uploadcounts$datapath, check.names = FALSE, sep = "\t"),
                   "txt" = {
                     # Detect separator for .txt files
                     first_line <- readLines(input$uploadcounts$datapath, n = 1)
                     if (grepl("\t", first_line)) {
                       read.table(input$uploadcounts$datapath, check.names = FALSE, sep = "\t")
                     } else {
                       read.table(input$uploadcounts$datapath, check.names = FALSE, sep = " ")
                     }
                   },
                   stop("Unsupported file type"))

      datatable(df)
    })


    output$dataTableGuides <- DT::renderDataTable({
      req(input$uploadguides)

      # Extract file extension
      file_ext <- tools::file_ext(input$uploadguides$name)

      # Read file based on extension and detect separator if necessary
      df <- switch(file_ext,
                   "csv" = read.table(input$uploadguides$datapath, check.names = FALSE, sep = ","),
                   "tsv" = read.table(input$uploadguides$datapath, check.names = FALSE, sep = "\t"),
                   "txt" = {
                     # Detect separator for .txt files
                     first_line <- readLines(input$uploadguides$datapath, n = 1)
                     if (grepl("\t", first_line)) {
                       read.table(input$uploadguides$datapath, check.names = FALSE, sep = "\t")
                     } else {
                       read.table(input$uploadguides$datapath, check.names = FALSE, sep = " ")
                     }
                   },
                   stop("Unsupported file type"))
      colnames(df) <- c("sgRNA_ID","sgRNA_sequence", "gene_ID")
      datatable(df)
    })

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

    observe({
      updateRadioButtons(session, ns("count_matrix_yes_no"), selected = character(0))
    })

    observeEvent(input$gotocounting, {
      updateTabsetPanel(parent_session, "inTabset", selected = "Counting")
    })
  })
}
