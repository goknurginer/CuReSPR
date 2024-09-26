library(shiny)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(refund.shiny)
library(tidyverse)
library(readr)
library(edgeR)

# Define UI
ui <- navbarPage("CuReSPR", id = "main", theme = shinytheme("cerulean"),
                 tabsetPanel(id = "inTabset",
                             tabPanel("Data Upload",
                                      sidebarLayout(
                                        sidebarPanel(
                                          h3("Enter the details of the experimental design"),
                                          helpText("First, define the number of comparison groups.
                                                        Then, upload your fastq files and assign each fastq file to its group.
                                                        Specify any biological or technical replicates, if they exist,
                                                        and indicate whether the files are paired-end or single-end.
                                                        Review and submit them for the counting step."),
                                          hr(),
                                          h4("Set group numbers"),
                                          helpText("Enter number of groups in your experiment."),
                                          numericInput("num", label = "", value = 0, min = 0),
                                          actionButton("nextnum", "Next"),
                                          conditionalPanel(condition = "input.nextnum > 0",
                                                           hr(),
                                                           h4("Assign group names"),
                                                           helpText("Enter names for each group."),
                                                           uiOutput("groupnames"),
                                                           actionButton("nextupload", "Next"),
                                                           hr()
                                          ),
                                          conditionalPanel(condition = "input.nextupload > 0",
                                                           p("Groups are ", textOutput("groups", inline = TRUE)),
                                                           hr(),
                                                           h4("Upload guide RNA library"),
                                                           helpText("Please upload the guide library."),
                                                           hr(),
                                                           fileInput("uploadguides", label = "", accept = c('.tsv', '.csv', '.txt'), multiple = TRUE),
                                                           actionButton("viewguides", "View guide library"),
                                                           hr(),
                                                           hr(),
                                                           h4("Upload sample information"),
                                                           helpText("Please upload the file containing sample information."),
                                                           hr(),
                                                           fileInput("uploadsamples", label = "", accept = c('.tsv', '.csv', '.txt'), multiple = TRUE),
                                                           actionButton("viewsamples", "View sample information"),
                                                           hr(),
                                                           radioButtons("count_matrix_yes_no", label = "Do you have a count matrix?", choices = list("Yes" = "yes", "No" = "no"), selected = ""),
                                                           conditionalPanel(condition = "input.count_matrix_yes_no == 'yes'",
                                                                            h4("Upload count matrix"),
                                                                            helpText("Please upload the count matrix."),
                                                                            hr(),
                                                                            fileInput("uploadcounts", label = "", accept = c('.tsv', '.csv', '.txt'), multiple = TRUE),
                                                                            actionButton("viewcounts", "View count matrix")
                                                           ),
                                                           conditionalPanel(condition = "input.count_matrix_yes_no == 'no'",
                                                                            h4("Upload fastq files"),
                                                                            fileInput("upload", label = "", accept = c('.fastq', 'fastq.gz'), multiple = TRUE),
                                                                            checkboxInput("paired", label = "Fastq files are paired-end"),
                                                                            checkboxInput("tech", "There are technical replicates"),
                                                                            checkboxInput("bio", "There are biological replicates"),
                                                                            actionButton("viewfiles", "View file details")
                                                           )
                                          )
                                        ),
                                        mainPanel(
                                          conditionalPanel(condition = "input.viewguides > 0",
                                                           hr(),
                                                           h4("Guide library"),
                                                           DT::dataTableOutput("dataTableGuides")
                                          ),
                                          conditionalPanel(condition = "input.viewsamples > 0",
                                                           hr(),
                                                           h4("Sample Information"),
                                                           DT::dataTableOutput("dataTableSamples")
                                          ),
                                          conditionalPanel(condition = "input.count_matrix_yes_no == 'no' && input.viewfiles > 0",
                                                           hr(),
                                                           h4("Enter sample details"),
                                                           helpText("Please enter the details about the samples in the following table."),
                                                           DT::dataTableOutput("dataTableFiles"),
                                                           verbatimTextOutput('sel'),
                                                           actionButton("gotocounting", "Go to counting")
                                          ),
                                          conditionalPanel(condition = "input.count_matrix_yes_no == 'yes' && input.viewcounts > 0",
                                                           hr(),
                                                           h4("Count matrix"),
                                                           DT::dataTableOutput("dataTableCounts"),
                                                           actionButton("gotopreprocessing", "Go to preprocessing")
                                          )
                                        )
                                      )
                             ),
                             tabPanel("Counting",
                                      sidebarLayout(
                                        sidebarPanel(
                                          h4("Select your method of counting"),
                                          selectInput("method", "", choices = c("Rsubread", "MAGeCK", "WEHI")),
                                          actionButton("guidecounts", "Get the guide counts")
                                        ),
                                        mainPanel(
                                          conditionalPanel(condition = "input.guidecounts > 0",
                                                           uiOutput("dynamic_ui")
                                          )
                                        )
                                      )
                             ),
                             tabPanel("Preprocessing",
                                      sidebarLayout(
                                        sidebarPanel(
                                          h4("Preprocessing Counts"),
                                          actionButton("create_dgelist", "Create DGEList")
                                        ),
                                        mainPanel(
                                          textOutput("dgelist_status")  # Display status for DGEList creation
                                        )
                                      )
                             ),
                             tabPanel("Differential Analysis", h4("Testing counting"), fluidPage(sidebarPanel())),
                             tabPanel("Pathway Analysis", h4("Testing counting"), fluidPage(sidebarPanel()))
                 )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to store the uploaded guides and sample information
  data_upload_output <- reactiveValues(uploadguides = NULL, viewguides = NULL)
  observeEvent(input$viewguides, {
    data_upload_output$uploadguides <- input$uploadguides
    data_upload_output$viewguides <- TRUE
  })

  sample_upload_output <- reactiveValues(uploadsamples = NULL, viewsamples = NULL)
  observeEvent(input$viewsamples, {
    sample_upload_output$uploadsamples <- input$uploadsamples
    sample_upload_output$viewsamples <- TRUE
  })

  myData <- reactiveVal(data.frame(Fastq = character(), Size = numeric(), Group = character()))
  observeEvent(input$viewfiles, {
    req(input$upload)
    test <- sapply(1:nrow(input$upload), function(i) {
      as.character(selectInput(paste0("sel", i), "", choices = unique(req(values())), width = "100px"))
    })
    newEntry <- data.frame(Fastq = input$upload[, 1],
                           Size = input$upload[, 2],
                           Group = test,
                           stringsAsFactors = FALSE)
    myData(newEntry)
  })

  base_data <- reactive({
    if (as.logical(input$paired)) {
      myData() %>% add_column(FastqPair = rep(1, nrow(myData())))
    } else {
      myData()
    }
  })

  v <- reactive({
    result <- base_data()
    if (as.logical(input$tech)) {
      result <- result %>% add_column(TechRep = rep(1, nrow(myData())))
    }
    if (as.logical(input$bio)) {
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

  output$dataTableSamples <- DT::renderDataTable({
    req(input$uploadsamples)

    # Extract file extension
    file_ext <- tools::file_ext(input$uploadsamples$name)

    # Read file based on extension and detect separator if necessary
    df <- switch(file_ext,
                 "csv" = read.table(input$uploadsamples$datapath, check.names = FALSE, sep = ","),
                 "tsv" = read.table(input$uploadsamples$datapath, check.names = FALSE, sep = "\t"),
                 "txt" = {
                   # Detect separator for .txt files
                   first_line <- readLines(input$uploadsamples$datapath, n = 1)
                   if (grepl("\t", first_line)) {
                     read.table(input$uploadsamples$datapath, check.names = FALSE, sep = "\t")
                   } else {
                     read.table(input$uploadsamples$datapath, check.names = FALSE, sep = " ")
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
    colnames(df) <- c("sgRNA_ID", "sgRNA_sequence", "gene_ID")
    datatable(df)
  })

  output$sel <- renderPrint({
    str(sapply(1:nrow(myData()), function(i) input[[paste0("sel", i)]]))
  })

  values <- reactive({
    unlist(lapply(1:n(), function(i) input[[paste0("group", i)]]))
  })

  n <- reactive({
    input$num
  })

  output$groupnames <- renderUI({
    groupnames <- lapply(1:n(), function(i) {
      textInput(paste0("group", i), label = paste0("Group ", i))
    })
    do.call(tagList, groupnames)
  })

  output$groups <- renderText({
    req(values())
    paste("Groups are:", paste(values(), collapse = ", "))
  })

  observeEvent(input$gotocounting, {
    updateTabsetPanel(session, "inTabset", selected = "Counting")
  })

  observeEvent(input$gotopreprocessing, {
    updateTabsetPanel(session, "inTabset", selected = "Preprocessing")
  })

  # Counting functionality
  observeEvent(input$guidecounts, {
    output$status <- renderText("Calculating guide counts...")
    req(data_upload_output$uploadguides)

    # Here you would add logic for the counting process
    # This is just a placeholder to show you might generate counts
    count_data <- data.frame(
      sgRNA_ID = c("sgRNA1", "sgRNA2"),
      Count = c(100, 200)  # Sample count data; replace with actual counting logic
    )

    output$count_table <- renderDT({
      datatable(count_data)
    })
  })

  # Create dynamic UI for counting output
  output$dynamic_ui <- renderUI({
    if (input$guidecounts) {
      tagList(
        DT::dataTableOutput("count_table"),
        downloadButton("download", "Download"),
        textOutput("status")
      )
    }
  })

  # Download handler for counting data
  output$download <- downloadHandler(
    filename = function() {
      paste0("guide-counts-", Sys.Date(), ".csv")
    },
    content = function(file) {
      # Ensure there is data to write
      req(input$guidecounts)
      count_data <- data.frame(
        sgRNA_ID = c("sgRNA1", "sgRNA2"),
        Count = c(100, 200)  # Sample count data; replace with actual counting logic
      )
      write.csv(count_data, file, row.names = FALSE)
    }
  )

  # Status for DGEList creation
  output$dgelist_status <- renderText({
    req(input$create_dgelist)  # Ensure the button was clicked
    if (is.null(data_upload_output$uploadcounts) ||
        is.null(data_upload_output$uploadguides) ||
        is.null(data_upload_output$uploadsamples)) {
      return("Please ensure all required files are uploaded.")
    }

    # Here you would implement the DGEList creation logic
    return("DGEList created successfully!")  # Update this to reflect actual status
  })
}

# Run the app
shinyApp(ui = ui, server = server)
