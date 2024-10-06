library(shiny)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(refund.shiny)
library(tidyverse)
library(readr)
library(edgeR)

# Set maximum upload size to 1 GB
options(shiny.maxRequestSize = 1000 * 1024^2)

# Define a function to read files and render DataTable
read_file_and_render <- function(file_input) {
  req(file_input)  # Ensure the file is uploaded

  # Extract file extension
  file_ext <- tools::file_ext(file_input$name)

  # Read file based on extension and detect separator if necessary
  df <- switch(file_ext,
               "csv" = read.table(file_input$datapath, check.names = FALSE, sep = ",", header = TRUE),
               "tsv" = read.table(file_input$datapath, check.names = FALSE, sep = "\t", header = TRUE),
               "txt" = {
                 # Detect separator for .txt files
                 first_line <- readLines(file_input$datapath, n = 1)
                 if (grepl("\t", first_line)) {
                   read.table(file_input$datapath, check.names = FALSE, sep = "\t", header = TRUE)
                 } else {
                   read.table(file_input$datapath, check.names = FALSE, sep = " ", header = TRUE)
                 }
               },
               stop("Unsupported file type"))

  # Set column names for the guide library if applicable
  if (file_ext %in% c("tsv", "csv", "txt")) {
    #colnames(df) <- c("sgRNA_ID", "sgRNA_sequence", "gene_ID")  # Assigning appropriate names
  }
  df
}

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
                                                                            fileInput("upload", label = "", accept = c('.fastq', 'fastq.gz'), multiple = TRUE)
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
                                                           h4("Sample information"),
                                                           DT::dataTableOutput("dataTableSamples")
                                          ),
                                          conditionalPanel(condition = "input.count_matrix_yes_no == 'no'",
                                                           hr(),
                                                           h4("Here are the uploaded Fastq files"),
                                                           verbatimTextOutput("fastqFilesOutput"),
                                                           actionButton("gotocountingfastq", "Go to Counting")  # Button to go to counting
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
                                          textOutput("dgelist_status"),  # Display status for DGEList creation
                                          verbatimTextOutput("dge_list_output")  # Output for DGEList
                                        )
                                      )
                             ),
                             tabPanel("Differential Analysis", h4("Testing counting"), fluidPage(sidebarPanel())),
                             tabPanel("Pathway Analysis", h4("Testing counting"), fluidPage(sidebarPanel()))
                 )
)

# Define server logic
server <- function(input, output, session) {

  # Reactive value to track Fastq file upload
  fastq_uploaded <- reactiveVal(FALSE)

  # Fastq file upload output
  output$fastqFilesOutput <- renderPrint({
    req(input$upload)
    fastq_file_names <- input$upload$name
    fastq_uploaded(TRUE)  # Mark Fastq files as uploaded
    paste("Uploaded Fastq files:", paste(fastq_file_names, collapse = ", "))
  })

  # Render DataTables for uploaded files

  observeEvent(input$viewcounts, {
    output$dataTableCounts <- DT::renderDataTable({
      req(input$uploadcounts)  # Ensure a file is uploaded
      datatable(read_file_and_render(input$uploadcounts))
    })
  })

  observeEvent(input$viewsamples, {
    output$dataTableSamples <- DT::renderDataTable({
      req(input$uploadsamples)  # Ensure a file is uploaded
      datatable(read_file_and_render(input$uploadsamples))
    })
  })

  observeEvent(input$viewguides, {
    output$dataTableGuides <- DT::renderDataTable({
      req(input$uploadguides)  # Ensure a file is uploaded
      datatable(read_file_and_render(input$uploadguides))
    })
  })

  # Handling dynamic UI for group names
  output$groupnames <- renderUI({
    groupnames <- lapply(1:input$num, function(i) {
      textInput(paste0("group", i), label = paste0("Group ", i))
    })
    do.call(tagList, groupnames)
  })

  output$groups <- renderText({
    req(input$num)
    group_values <- sapply(1:input$num, function(i) input[[paste0("group", i)]])
    paste("Groups are:", paste(group_values, collapse = ", "))
  })

  observeEvent(input$gotocountingfastq, {
    updateTabsetPanel(session, "inTabset", selected = "Counting")  # Navigate to Counting tab after Fastq files loaded
  })

  observeEvent(input$gotopreprocessing, {
    updateTabsetPanel(session, "inTabset", selected = "Preprocessing")
  })

  # Counting functionality
  observeEvent(input$guidecounts, {
    output$status <- renderText("Calculating guide counts...")
    req(input$uploadguides)

    # This is just a placeholder. Add logic for the counting process with Rsubread, MAGeCK, etc. later.
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

  # DGEList creation
  observeEvent(input$create_dgelist, {
    req(input$uploadcounts, input$uploadsamples)  # Ensure necessary files are uploaded

    # Create the DGEList object
    dge <- DGEList(counts = read_file_and_render(input$uploadcounts)[,-c(1:2)],
                   genes = read_file_and_render(input$uploadguides),
                   samples = read_file_and_render(input$uploadsamples))
    dge$samples$group <- dge$samples$Groups
    dge$samples$Groups <- NULL
    rownames(dge) <- read_file_and_render(input$uploadcounts)[,1]
    # Print the DGEList output in the Preprocessing tab
    output$dge_list_output <- renderPrint({
      dge  # Display the DGEList object
    })

    output$dgelist_status <- renderText("DGEList created successfully!")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
