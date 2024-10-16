setwd("/Users/giner.g/Documents/Github/CuReSPR/R")
source("global.R")
global_dge <- NULL

# Define UI
ui <- navbarPage(
  "CuReSPR", id = "main", theme = shinytheme("cerulean"),
  tabsetPanel(
    id = "inTabset",
    tabPanel(
      "Data Upload",
      sidebarLayout(
        sidebarPanel(
          h3("Enter the details of the experimental design"),
          helpText(
            "First upload ",
            "your fastq files and assign each fastq file to its group. ",
            "Specify any biological or technical replicates, if they exist, ",
            "and indicate whether the files are paired-end or single-end. ",
            "Review and submit them for the counting step."
          ),
          hr(),
          h4("Upload guide RNA library"),
          helpText("Please upload the guide library."),
          hr(),
          fileInput(
            "uploadguides", label = "",
            accept = c(".tsv", ".csv", ".txt"), multiple = TRUE
          ),
          actionButton("viewguides", "View guide library"),
          hr(),
          hr(),
          h4("Upload sample information"),
          helpText(
            "Please upload the file containing sample information."
          ),
          hr(),
          fileInput(
            "uploadsamples", label = "",
            accept = c(".tsv", ".csv", ".txt"), multiple = TRUE
          ),
          actionButton("viewsamples", "View sample information"),
          hr(),
          radioButtons(
            "count_matrix_yes_no",
            label = "Do you have a count matrix?",
            choices = list("Yes" = "yes", "No" = "no"), selected = ""
          ),
          conditionalPanel(
            condition = "input.count_matrix_yes_no == 'yes'",
            h4("Upload count matrix"),
            helpText("Please upload the count matrix."),
            hr(),
            fileInput(
              "uploadcounts", label = "", 
              accept = c(".tsv", ".csv", ".txt"), multiple = TRUE
            ),
            actionButton("viewcounts", "View count matrix")
          ),
          conditionalPanel(
            condition = "input.count_matrix_yes_no == 'no'",
            h4("Upload fastq files"),
            fileInput(
              "upload", label = "", 
              accept = c(".fastq", "fastq.gz"), multiple = TRUE
            )
          ),
          hr(),
          h4("Set group numbers"),
          helpText("Enter number of groups in your experiment."),
          numericInput("num", label = "", value = 0, min = 0),
          actionButton("nextnum", "Next"),
          conditionalPanel(
            condition = "input.nextnum > 0",
            hr(),
            h4("Assign group names"),
            helpText("Enter names for each group."),
            hr(),
            uiOutput("groupnames"),
            p(textOutput("groups", inline = TRUE)),
            uiOutput("after_upload_button"),
            hr()
          ),
        ),
        mainPanel(
          conditionalPanel(
            condition = "input.viewguides > 0",
            hr(),
            h4("Guide library"),
            DT::dataTableOutput("dataTableGuides")
          ),
          conditionalPanel(
            condition = "input.viewsamples > 0",
            hr(),
            h4("Sample information"),
            DT::dataTableOutput("dataTableSamples")
          ),
          conditionalPanel(
            condition = "input.count_matrix_yes_no == 'no'",
            hr(),
            h4("Here are the uploaded Fastq files"),
            verbatimTextOutput("fastqFilesOutput")
          ),
          conditionalPanel(
            condition = "input.count_matrix_yes_no == 'yes' 
            && input.viewcounts > 0",
            hr(),
            h4("Count matrix"),
            DT::dataTableOutput("dataTableCounts")
          )
        )
      )
    ),
    tabPanel(
      "Counting",
      sidebarLayout(
        sidebarPanel(
          h4("Select method of counting"),
          selectInput(
            "method", "",
            choices = c("Rsubread", "MAGeCK", "WEHI")
          ),
          actionButton("guidecounts", "Get the guide counts")
        ),
        mainPanel(
          conditionalPanel(
            condition = "input.guidecounts > 0",
            uiOutput("dynamic_ui")
          )
        )
      )
    ),
    tabPanel(
      "Preprocessing",
      sidebarLayout(
        sidebarPanel(
          h3("Select preprocessing options"),
          h4("Select software for analysing your screen"),
          selectInput(
            "software", "",
            choices = c("", "MAGeCK", "edgeR"), selected = NULL
          ),
          conditionalPanel(
            condition = "input.software == 'edgeR'",
            h3("Create edgeR data object"),
            actionButton("create_dgelist", "Create DGEList"),
            h3("Check the quality of the experiment"),
            selectInput(
              "quality_check", "",
              choices = c(
                "", "View guides distribution",
                "View guide distribution per gene"
              ),
              selected = NULL
            ),
            conditionalPanel(
              condition = "input.quality_check == 'View guide distribution per gene'", # nolint
              selectizeInput(
                "selected_gene",
                "Select or Enter Gene Symbol:",
                choices = NULL,  # Choices will be updated dynamically
                multiple = FALSE,
                # Single selection (or set to TRUE for multiple selections)
                options = list(
                  placeholder = 'Type to search for a gene...',
                  maxOptions = 1000,  # Number of options to display at once in the dropdown
                  allowEmptyOption = TRUE  # Allow an empty input field before typing
                )
              )
            )
          )
        ),
        mainPanel(
          textOutput("dgelist_status"),
          conditionalPanel(
            condition = "input.quality_check == 'View guides distribution'",
            h3("The distribution of SgRNA numbers"),
            plotOutput("guide_distribution"),
            uiOutput("download_guide_distribution_button")
          ),
          conditionalPanel(
            condition = "input.quality_check == 'View guide distribution per gene'", # nolint
            h3("The distribution of guides per gene"),
            plotOutput("guide_distribution_per_gene"),
            uiOutput("download_guide_distribution_per_gene_button")
          )
        )
      )
    ),
    tabPanel("Differential Analysis",
             h4("Testing counting"),
             fluidPage(sidebarPanel())),
    tabPanel("Pathway Analysis",
             h4("Testing counting"),
             fluidPage(sidebarPanel()))
  )
)

# Define server logic
server <- function(input, output, session) {

  output$after_upload_button <- renderUI({
    req(input$nextnum > 0)  # Ensure input.nextnum is greater than 0
    if (input$count_matrix_yes_no == "no") {
      actionButton("gotocountingfastq", "Go to Counting")
    } else if (input$count_matrix_yes_no == "yes") {
      actionButton("gotopreprocessing", "Go to Preprocessing")
    }
  })

  # Reactive value to track Fastq file upload
  fastq_uploaded <- reactiveVal(FALSE)

  # Fastq file upload output
  output$fastqFilesOutput <- renderPrint({
    req(input$upload)
    fastq_file_names <- input$upload$name
    fastq_uploaded(TRUE)  # Mark Fastq files as uploaded
    paste("Uploaded Fastq files:", 
          paste(fastq_file_names, collapse = ", "))
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
      textInput(paste0("group", i), label = paste0("Groups are: ", i))
    })
    do.call(tagList, groupnames)
  })

  # Defining output variables
  output$groups <- renderText({
    req(input$num)
    group_values <- sapply(1:input$num, 
                           function(i) input[[paste0("group", i)]])
    paste("Groups are:", paste(group_values, collapse = ", "))
  })

  # Switching between tabs
  observeEvent(input$gotocountingfastq, {
    updateTabsetPanel(session, "inTabset", selected = "Counting")
  })

  observeEvent(input$gotopreprocessing, {
    updateTabsetPanel(session, "inTabset", selected = "Preprocessing")
  })

  # Counting functionality
  observeEvent(input$guidecounts, {
    output$status <- renderText("Calculating guide counts...")
    req(input$uploadguides)

    # Placeholder for actual logic; add counting logic later
    count_data <- data.frame(
      sgRNA_ID = c("sgRNA1", "sgRNA2"),
      Count = c(100, 200)  # Sample count data
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
      req(input$guidecounts)
      count_data <- data.frame(
        sgRNA_ID = c("sgRNA1", "sgRNA2"),
        Count = c(100, 200)  # Sample count data
      )
      write.csv(count_data, file, row.names = FALSE)
    }
  )

  # DGEList creation
  edgeR_object <- reactiveVal(NULL)

  observeEvent(input$create_dgelist, {
    req(input$uploadcounts, input$uploadsamples)

    # Create the DGEList object
    dge <- DGEList(
      counts = read_file_and_render(input$uploadcounts)[,-c(1:2)],
      genes = read_file_and_render(input$uploadguides),
      samples = read_file_and_render(input$uploadsamples)
    )
    dge$samples$group <- dge$samples$Groups
    dge$samples$Groups <- NULL
    rownames(dge) <- read_file_and_render(input$uploadcounts)[, 1]
    edgeR_object(dge)  # Set the edgeR DGEList object
    output$dgelist_status <- renderText("DGEList created successfully!")

    output$dge_list_output <- renderPrint({
      req(edgeR_object())
      edgeR_object()
    })
  })

  # Dynamically update gene_ids based on the reactive edgeR_object
  observe({
    req(edgeR_object())  # Ensure edgeR_object exists

    # Extract gene IDs from the DGEList object
    gene_ids <- unique(edgeR_object()$genes$Gene_ID)  # Assuming Gene_IDs are in rownames

    # Dynamically update the selectInput with gene IDs
    updateSelectizeInput(session,
      "selected_gene",
      choices = gene_ids,
      server = TRUE
    )
  })

  # Display quality check outputs
  ## Display 'View guide distribution'
  guide_pdf <- function() {
    req(edgeR_object())
    barplot(
      table(table(edgeR_object()$genes$Gene_ID)),
      xlab = "Number of sgRNA per gene",
      main = "The distribution of sgRNA per gene",
      col = "#CD534CFF", xlim = c(0, 6.4), ylab = "frequency",
      border = NA
    )
  }

  output$guide_distribution <- renderPlot({
    if (!is.null(guide_pdf())) guide_pdf()
  })

  output$download_guide_distribution <- downloadHandler(
    filename = function() {
      paste("guide-distribution", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file)
      guide_pdf()
      dev.off()
    }
  )

  output$download_guide_distribution_button <- renderUI({
    req(guide_pdf())
    downloadButton("download_guide_distribution")
  })

## Display view guide distribution per gene line plots
gene.linePlot <- function(gene = NULL) {
  req(edgeR_object())
  req(input$selected_gene)
  gene <- input$selected_gene
  # If no gene is provided, use the first gene in the dataset
  if (is.null(gene)) {
    gene <- edgeR_object()$genes$Gene_ID[1]  # Select the first gene by default
  }

  # Filter the genes to find the selected gene
  gene_rows <- edgeR_object()$genes$Gene_ID == gene
  if (any(gene_rows)) {
    # Subset the counts for the specified gene
    pg <- edgeR_object()$counts[gene_rows, ]

    # Reshape the data for plotting
    df <- reshape2::melt(pg)
    
    # Rename columns for clarity
    colnames(df) <- c("SgRNA_Sequence", "Condition", "Abundance")

    # Create line plot using ggplot
    p <- ggplot(data = df, aes(x = Condition, y = Abundance, group = SgRNA_Sequence)) +
      geom_line(aes(color = SgRNA_Sequence)) +
      theme_classic() +
      ggtitle(paste0("Gene ", gene, " SgRNA Distribution"))

    # Return the plot object
    return(p)
  } else {
    return(NULL)
  }
}

output$guide_distribution_per_gene <- renderPlot({
  # No need to require input$selected_gene
  p <- gene.linePlot()  # Default behavior if no gene is provided
  if (!is.null(p)) {
    print(p)
  } else {
    plot.new()
    text(0.5, 0.5, "No data available for the selected gene.", cex = 1.5)
  }
})

output$download_guide_distribution_per_gene <- downloadHandler(
  filename = function() {
    paste("guide-distribution-", input$selected_gene, "-", Sys.Date(), ".pdf", sep = "")
  },
  content = function(file) {
    pdf(file)
    gene.linePlot(input$selected_gene)
    dev.off()
  }
)

output$download_guide_distribution_per_gene_button <- renderUI({
  req(gene.linePlot())
  downloadButton("download_guide_distribution_per_gene")
})
}

# Run the app
shinyApp(ui = ui, server = server)
