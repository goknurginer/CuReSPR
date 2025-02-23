# app.R

setwd("/Users/giner.g/Documents/Github/CuReSPR/R")
source("global.R")

# Define UI
ui <- navbarPage("CuReSPR",
                 id = "main",
                 theme = shinytheme("cerulean"),
                 tabPanel("Data Upload",
                          sidebarLayout(
                            sidebarPanel(
                              # Add footer with text and image (positioned at the top of the page)
                              tags$div(
                                style = "position: fixed; top: 0; right: 0; width: auto; text-align: center; background-color: #428bca; padding: 10px; z-index: 1000; color: white;",
                                tags$span("This website is supported by     "),
                                tags$img(src = "cass.png", height = "30px", width = "auto")
                              ),
                              # Modal 1: Enter Experimental Design
                              h3(tags$a("Enter the details of the experiment",
                                        href = "#", onclick = "Shiny.setInputValue('showModal1', true);"
                                        )),
                              helpText("Click the title above for detailed instructions."),
                              # Modal 2: Upload guide RNA library
                              h3(tags$a(
                                "1. Upload guide RNA library",
                                href = "#", onclick = "Shiny.setInputValue('showModal2', true);"
                              )),
                              helpText("Click the title above for detailed instructions."),
                              fileInput(
                                "uploadguides", label = "",
                                accept = c(".tsv", ".csv", ".txt"), multiple = TRUE
                              ),
                              checkboxInput("example_guide_library", "Use example guide library", FALSE),
                              actionButton("viewguides", "View guide library"),
                              # Modal 3: Upload guide RNA library
                              h3(tags$a(
                                "2. Upload sample information",
                                href = "#", onclick = "Shiny.setInputValue('showModal3', true);"
                              )),
                              helpText("Click the title above for detailed instructions."),
                              fileInput(
                                "uploadsamples", label = "",
                                accept = c(".tsv", ".csv", ".txt"), multiple = TRUE
                              ),
                              checkboxInput("example_sample_info", "Use example sample information", FALSE),
                              actionButton("viewsamples", "View sample information"),
                              hr(),
                              radioButtons("count_matrix_yes_no",
                                           label = "Do you have a count matrix?",
                                           choices = list("Yes" = "yes", "No" = "no"), selected = ""
                              ),
                              conditionalPanel(
                                condition = "input.count_matrix_yes_no == 'yes'",
                                # Modal 4: Upload count matrix
                                h3(tags$a(
                                  "3. Upload count matrix",
                                  href = "#", onclick = "Shiny.setInputValue('showModal4', true);"
                                )),
                                helpText("Click the title above for detailed instructions."),
                                fileInput(
                                  "uploadcounts", label = "",
                                  accept = c(".tsv", ".csv", ".txt"), multiple = TRUE
                                ),
                                checkboxInput("example_count_matrix", "Use example count matrix", FALSE),
                                actionButton("viewcounts", "View count matrix")
                              ),
                              conditionalPanel(
                                condition = "input.count_matrix_yes_no == 'no'",
                                # Modal 5: Upload fastq files
                                h3(tags$a(
                                  "4. Upload fastq files",
                                  href = "#", onclick = "Shiny.setInputValue('showModal5', true);"
                                )),
                                helpText("Click the title above for detailed instructions."),
                                fileInput(
                                  "upload", label = "",
                                  accept = c(".fastq", "fastq.gz"), multiple = TRUE
                                ),
                                checkboxInput("example_fastq_files", "Use example fastq files", FALSE),
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
                                condition = "input.count_matrix_yes_no == 'no' && !is.null(input$upload)",
                                hr(),
                                h4("Here are the uploaded Fastq files"),
                                verbatimTextOutput("fastqFilesOutput"),
                                uiOutput("datauploadnext")
                              ),
                              conditionalPanel(
                                condition = "input.count_matrix_yes_no == 'yes' && input.viewcounts > 0",
                                hr(),
                                h4("Count matrix"),
                                DT::dataTableOutput("dataTableCounts"),
                                uiOutput("datauploadnext")
                              )
                            )
                          )
                 ),
                 tabPanel("Counting",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Select method of counting"),
                              selectInput(inputId = "method",
                                          label = "",
                                          choices = c("Rsubread", "MAGeCK", "WEHI")
                              ),
                              actionButton(inputId = "guidecounts", "Get the guide counts")
                            ),
                            mainPanel(
                              conditionalPanel(
                                condition = "input.guidecounts > 0",
                                uiOutput("download_counts")
                              )
                            )
                          )
                 ),

  # Preprocessing Tab with Sidebar
  tabPanel("Preprocessing",
    sidebarLayout(
      sidebarPanel(
        # Select Software for Analyzing the Screen
        h4("Select Software for Analyzing Your Screen"),
        selectInput(
          "software", "",
          choices = c("", "MAGeCK", "edgeR"), selected = NULL
        ),

        # Select Method Sub-Tab
        conditionalPanel(
          condition = "input.software == 'edgeR'",
          h3("Create edgeR Data Object"),
          actionButton("create_dgelist", "Create DGEList"),
          h3("Check the Quality of the Experiment"),
          selectInput(
            "quality_check", "",
            choices = c(
              "", "View guides distribution",
              "View guide distribution per gene",
              "View gene abundances across samples"
            ),
            selected = NULL
          ),
          conditionalPanel(
            condition = "input.quality_check == 'View guide distribution per gene'",
            selectizeInput(
              "selected_gene",
              "Select or Enter Gene Symbol:",
              choices = NULL,
              multiple = FALSE,
              options = list(
                placeholder = 'Type to search for a gene...',
                maxOptions = 1000,
                allowEmptyOption = TRUE
              )
            )
          ),
          conditionalPanel(
            condition = "input.quality_check == 'View gene abundances across samples'",
            selectizeInput(
              "selected_gene1",
              "Select or Enter Gene Symbol:",
              choices = NULL,
              multiple = FALSE,
              options = list(
                placeholder = 'Type to search for a gene...',
                maxOptions = 1000,
                allowEmptyOption = TRUE
              )
            )
          )
        ),

        conditionalPanel(
          condition = "input.software == 'MAGeCK'",
          actionButton("create_dgelist", "Create DGEList"),
          h3("Check the Quality of the Experiment"),
          selectInput(
            "quality_check", "",
            choices = c(
              "", "View guides distribution",
              "View guide distribution per gene",
              "View gene abundances across samples"
            ),
            selected = NULL
          ),
          conditionalPanel(
            condition = "input.quality_check == 'View guide distribution per gene'",
            selectizeInput(
              "selected_gene",
              "Select or Enter Gene Symbol:",
              choices = NULL,
              multiple = FALSE,
              options = list(
                placeholder = 'Type to search for a gene...',
                maxOptions = 1000,
                allowEmptyOption = TRUE
              )
            )
          ),
          conditionalPanel(
            condition = "input.quality_check == 'View gene abundances across samples'",
            selectizeInput(
              "selected_gene1",
              "Select or Enter Gene Symbol:",
              choices = NULL,
              multiple = FALSE,
              options = list(
                placeholder = 'Type to search for a gene...',
                maxOptions = 1000,
                allowEmptyOption = TRUE
              )
            )
          )
        ),

        # Filtering Section
        h4("Filtering Options"),
        helpText("The filtering options may take up to 1 minute to load. Please don't leave this page."),
        uiOutput("filter3_drop_down"),
        conditionalPanel(
          condition = "typeof input.filter3_what !== 'undefined' && input.filter3_what.length > 0",
          selectInput("filter_what", label = h4("Filter Type"),
                      choices = list("No Filter" = "none",
                                     "Filter Out all Zeros Counts" = "zero",
                                     "edgeR FilterByExpr" = "edgeR",
                                     "New Filter" = "filter3")),
          checkboxInput("all_filters", label = "Show results for all filters", value = FALSE),
          checkboxInput("show_mds", label = "Show MDS plot", value = FALSE)
        ),

        # Normalisation Section
        h4("Normalisation Options"),
        selectInput("norm_what", label = h4("Normalisation Type"),
                    choices = list("TMM",
                                   "TMMwsp",
                                   "RLE",
                                   "upperquartile",
                                   "none")),
        uiOutput("norm_quant"),
        checkboxInput("all_norms", label = "Show results for all normalisation methods", value = FALSE),
        checkboxInput("imputation", label = "Impute before normalisation", value = FALSE)
      ),

      mainPanel(
        textOutput("dgelist_status"),
        verbatimTextOutput("dge_list_output"),

        # Conditional panels based on quality check selection
        conditionalPanel(
          condition = "input.quality_check == 'View guides distribution'",
          h3("The distribution of SgRNA numbers"),
          plotOutput("guide_distribution"),
          uiOutput("download_guide_distribution_button")
        ),
        conditionalPanel(
          condition = "input.quality_check == 'View guide distribution per gene'",
          h3("The distribution of guides per gene"),
          plotOutput("guide_distribution_per_gene"),
          uiOutput("download_guide_distribution_per_gene_button")
        ),
        conditionalPanel(
          condition = "input.quality_check == 'View gene abundances across samples'",
          h4("The distribution of gene abundances"),
          plotOutput("gene_abundance_distribution"),
          uiOutput("download_gene_abundance_distribution_button")
        ),

        # Filtering results
        conditionalPanel(
          condition = "! output.fileUploaded",
          helpText("Please upload your data under 'Data Quality Check'")
        ),
        conditionalPanel(
          condition = "input.all_filters == 0 & input.show_mds == 0 & input.filter3_what.length > 0",
          fluidRow(
            splitLayout(cellWidths = c("50%", "50%"), plotOutput("raw_density"), plotOutput("filtered_density")),
            splitLayout(cellWidths = c("50%", "50%"), uiOutput("download_raw_dens_button"), uiOutput("download_filtered_dens_button"))
          )
        ),
        conditionalPanel(
          condition = "input.all_filters == 0 & input.show_mds == 1",
          fluidRow(
            splitLayout(cellWidths = c("50%", "50%"), plotOutput("raw_mds"), plotOutput("filtered_mds")),
            splitLayout(cellWidths = c("50%", "50%"), uiOutput("download_raw_mds_plot_button"), uiOutput("download_filtered_mds_plot_button"))
          )
        ),
        conditionalPanel(
          condition = "input.all_filters == 1 & input.show_mds == 0",
          fluidRow(
            splitLayout(cellWidths = c("50%", "50%"), plotOutput("raw_density2"), plotOutput("filtered_density_zero")),
            splitLayout(cellWidths = c("50%", "50%"), plotOutput("filtered_density_edgeR"), plotOutput("filtered_density_f3")),
            uiOutput("download_all_dens_button")
          )
        ),
        conditionalPanel(
          condition = "input.all_filters == 1 & input.show_mds == 1",
          fluidRow(
            splitLayout(cellWidths = c("50%", "50%"), plotOutput("raw_mds2"), plotOutput("zero_mds_plot")),
            splitLayout(cellWidths = c("50%", "50%"), plotOutput("edgeR_mds_plot"), plotOutput("f3_mds_plot")),
            uiOutput("download_all_mds_button")
          )
        ),
        uiOutput("to_norm"),

        # Normalisation results
        conditionalPanel(
          condition = "! output.fileUploaded",
          helpText("Please upload your data under 'Data Quality Check'")
        ),
        conditionalPanel(
          condition = "input.all_norms == 0",
          fluidRow(
            splitLayout(cellWidths = c("50%", "50%"), plotOutput("norm_dge_none"), plotOutput("normed_dge")),
            splitLayout(cellWidths = c("50%", "50%"), uiOutput("download_none_dge_box_button"), uiOutput("download_normed_dge_box_button"))
          )
        ),
        conditionalPanel(
          condition = "input.all_norms == 1",
          fluidRow(
            splitLayout(cellWidths = c("50%", "50%"), plotOutput("norm_box_none"), plotOutput("norm_box_tmm")),
            splitLayout(cellWidths = c("50%", "50%"), plotOutput("norm_box_tmmwsp"), plotOutput("norm_box_RLE")),
            splitLayout(cellWidths = c("50%", "50%"), plotOutput("norm_box_quant")),
            uiOutput("download_all_norm_button")
          )
        ),
        uiOutput("to_dim")
      )
    )
  ),
tabPanel("Analysis",
  sidebarLayout(
    sidebarPanel(
      # Fitting Model Section
      h4("Fitting Model Options"),
      checkboxInput("bcv", label = "Show BCV plot", value = FALSE),
      helpText("It can take up to 2 minutes to generate the BCV plot or fit the model. Once you select this checkbox, please don't leave this page until you see the result."),
      selectInput("model", label = h4("Select model"),
                  choices = list("Ebayes", "Generalised Linear Model", "Generalised Linear Model (Quasi Likelihood)")),

      # DE Genes Section
      h4("Differential Expression Genes"),
      selectInput("DE_check", label = NULL,
                  choices = list("Check DE genes in each contrast",
                                 "Compare DE genes in different contrasts")
      ),
      conditionalPanel(
        condition = "input.DE_check == 'Check DE genes in each contrast'",
        uiOutput("ind_contrast")
      ),
      conditionalPanel(
        condition = "input.DE_check == 'Compare DE genes in different contrasts'",
        uiOutput("contrast1"),
        uiOutput("contrast2")
      ),

      # Gene Set Test Section
      h4("Gene Set Test"),
      p("For the gene test, we focus on genes that have a number of guides greater than or equal to the threshold:"),
      numericInput("gene_threshold", label = h4("Enter threshold"), value = 3, step = 1, min = 2),
      uiOutput("gene_set_contrast"),
      uiOutput('gene_set_down')
    ),
    mainPanel(
      # Fitting Model Outputs
      h3("Fitting Model"),
      conditionalPanel(condition = "input.bcv == '1'",
                       plotOutput("plot_bcv_disp"),
                       uiOutput("download_bcv_button")
      ),
      conditionalPanel(condition = "! output.fileUploaded",
                       helpText("Please upload your data under 'Data Quality Check'")
      ),
      conditionalPanel(condition = "input.model == 'Ebayes'",
                       dataTableOutput("limma_table"),
                       uiOutput("download_limma_table_button")
      ),
      conditionalPanel(condition = "input.model == 'Generalised Linear Model'",
                       dataTableOutput("edgeR_table_lrt"),
                       uiOutput("download_lrt_table_button")
      ),
      conditionalPanel(condition = "input.model == 'Generalised Linear Model (Quasi Likelihood)'",
                       dataTableOutput("edgeR_table_qlf"),
                       uiOutput("download_qlf_table_button")
      ),
      uiOutput("to_de"),

      # DE Genes Outputs
      h3("Examine Differentially Expressed Genes"),
      conditionalPanel(condition = "! output.fileUploaded",
                       helpText("Please upload your data under 'Data Quality Check'")
      ),
      conditionalPanel(condition = "input.model == 'Ebayes' && input.DE_check == 'Compare DE genes in different contrasts'",
                       plotOutput("limma_ven"),
                       uiOutput("download_limma_ven_button"),
                       dataTableOutput("limma_com_gene"),
                       uiOutput("download_limma_com_button")
      ),
      conditionalPanel(condition = "input.model == 'Ebayes' && input.DE_check == 'Check DE genes in each contrast'",
                       fluidRow(plotOutput("limma_md")),
                       uiOutput("download_limma_md_button"),
                       plotOutput("limma_heatmap", height = 700),
                       uiOutput("download_limma_heatmap_button")
      ),
      conditionalPanel(
        condition = "input.model == 'Generalised Linear Model' && input.DE_check == 'Compare DE genes in different contrasts'",
        plotOutput("edgeR_ven_lrt"),
        uiOutput("download_lrt_ven_button"),
        dataTableOutput("edgeR_com_gene_lrt"),
        uiOutput("download_lrt_com_button")
      ),
      conditionalPanel(
        condition = "input.model == 'Generalised Linear Model (Quasi Likelihood)' && input.DE_check == 'Compare DE genes in different contrasts'",
        plotOutput("edgeR_ven_qlf"),
        uiOutput("download_qlf_ven_button"),
        dataTableOutput("edgeR_com_gene_qlf"),
        uiOutput("download_qlf_com_button")
      ),
      conditionalPanel(condition = "input.model == 'Generalised Linear Model' && input.DE_check == 'Check DE genes in each contrast'",
                       plotOutput("edgeR_md_lrt"),
                       uiOutput("download_lrt_md_button"),
                       plotOutput("edgeR_heatmap_lrt", height = 800),
                       uiOutput("download_lrt_heatmap_button")
      ),
      conditionalPanel(condition = "input.model == 'Generalised Linear Model (Quasi Likelihood)' && input.DE_check == 'Check DE genes in each contrast'",
                       plotOutput("edgeR_md_qlf"),
                       uiOutput("download_qlf_md_button"),
                       plotOutput("edgeR_heatmap_qlf", height = 800),
                       uiOutput("download_qlf_heatmap_button")
      ),
      uiOutput("to_gene_set"),

      # Gene Set Test Outputs
      h3("Gene Set Test"),
      textOutput("gene_set_fail"),
      dataTableOutput("gene_set_table"),
      uiOutput("download_gene_set_table_button"),
      conditionalPanel(condition = "input.model == 'Ebayes'",
                       plotOutput("limma_barcode_plot"),
                       uiOutput("download_limma_barcode_plot_button")
      ),
      conditionalPanel(condition = "input.model == 'Generalised Linear Model'",
                       plotOutput("lrt_barcode_plot"),
                       uiOutput("download_lrt_barcode_plot_button")
      ),
      conditionalPanel(
        condition = "input.model == 'Generalised Linear Model (Quasi Likelihood)'",
        plotOutput("qlf_barcode_plot"),
        uiOutput("download_qlf_barcode_plot_button")
      )
    )
  )
)

)

# Define server logic
server <- function(input, output, session) {
  # Modals ####
  # Modal for Step 1
  observeEvent(input$showModal1, {
    showModal(modalDialog(
      title = "Enter the details of the experiment",
      "Please upload your guide library, sample information and count matrix or fastq files.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    runjs('Shiny.setInputValue("showModal1", null)')  # Reset the input value
  })
  # Modal for Step 2
  observeEvent(input$showModal2, {
    showModal(modalDialog(
      title = "Guide RNA Library Details",
       "Please make sure that the file you're uploading follows the required format.
       The column names in your file should be:
       - 'SgRNA_ID' (the identifier for each guide),
       - 'SgRNA_Sequence' (the sequence of the guide RNA),
       - 'Gene_ID' (the identifier for the gene targeted by the guide).",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    runjs('Shiny.setInputValue("showModal2", null)')  # Reset the input value
  })

  observeEvent(input$showModal3, {
    showModal(modalDialog(
      title = "Sample Information Upload Details",
         "Please make sure that the file you're uploading follows the required format.
         The column names in your file should be:
         - 'Fastqnames' (the name of the fastq file),
         - 'Groups' (the experimental groups),
         - 'Biorep' (biological replicate),
         - 'Techrep' (technical replicate).",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    runjs('Shiny.setInputValue("showModal3", null)')  # Reset the input value to allow multiple clicks
  })

  observeEvent(input$showModal4, {
    showModal(modalDialog(
      title = "Count Matrix Upload Details",
         "Please make sure that the file you're uploading follows the required format.
         The column names in your file should be:
         - 'SgRNA_Sequence' (the sequence of the guide RNA),
         - 'Gene_ID' (the gene ID),
         - Sample names (the expression values across the samples).",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    runjs('Shiny.setInputValue("showModal4", null)')  # Reset the input value to allow multiple clicks
  })

  observeEvent(input$showModal5, {
    showModal(modalDialog(
      title = "Fastq Files Upload Details",
         "Please make sure that the files you're uploading are in the correct format.
         Ensure that the fastq files are named properly and that they are paired if necessary.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))

    # Reset input after modal is shown (this allows multiple clicks)
    runjs('Shiny.setInputValue("showModal5", null)')  # Reset the input value to allow multiple clicks
  })
  #Functions ####
  # x labels: group names + Biorep + Techrep
  get_x_lab <- function(dge){
    x_lab <- dge$samples$group
    # check if there are biological replicates
    if ("Biorep"  %in% names(dge$samples) & length(unique(dge$samples$Biorep)) >1){
      x_lab <- paste(x_lab, dge$samples$Biorep, sep = ".")
    }
    if ("Techrep"  %in% names(dge$samples) & length(unique(dge$samples$Techrep)) >1){
      x_lab <- paste(x_lab, dge$samples$Techrep, sep = ".")
    }
    return(x_lab)
  }

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

  gene.linePlot <- function() {
    req(edgeR_object())
    req(input$selected_gene)
    gene <- input$selected_gene
    gene_rows <- edgeR_object()$genes$Gene_ID == gene # Filter the genes to find the selected gene
    if (any(gene_rows)) {
      pg <- edgeR_object()$counts[gene_rows, ] # Subset the counts for the specified gene
      df <- reshape2::melt(pg)
      colnames(df) <- c("SgRNA_Sequence", "Sample", "Abundance")
      p <- ggplot(data = df, aes(x = Sample, y = Abundance, group = SgRNA_Sequence)) +
        geom_line(aes(color = SgRNA_Sequence)) +
        theme_classic() +
        ggtitle(paste0("Gene ", gene, " SgRNA Distribution"))
      return(p)
    } else {
      return(NULL)
    }
  }

  gene_dist_plot <- function() {
    req(edgeR_object())
    req(input$selected_gene1)
    gene <- input$selected_gene1
    gene_rows <- edgeR_object()$genes$Gene_ID == gene
    if (any(gene_rows)) {
      pg <- edgeR_object()$counts[gene_rows, ]
      gene_counts <- colSums(pg) # Sum counts across guides for the gene in each sample
      barplot(gene_counts,
              xlab = "Samples",
              main = paste("Guide abundance of gene ", gene, " across samples", sep = ""),
              col = "#CD534CFF",
              ylab = "Abundance",
              names.arg = names(gene_counts),
              las = 2,  # Rotate sample labels if necessary
              border = NA)
    }
  }
  #Switching between tabs ####
  observeEvent(input$gotocounting, {
    updateTabsetPanel(session, "main", "Counting")
  })
  observeEvent(input$gotopreprocessing, {
    updateTabsetPanel(session, "main", "Select method")
  })
  observeEvent(input$deDone, {
    updateTabsetPanel(session,  "main", "Gene set test")
  })
  observeEvent(input$bcvDone, {
    updateTabsetPanel(session,  "main", "Fitting model")
  })
  observeEvent(input$modDone, {
    updateTabsetPanel(session,  "main", "DE genes")
  })
  observeEvent(input$filterDone, {
    updateTabsetPanel(session,  "main", "Normalisation")
  })
  observeEvent(input$normDone, {
    updateTabsetPanel(session,  "main", "Fitting model")
  })
  #Dynamically generate UI elements ####
  output$datauploadnext <- renderUI({
    req(input$count_matrix_yes_no)
    if (input$count_matrix_yes_no == "no") {
      actionButton("gotocounting", "Go to Counting")
    } else if (input$count_matrix_yes_no == "yes") {
      actionButton("gotopreprocessing", "Go to Preprocessing")
    }
  })

  output$download_counts <- renderUI({  # download counting output button
    if (input$guidecounts) {
      tagList(
        DT::dataTableOutput("count_table"),
        downloadButton("download", "Download"),
        textOutput("status")
      )
    }
  })
  output$download_guide_distribution_button <- renderUI({
    req(guide_pdf())
    downloadButton("download_guide_distribution")
  })
  output$download_guide_distribution_per_gene_button <- renderUI({
    req(gene.linePlot())
    downloadButton("download_guide_distribution_per_gene")
  })
  output$download_gene_abundance_distribution_button <- renderUI({
    req(gene_dist_plot())
    downloadButton("download_gene_abundance_distribution")
  })
  output$to_norm <- renderUI({
    filter <- " "
    if (input$filter_what == "none") filter <- "No Filter"
    if (input$filter_what == "zero") filter <- "All Zeros Filtered"
    if (input$filter_what == "edgeR") filter <- "edgeR FilterByExpr"
    if (input$filter_what == "filter3") filter <- "New Filter"
    actionButton(inputId = "filterDone", label = paste("Proceed to Normalisation with ", filter, sep = ""))
  })
  output$to_dim <- renderUI({
    actionButton(inputId = "normDone", label = paste("Proceed to Analysis with ", input$norm_what, sep = ""))
  })
  #Dynamically update gene_ids based on the reactive edgeR_object ####
  observe({
    req(edgeR_object())
    # Extract gene IDs from the DGEList object
    gene_ids <- unique(edgeR_object()$genes$Gene_ID)
    # Update selectizeInput for 'selected_gene' (View guide distribution per gene)
    updateSelectizeInput(session,
                         "selected_gene",
                         choices = gene_ids,
                         server = TRUE
    )
    # Update selectizeInput for 'selected_gene1' (View gene abundances across samples)
    updateSelectizeInput(session,
                         "selected_gene1",
                         choices = gene_ids,
                         server = TRUE
    )
  })
  #Render DataTables for uploaded files ####
  count_data <- reactive({
    #Use example file if checkbox is selected
    if (input$example_count_matrix) {
      read_file_and_render(example_file_path = "/Users/giner.g/Documents/Github/CuReSPR/exampledata/counts.tsv")
    } else if (!is.null(input$uploadcounts)) {
      #If user uploads a file, read that
      read_file_and_render(file_input = input$uploadcounts)
    } else {
      NULL
    }
  })

  observeEvent(input$viewcounts, {
    output$dataTableCounts <- DT::renderDataTable({
      datatable(count_data())
    })
  })

  sample_data <- reactive({
    #Use example file if checkbox is selected
    if (input$example_sample_info) {
      read_file_and_render(example_file_path = "/Users/giner.g/Documents/Github/CuReSPR/exampledata/samples.csv")
    } else if (!is.null(input$uploadsamples)) {
      #If user uploads a file, read that
      read_file_and_render(file_input = input$uploadsamples)
    } else {
      NULL
    }
  })

  observeEvent(input$viewsamples, {
    output$dataTableSamples <- DT::renderDataTable({
      datatable(sample_data())
    })
  })

  guide_data <- reactive({
    #Use example file if checkbox is selected
    if (input$example_guide_library) {
      read_file_and_render(example_file_path = "/Users/giner.g/Documents/Github/CuReSPR/exampledata/guide_library.csv")
    } else if (!is.null(input$uploadguides)) {
      #If user uploads a file, read that
      read_file_and_render(file_input = input$uploadguides)
    } else {
      #Return NULL if neither option is available
      NULL
    }
  })

  observeEvent(input$viewguides, {
    output$dataTableGuides <- DT::renderDataTable({
      datatable(guide_data())
    })
  })

  #Create reactive values globally ####
  fastq_uploaded <- reactiveVal(FALSE)

  edgeR_object <- reactiveVal(NULL)

  x_lab <- reactive({
    req(edgeR_object())
    return(get_x_lab(edgeR_object()))
  })

  #Downloadhandler functions ####
  output$download <- downloadHandler(
    filename = function() {
      paste0("guide-counts-", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(input$guidecounts)
      count_data <- data.frame(
        sgRNA_ID = c("sgRNA1", "sgRNA2"),
        Count = c(100, 200)
      )
      write.csv(count_data, file, row.names = FALSE)
    }
  )
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
  output$download_guide_distribution_per_gene <- downloadHandler(
    filename = function() {
      paste("guide-distribution-", input$selected_gene, "-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file)
      p <- gene.linePlot()
      if (!is.null(p)) print(p)
      dev.off()
    }
  )
  output$download_gene_abundance_distribution <- downloadHandler(
    filename = function() {
      paste('distribution-of-', input$selected_gene1 ,'-', Sys.Date(), '.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      gene_dist_plot()
      dev.off()
    }
  )
  #RenderPrint, RenderText, RenderDT outputs ####
  output$fastqFilesOutput <- renderPrint({
    req(input$upload)
    if(!is.null(input$upload)){
      fastq_file_names <- input$upload$name
      fastq_uploaded(TRUE)
      paste("Uploaded Fastq files:", paste(fastq_file_names, collapse = ", "))
    }
  })
  output$groups <- renderText({
    req(input$num)
    group_values <- sapply(1:input$num,
                           function(i) input[[paste0("group", i)]])
    paste("Groups are:", paste(group_values, collapse = ", "))
  })
  observeEvent(input$guidecounts, {
    output$status <- renderText("Calculating guide counts...")   # Counting functionality
    req(input$uploadguides)
    count_data <- data.frame( # Placeholder for actual logic; add counting logic later
      sgRNA_ID = c("sgRNA1", "sgRNA2"),
      Count = c(100, 200)
    )
    output$count_table <- renderDT({
      datatable(count_data)
    })
  })
  # Define the reactive DGEList object
  dge_data <- reactive({
    req(input$create_dgelist)  # Ensure the button is clicked

    # Create the DGEList object using the reactive data directly
    dge <- DGEList(
      counts = count_data()[,-c(1:2)],  # Adjust columns as needed (remove first two columns)
      genes = guide_data(),
      samples = sample_data()
    )

    # Clean up the sample metadata (remove the 'Groups' column)
    dge$samples$group <- dge$samples$Groups
    dge$samples$Groups <- NULL

    # Assign row names based on the gene names from the first column of the count matrix
    rownames(dge) <- count_data()[, 1]

    # Store the DGEList object in the edgeR_object reactive value
    edgeR_object(dge)

    return(dge)  # Return the DGEList object (not necessary, but for debugging)
  })

  # Render the DGEList status message after creation
  output$dgelist_status <- renderText({
    req(dge_data())  # Ensure the DGEList object is created before rendering the status
    "DGEList created successfully!"
  })

  # Optionally, you can also show a summary of the DGEList
  output$dge_summary <- renderPrint({
    req(dge_data())  # Ensure DGEList is available before showing the summary
    summary(dge_data())  # Print a summary of the DGEList object
  })

  #Display quality check outputs ####
  ## Display 'View guides distribution'
  output$guide_distribution <- renderPlot({
    if (!is.null(guide_pdf())) guide_pdf()
  })
  ## Display 'View guide distribution per gene' line plots
  output$guide_distribution_per_gene <- renderPlot({
    p <- gene.linePlot()
    if (!is.null(p)) {
      print(p)
    } else {
      plot.new()
      text(0.5, 0.5, "No data available for the selected gene.", cex = 1.5)
    }
  })
  ## Display 'View gene abundances across samples'
  output$gene_abundance_distribution <- renderPlot({
    gene_dist_plot()
  })
  #Filtering ####
  group_col <- reactive({
    return("group")
  })
  group <- reactive({
    if(!is.null(edgeR_object())){
      groups <- edgeR_object()$samples$group
      if(is.factor(groups)) groups <- unfactor(edgeR_object()$samples$group)
      return(groups)
    }
  })
  uncontrolled_group <- reactive({
    return(setdiff(unique(group()), input$filter3_what))
  })
  output$filter3_drop_down <- renderUI({
    req(edgeR_object())
    multiInput("filter3_what",
               label = h4("Select all controlled group for filtering purpose"),
               choices = unique(group())
    )
  })
  filtered_dge_f3 <- reactive({
    if(! is.null(edgeR_object()) &&  !is.null(input$filter_what) &&  !is.null(input$filter3_what)){
      return(filter3_update(edgeR_object(), group_col(), uncontrolled_group()))
    }
  })

  filtered_dge_edgeR <- reactive({
    if(! is.null(edgeR_object()) &&  !is.null(input$filter_what)){
      return(edgeR_filter(edgeR_object(), as.factor(group())))
    }
  })

  filtered_dge_zero <- reactive({
    if(! is.null(edgeR_object()) &&  !is.null(input$filter_what)){
      return(zero_filter(edgeR_object()))
    }
  })

  raw_dens <- function(){
    if(!is.null(edgeR_object()) ){
      dim <- dim(edgeR_object())[1]


      plot_density(edgeR_object(), paste("No Filter (",dim," guides left)", sep = ""))
    }
  }

  output$raw_density <- renderPlot({
    if(!is.null(raw_dens())) raw_dens()
  })

  output$download_raw_dens <- downloadHandler(
    filename = function() {
      paste('raw_density' ,Sys.Date(), '.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      raw_dens()
      dev.off()
    }
  )

  output$download_raw_dens_button <- renderUI({
    req(raw_dens())
    downloadButton("download_raw_dens")
  })

  output$raw_density2 <- renderPlot({
    if(!is.null(raw_dens())) raw_dens()
  })

  filtered_dge <- reactive({
    if( ! is.null(edgeR_object()) &&  !is.null(input$filter_what) &&
        (input$filter_what != "filter3" || !is.null(input$filter3_what)) ){
      if (input$filter_what == "filter3"){
        return(filtered_dge_f3())
      }
      if (input$filter_what == "edgeR"){
        return(filtered_dge_edgeR())
      }
      if (input$filter_what == "zero"){
        return(filtered_dge_zero())
      } else{
        return(edgeR_object())
      }
    }
  })

  filtered_dens_f3 <- function(){
    req(filtered_dge_f3())
    req(input$filter3_what)
    dim <- dim(filtered_dge_f3())
    if (is.null(dim)){
      dim <- 0
    } else{
      dim <- dim[1]
    }


    return(plot_density(filtered_dge_f3(), paste("Filtered by New Filter (",dim," guides left)", sep = "") ))
  }

  filtered_dens_edgeR <- function(){
    req(filtered_dge_edgeR())
    dim <- dim(filtered_dge_edgeR())
    if (is.null(dim)){
      dim <- 0
    } else{
      dim <- dim[1]
    }


    return(plot_density(filtered_dge_edgeR(),
                        paste( "Filtered by edgeR FilterByExpr \n (",dim," guides left)", sep = "")))
  }

  filtered_dens_zero <- function(){
    req(filtered_dge_zero())
    dim <- dim(filtered_dge_zero())
    if (is.null(dim)){
      dim <- 0
    } else{
      dim <- dim[1]
    }


    return(plot_density(filtered_dge_zero(),
                        paste("Filter out all zero counts (",dim," guides left)", sep = "")))
  }
  filtered_dens <- function(){
    if( ! is.null(edgeR_object()) &&  !is.null(input$filter_what) &&
        (input$filter_what != "filter3" || !is.null(input$filter3_what)) ){
      if (input$filter_what == "filter3"){
        filtered_dens_f3()
      }
      if (input$filter_what == "edgeR"){
        filtered_dens_edgeR()
      }
      if (input$filter_what == "zero"){
        filtered_dens_zero()
      }
    }
  }

  output$filtered_density <- renderPlot({
    if(!is.null(filtered_dens())) filtered_dens()
  })

  output$download_filtered_dens <- downloadHandler(
    filename = function() {
      paste('filtered_density-', input$filter_what ,'-' ,Sys.Date(), '.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      filtered_dens()
      dev.off()
    }
  )

  output$download_filtered_dens_button <- renderUI({
    if((input$filter_what == "filter3" && !is.null(filtered_dens_f3())) ||
       (input$filter_what == "edgeR" && !is.null(filtered_dens_edgeR())) ||
       (input$filter_what == "zero" && !is.null(filtered_dens_zero()))
    ) downloadButton("download_filtered_dens")
  })

  output$filtered_density_edgeR <- renderPlot({
    req(filtered_dens_edgeR())
    filtered_dens_edgeR()
  })

  output$filtered_density_f3 <- renderPlot({
    req(filtered_dens_f3())
    filtered_dens_f3()
  })

  output$filtered_density_zero <- renderPlot({
    req(filtered_dens_zero())
    filtered_dens_zero()
  })

  all_dens <- function(){
    par(mfrow=c(2,2))
    raw_dens()
    filtered_dens_zero()
    filtered_dens_edgeR()
    filtered_dens_f3()

  }

  output$download_all_dens <- downloadHandler(
    filename = function(){
      paste('all-filters', Sys.Date(),'.pdf',sep='')
    },
    content = function(file){
      pdf(file, height = 6)
      all_dens()
      dev.off()
    }
  )
  output$download_all_dens_button <- renderUI({
    req(all_dens())
    req(filtered_dens_f3())
    req(filtered_dens_edgeR())
    req(filtered_dens_zero())
    req(raw_dens())
    downloadButton("download_all_dens")
  })

  ## mds plots
  filtered_mds <- function(){
    if( ! is.null(edgeR_object()) &&  !is.null(input$filter_what) &&
        (input$filter_what != "filter3" || !is.null(input$filter3_what)) ){
      if (input$filter_what == "filter3"){

        print(plot_mds(filtered_dge_f3(),paste("MDS filtered by New Filter", sep = "")))
      }
      if (input$filter_what == "edgeR"){

        print(plot_mds(filtered_dge_edgeR(),
                       paste( "MDS filtered by edgeR FilterByExpr", sep = "")))
      }
      if (input$filter_what == "zero"){

        print(plot_mds(filtered_dge_zero(),
                       paste("MDS with all zero counts filtered", sep = "")))
      }
    }
  }

  output$filtered_mds <- renderPlot({
    if(!is.null(filtered_mds())) filtered_mds()
  })

  output$download_filtered_mds_plot <- downloadHandler(
    filename = function() {
      paste('filtered_mds-', input$filter_what ,Sys.Date(), '.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      filtered_mds()
      dev.off()
    }
  )

  output$download_filtered_mds_plot_button <- renderUI({
    req(filtered_dge())
    req(input$filter_what != "none")
    downloadButton("download_filtered_mds_plot")
  })

  raw_mds_plot <- function(){
    if(!is.null(edgeR_object()) ){
      dim <- dim(edgeR_object())[1]

      plot_mds(edgeR_object(), paste("Samples groups MDS with No Filter", sep = ""))
    }
  }

  output$raw_mds <- renderPlot({
    if(!is.null(raw_mds_plot()) )  raw_mds_plot()
  })

  output$download_raw_mds_plot <- downloadHandler(
    filename = function() {
      paste('raw_mds',Sys.Date(), '.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      raw_mds_plot()
      dev.off()
    }
  )

  output$download_raw_mds_plot_button <- renderUI({
    req(edgeR_object())
    downloadButton("download_raw_mds_plot")
  })

  output$raw_mds2 <- renderPlot({
    if(!is.null(edgeR_object()) ){
      dim <- dim(edgeR_object())[1]

      plot_mds(edgeR_object(), paste("Samples groups MDS with No Filter", sep = ""))
    }
  })

  edgeR_mds <- function(){
    req(filtered_dge_edgeR())

    plot_mds(filtered_dge_edgeR(),
             paste( "MDS filtered by edgeR FilterByExpr", sep = ""))

  }
  output$edgeR_mds_plot <- renderPlot({
    req(filtered_dge_edgeR())
    edgeR_mds()
  })

  f3_mds <- function(){
    req(filtered_dge_f3())

    plot_mds(filtered_dge_f3(),paste("MDS filtered by New Filter", sep = "") )
  }

  output$f3_mds_plot <- renderPlot({
    req(filtered_dge_f3())
    f3_mds()
  })

  zero_mds <- function(){
    req(filtered_dge_zero())
    plot_mds(filtered_dge_zero(),
             paste("MDS with all zero counts filtered", sep = ""))
  }

  output$zero_mds_plot <- renderPlot({
    req(filtered_dge_zero())
    zero_mds()
  })

  all_mds <- function(){
    par(mfrow=c(2,2))
    raw_mds_plot()
    zero_mds()
    edgeR_mds()
    f3_mds()
  }

  output$download_all_mds <- downloadHandler(
    filename = function(){
      paste('all-mds', Sys.Date(),'.pdf',sep='')
    },
    content = function(file){
      pdf(file, height = 6)
      all_mds()
      dev.off()
    }
  )

  output$download_all_mds_button <- renderUI({
    req(filtered_dens_f3())
    req(filtered_dens_edgeR())
    req(filtered_dens_zero())
    req(raw_dens())
    downloadButton("download_all_mds")
  })
  ## -------------------- Normalisation --------------------
  norm_dge_tmm <- reactive({
    if( ! is.null(filtered_dge()) ){
      filtered_dge <- filtered_dge()
      if (input$imputation){
        imp.dge <- zero_imp(filtered_dge(),group())
        imp.dge <- calcNormFactors(imp.dge, method = "TMM")
        filtered_dge$samples$norm.factors <- imp.dge$samples$norm.factors
        return(filtered_dge)
      } else{
        return(calcNormFactors(filtered_dge(), method = "TMM"))
      }
    }
  })

  norm_dge_tmmwsp <- reactive({
    if( ! is.null(filtered_dge()) &&  !is.null(input$norm_what) ){
      filtered_dge <- filtered_dge()
      if (input$imputation){
        imp.dge <- zero_imp(filtered_dge(),group())
        imp.dge <- calcNormFactors(imp.dge, method = "TMMwsp")
        filtered_dge$samples$norm.factors <- imp.dge$samples$norm.factors
        return(filtered_dge)
      } else{
        return(calcNormFactors(filtered_dge(), method = "TMMwsp"))
      }
    }
  })

  norm_dge_RLE <- reactive({
    if( ! is.null(filtered_dge()) ){
      filtered_dge <- filtered_dge()
      if (input$imputation){
        imp.dge <- zero_imp(filtered_dge(),group())
        imp.dge <- calcNormFactors(imp.dge, method = "RLE")
        filtered_dge$samples$norm.factors <- imp.dge$samples$norm.factors
        return(filtered_dge)
      } else{
        return(calcNormFactors(filtered_dge(), method = "RLE"))
      }
    }
  })

  norm_dge_quant <- reactive({
    if( ! is.null(filtered_dge()) &&  !is.null(input$norm_what) && !is.null(input$quant_num)){
      filtered_dge <- filtered_dge()
      if (input$imputation){
        imp.dge <- zero_imp(filtered_dge(),group())
        imp.dge <- calcNormFactors(imp.dge, method = "upperquartile", p=input$quant_num)
        filtered_dge$samples$norm.factors <- imp.dge$samples$norm.factors
        return(filtered_dge)
      } else{
        return(calcNormFactors(filtered_dge(), method = "upperquartile", p=input$quant_num))
      }
    }
  })

  norm_dge_none <- reactive({
    if( ! is.null(filtered_dge()) &&  !is.null(input$norm_what)){
      filtered_dge <- filtered_dge()
      if (input$imputation){
        imp.dge <- zero_imp(filtered_dge(),group())
        imp.dge <- calcNormFactors(imp.dge, method = "none")
        filtered_dge$samples$norm.factors <- imp.dge$samples$norm.factors
        return(filtered_dge)
      } else{
        return(calcNormFactors(filtered_dge(), method = "none"))
      }
    }
  })


  output$norm_quant <- renderUI({
    if(!is.null(filtered_dge()) && !is.null(input$norm_what)  &&
       (  input$all_norms ||  (input$norm_what == "upperquartile") ) ){
      numericInput("quant_num", label = h4("Enter quartile for upperquartile"), value = 0.75,
                   min = 0, max=1, step = 0.01)
    }
  })

  output$norm_dge_none <- renderPlot({
    if( ! is.null(norm_dge_none()) &&  !is.null(input$norm_what)){
      plot_boxplot(norm_dge_none(), "Unnormalised Data", x_lab())
    }
  })

  normed_dge_box <- function(){
    if(!is.null(filtered_dge()) && !is.null(input$norm_what)){
      if( input$norm_what == "TMM"){
        return(plot_boxplot(norm_dge_tmm(), "Normalised with TMM", x_lab()))
      }
      if( input$norm_what == "TMMwsp"){
        return(plot_boxplot(norm_dge_tmmwsp(), "Normalised with TMMwsp", x_lab()))
      }
      if( input$norm_what == "upperquartile"){
        return(plot_boxplot(norm_dge_quant(),
                            paste("Normalised with upperquartile with p=",
                                  input$quant_num, sep = "" ), x_lab()))
      }
      if( input$norm_what == "RLE"){
        return(plot_boxplot(norm_dge_RLE(), "Normalised with RLE", x_lab()))
      }
    }
  }

  output$download_all_norm <- downloadHandler(
    filename = function(){
      paste('all-norm', Sys.Date(),'.pdf',sep='')
    },
    content = function(file){
      pdf(file, height = 12)
      par(mfrow=c(3,2))
      plot_boxplot(norm_dge_none(), "Unnormalised Data", x_lab())
      plot_boxplot(norm_dge_tmm(), "Normalised with TMM", x_lab())
      plot_boxplot(norm_dge_tmmwsp(), "Normalised with TMMwsp", x_lab())
      plot_boxplot(norm_dge_RLE(), "Normalised with RLE", x_lab())
      plot_boxplot(norm_dge_quant(),
                   paste("Normalised with upperquartile with p=",
                         input$quant_num, sep = "" ), x_lab())
      dev.off()
    }
  )


  output$download_all_norm_button <- renderUI({
    req(norm_dge_none())
    req(norm_dge_tmm())
    req(norm_dge_tmmwsp())
    req(norm_dge_RLE())
    req(norm_dge_quant())
    req(input$quant_num)
    downloadButton("download_all_norm")
  })

  output$normed_dge <- renderPlot({
    if(!is.null(normed_dge_box())) normed_dge_box()
  })

  output$download_normed_dge_box <- downloadHandler(
    filename = function() {
      paste('normed_boxplot-',input$norm_what,'-',Sys.Date(), '.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      normed_dge_box()
      dev.off()
    }
  )



  output$download_normed_dge_box_button <- renderUI({
    req(normed_dge())
    downloadButton("download_normed_dge_box")
  })

  normed_dge <- reactive({
    if(!is.null(filtered_dge()) && !is.null(input$norm_what)){
      if( input$norm_what == "TMM"){
        return(norm_dge_tmm())
      }
      if( input$norm_what == "TMMwsp"){
        return(norm_dge_tmmwsp())
      }
      if( input$norm_what == "upperquartile"){
        return(norm_dge_quant())
      }
      if( input$norm_what == "RLE"){
        return(norm_dge_RLE())
      }
    }
  })


  output$norm_box_tmm <- renderPlot({
    if(!is.null(norm_dge_tmm())){
      plot_boxplot(norm_dge_tmm(), "Normalised with TMM", x_lab())
    }
  })

  output$norm_box_tmmwsp <- renderPlot({
    if(!is.null(norm_dge_tmmwsp())){
      plot_boxplot(norm_dge_tmmwsp(), "Normalised with TMMwsp", x_lab())
    }
  })

  output$norm_box_RLE <- renderPlot({
    if(!is.null(norm_dge_RLE())){
      plot_boxplot(norm_dge_RLE(), "Normalised with RLE", x_lab())
    }
  })

  output$norm_box_quant <- renderPlot({
    if(!is.null(norm_dge_quant())){
      plot_boxplot(norm_dge_quant(),
                   paste("Normalised with upperquartile with p=",
                         input$quant_num, sep = "" ), x_lab())
    }
  })

  norm_box_none <- function(){
    if( ! is.null(norm_dge_none())){
      return(plot_boxplot(norm_dge_none(), "Unnormalised Data", x_lab()))
    }
  }

  output$norm_box_none <- renderPlot({
    if(! is.null(norm_box_none())){
      norm_box_none()
    }
  })

  output$download_none_dge_box <- downloadHandler(
    filename = function() {
      paste('unnormed_boxplot-',Sys.Date(), '.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      norm_box_none()
      dev.off()
    }
  )

  output$download_none_dge_box_button <- renderUI({
    req(norm_dge_none())
    downloadButton("download_none_dge_box")
  })

  # -------------------- Analysis -------------------------------------
  ## -------------------- BCV --------------------
  design <-reactive({
    if(! is.null(normed_dge())){
      return(get_design(normed_dge()))
    }
  })



  disp_dge <- reactive({
    if(! is.null(normed_dge()) && ! is.null(design())){
      return(estimateDisp(normed_dge(), design()))
    }
  })

  bcv_plot_disp <- function(){
    if(!is.null(disp_dge())){
      #par(mar=c(5,5,5,5))
      return(plotBCV(disp_dge()))
    }
  }

  output$download_bcv <- downloadHandler(
    filename = function() {
      paste('bcv-', Sys.Date(), '.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      (bcv_plot_disp())
      dev.off()
    }
  )

  output$download_bcv_button <- renderUI({
    req(disp_dge())
    downloadButton("download_bcv")
  })


  output$plot_bcv_disp <- renderPlot({
    if(!is.null(bcv_plot_disp())){
      bcv_plot_disp()
    }
  })


  ## Fitting model
  contrast <- reactive({
    if(!is.null(disp_dge())){
      get_contrast_matrix(disp_dge(), design())
    }
  })

  v_dge <- reactive({
    if(!is.null(disp_dge()) && !is.null(design())){
      return(voom(disp_dge(), design(), plot = FALSE))
    }
  })

  efit <- reactive({
    if(!is.null(disp_dge()) && !is.null(design()) && !is.null(contrast())){
      return(limma_pipeline(disp_dge(), design(), contrast()))
    }
  })


  limma_df <- function(){
    if(!is.null(efit())){
      return(limma_table(efit()))
    }
  }

  limma_tab <- function(){
    if(!is.null(limma_df())){
      datatable(limma_df(), rownames = TRUE)
    }
  }



  output$limma_table <-  DT::renderDataTable({
    if(!is.null(limma_tab())) limma_tab()
  })

  output$download_limma_table <- downloadHandler(
    filename = function() {
      paste('limma_table-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(limma_df(), con)
    }
  )

  output$download_limma_table_button <- renderUI({
    req(limma_df())
    downloadButton("download_limma_table")
  })


  lrt <- reactive({
    if(!is.null(disp_dge()) && !is.null(design()) && !is.null(contrast()) )
      return(edgeR_pipeline(disp_dge(), design(), "glm", contrast()))
  })

  qlf <- reactive({
    if(!is.null(disp_dge()) && !is.null(design()) && !is.null(contrast()) )
      return(edgeR_pipeline(disp_dge(), design(), "glmQF", contrast()))
  })

  lrt_tab <- function(){
    if(!is.null(lrt())){
      return(edgeR_table(lrt()))
    }
  }

  output$edgeR_table_lrt <-  DT::renderDataTable({
    if(!is.null(lrt_tab())) datatable(lrt_tab(), rownames = TRUE)
  })

  output$download_lrt_table <- downloadHandler(
    filename = function() {
      paste('lrt_table-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(lrt_tab(), con)
    }
  )

  output$download_lrt_table_button <- renderUI({
    req(lrt_tab())
    downloadButton("download_lrt_table")
  })

  output$download_qlf_table <- downloadHandler(
    filename = function() {
      paste('qlf_table-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(qlf_tab(), con)
    }
  )

  output$download_qlf_table_button <- renderUI({
    req(qlf_tab())
    downloadButton("download_qlf_table")
  })

  qlf_tab <- function(){
    if(!is.null(qlf())){
      return(edgeR_table(qlf()))
    }
  }

  output$edgeR_table_qlf <-  DT::renderDataTable({
    if(!is.null(qlf_tab())) datatable(qlf_tab(), rownames = TRUE)
  })



  output$to_de <- renderUI({
    actionButton(inputId = "modDone", label = paste("Proceed to examine DE genes with ", input$model, sep = ""))
  })

  output$to_gene_set <- renderUI({
    actionButton(inputId = "deDone", label = paste("Proceed to gene set test ", sep = ""))
  })

  observeEvent(input$deDone, {
    updateTabsetPanel(session,  "main", "Gene set test")
  })


  observeEvent(input$bcvDone, {
    updateTabsetPanel(session,  "main", "Fitting model")
  })

  observeEvent(input$modDone, {
    updateTabsetPanel(session,  "main", "DE genes")
  })


  ## Examine DE genes
  output$ind_contrast <- renderUI({
    if(!is.null(contrast())){
      selectInput("which_contrast", label = h4("Select contrast"),
                  choices = colnames(contrast()))
    }
  })

  output$contrast1 <- renderUI({
    if(!is.null(contrast())){
      selectInput("contrast1", label = h4("Select contrast 1"),
                  choices = colnames(contrast()))
    }
  })

  output$contrast2 <- renderUI({
    if(!is.null(contrast()) && !is.null(input$contrast1)){
      selectInput("contrast2", label = h4("Select contrast 2"),
                  choices =setdiff(colnames(contrast()),input$contrast1 ))
    }
  })

  limma_ven <- function(){
    if(!is.null(efit()) && !is.null(input$contrast1) && !is.null(input$contrast2) ){
      get_limma_ven(efit(),input$contrast1, input$contrast2)
    }
  }

  output$limma_ven <- renderPlot({
    if(!is.null(limma_ven())) limma_ven()
  })

  output$download_limma_ven <- downloadHandler(
    filename = function() {
      paste('limma_ven_diagram-', Sys.Date(), '.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      (limma_ven())
      dev.off()
    }
  )

  output$download_limma_ven_button <- renderUI({
    req(efit())
    req(input$contrast1)
    req(input$contrast2)
    downloadButton("download_limma_ven")
  })

  limma_com <- function(){
    if(!is.null(efit()) && !is.null(input$contrast1) && !is.null(input$contrast2) ){
      limma_common(efit(),input$contrast1, input$contrast2)
    }
  }

  output$limma_com_gene <- DT::renderDataTable({
    if (!is.null(limma_com())) limma_com()
  })

  output$download_limma_com <- downloadHandler(
    filename = function() {
      paste('limma_common_gene-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(limma_com(), con)
    }
  )

  output$download_limma_com_button <- renderUI({
    req(limma_com())
    downloadButton("download_limma_com")
  })

  edgeR_ven_lrt <- function(){
    if(!is.null(lrt()) && !is.null(input$contrast1) && !is.null(input$contrast2)
       && !is.null(contrast()) ){
      edgeR_ven(lrt(),input$contrast1, input$contrast2,contrast())
    }
  }

  output$edgeR_ven_lrt <- renderPlot({
    if(!is.null(edgeR_ven_lrt())) edgeR_ven_lrt()
  })

  output$download_lrt_ven <- downloadHandler(
    filename = function() {
      paste('edgeR_lrt_ven_diagram-', Sys.Date(), '.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      edgeR_ven_lrt()
      dev.off()
    }
  )
  output$download_lrt_ven_button <- renderUI({
    req(lrt())
    req(input$contrast1)
    req(input$contrast2)
    downloadButton("download_lrt_ven")
  })


  lrt_com <- function(){
    if(!is.null(lrt()) && !is.null(input$contrast1) && !is.null(input$contrast2) ){
      return(edgeR_common(lrt(),input$contrast1, input$contrast2, disp_dge(),contrast()))
    }
  }

  output$edgeR_com_gene_lrt <- DT::renderDataTable({
    if(!is.null(lrt_com())) lrt_com()
  })

  output$download_lrt_com <- downloadHandler(
    filename = function() {
      paste('lrt_common_gene-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(lrt_com(), con)
    }
  )

  output$download_lrt_com_button <- renderUI({
    req(lrt_com())
    downloadButton("download_lrt_com")
  })

  edgeR_ven_qlf <- function(){
    if(!is.null(qlf()) && !is.null(input$contrast1) && !is.null(input$contrast2)
       && !is.null(contrast()) ){
      edgeR_ven(qlf(),input$contrast1, input$contrast2,contrast())
    }
  }

  output$edgeR_ven_qlf <- renderPlot({
    if(!is.null(edgeR_ven_qlf())) edgeR_ven_qlf()
  })

  output$download_qlf_ven <- downloadHandler(
    filename = function() {
      paste('edgeR_qlf_ven_diagram-', Sys.Date(), '.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      edgeR_ven_qlf()
      dev.off()
    }
  )

  output$download_qlf_ven_button <- renderUI({
    req(qlf())
    req(input$contrast1)
    req(input$contrast2)
    downloadButton("download_qlf_ven")
  })

  qlf_com <- function(){
    if(!is.null(qlf()) && !is.null(input$contrast1) && !is.null(input$contrast2) ){
      return(edgeR_common(qlf(),input$contrast1, input$contrast2, disp_dge(),contrast()))
    }
  }

  output$edgeR_com_gene_qlf <- DT::renderDataTable({
    if(!is.null(qlf_com())) qlf_com()
  })

  output$download_qlf_com <- downloadHandler(
    filename = function() {
      paste('qlf_common_gene-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(qlf_com(), con)
    }
  )

  output$download_qlf_com_button <- renderUI({
    req(qlf_com())
    downloadButton("download_qlf_com")
  })

  limma_md <- function(){
    if(!is.null(efit()) && !is.null(input$which_contrast)){
      get_limma_md(efit(),input$which_contrast)
    }
  }

  output$limma_md <- renderPlot({
    if(!is.null(limma_md())) limma_md()
  })


  output$download_limma_md <- downloadHandler(
    filename = function() {
      paste('limma-md-', input$which_contrast ,Sys.Date(), '.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      limma_md()
      dev.off()
    }
  )

  output$download_limma_md_button <- renderUI({
    req(efit())
    req(input$which_contrast)
    downloadButton("download_limma_md")
  })


  limma_heatmap <- function(){
    if(!is.null(efit()) && !is.null(input$which_contrast)){
      return(get_limma_heatmap(efit(),input$which_contrast, disp_dge()))
    }
  }

  output$limma_heatmap <- renderPlot({
    if(!is.null(limma_heatmap())) limma_heatmap()
  })

  output$download_limma_heatmap <- downloadHandler(
    filename = function() {
      paste('limma-heatmap-', input$which_contrast,Sys.Date(), '.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      limma_heatmap()
      dev.off()
    }
  )

  output$download_limma_heatmap_button <- renderUI({
    req(limma_heatmap())
    downloadButton("download_limma_heatmap")
  })

  lrt_md <- function(){
    if(!is.null(lrt()) && !is.null(input$which_contrast)){
      edgeR_md(lrt(),input$which_contrast, contrast())
    }
  }

  output$edgeR_md_lrt <- renderPlot({
    if(!is.null(lrt_md())) lrt_md()
  })

  output$download_lrt_md <- downloadHandler(
    filename = function() {
      paste('lrt-md-',input$which_contrast, Sys.Date(), '.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      lrt_md()
      dev.off()
    }
  )

  output$download_lrt_md_button <- renderUI({
    req(lrt())
    req(input$which_contrast)
    downloadButton("download_lrt_md")
  })

  lrt_heatmap <- function(){
    if(!is.null(lrt()) && !is.null(input$which_contrast)){
      return(edgeR_heatmap(lrt(),input$which_contrast, disp_dge(), contrast()))
    }
  }

  output$edgeR_heatmap_lrt <- renderPlot({
    if(!is.null(lrt_heatmap())) lrt_heatmap()
  })

  output$download_lrt_heatmap <- downloadHandler(
    filename = function() {
      paste('lrt-heatmap-',input$which_contrast, Sys.Date(), '.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      lrt_heatmap()
      dev.off()
    }
  )

  output$download_lrt_heatmap_button <- renderUI({
    req(lrt_heatmap())
    downloadButton("download_lrt_heatmap")
  })

  qlf_md <- function(){
    if(!is.null(qlf()) && !is.null(input$which_contrast)){
      par(mar=c(0.01,0.01,0.01,0.01))
      edgeR_md(qlf(),input$which_contrast, contrast())
    }
  }

  output$edgeR_md_qlf <- renderPlot({
    if(!is.null(qlf_md())) qlf_md()
  })

  output$download_qlf_md <- downloadHandler(
    filename = function() {
      paste('qlf-md-',input$which_contrast, Sys.Date(), '.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      qlf_md()
      dev.off()
    }
  )

  output$download_qlf_md_button <- renderUI({
    req(qlf())
    req(input$which_contrast)
    downloadButton("download_qlf_md")
  })

  qlf_heatmap <- function(){
    if(!is.null(qlf()) && !is.null(input$which_contrast)){
      return(edgeR_heatmap(qlf(),input$which_contrast, disp_dge(), contrast()))
    }
  }

  output$edgeR_heatmap_qlf <- renderPlot({
    if(!is.null(qlf_heatmap())) qlf_heatmap()
  })

  output$download_qlf_heatmap <- downloadHandler(
    filename = function() {
      paste('qlf-heatmap-',input$which_contrast, Sys.Date(), '.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      qlf_heatmap()
      dev.off()
    }
  )

  output$download_qlf_heatmap_button <- renderUI({
    req(qlf_heatmap())
    downloadButton("download_qlf_heatmap")
  })

  output$gene_set_contrast <- renderUI({
    if(!is.null(contrast())){
      selectInput("gene_set_comp", label = h4("Select contrast"),
                  choices = colnames(contrast()))
    }
  })

  genesymbollist <- reactive({
    if(!is.null(disp_dge())){
      if(is.na(input$gene_threshold)){
        return(NULL)
      } else{
        threshold <- input$gene_threshold
      }
      return(get_gene_symbol_lis(disp_dge(),threshold))
    }
  })

  fry_table <- function(){
    req(genesymbollist())
    if(!is.null(disp_dge()) && !is.null(input$gene_set_comp) && !is.null(contrast()) && !is.null(genesymbollist())){
      return(do_fry(disp_dge(), input$gene_set_comp ,contrast(),genesymbollist() ,design()))
    }
  }

  output$gene_set_table <- DT::renderDataTable(
    #req(genesymbollist())
    if(!is.null(fry_table()) && !is.null(genesymbollist()))   fry_table()
  )

  output$gene_set_fail <- renderText (
    if(is.null(genesymbollist()))   "No genes with guide number above the threshold. Gene set test cannot be done."
  )

  #if(is.null(fry_table()) && is.null(genesymbollist()))
  output$download_gene_set_table <- downloadHandler(
    filename = function() {
      paste('gene-set-table-', input$gene_set_comp ,'-',Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(fry_table(), con)
    }
  )

  output$download_gene_set_table_button <- renderUI({
    req(fry_table())
    if(!is.null(genesymbollist())) downloadButton("download_gene_set_table")
  })


  limma_barcode <- function(){
    if(!is.null(efit()) && !is.null(input$gene_set_comp) && !is.null(input$gene_set_down)
       && !is.null(genesymbollist())  && !is.null(fry_table())){
      if(!is.null(genesymbollist())) get_barcode_limma(efit(), input$gene_set_comp,  input$gene_set_down,genesymbollist())
    }
  }

  output$limma_barcode_plot <- renderPlot({
    req(genesymbollist())
    if(!is.null(limma_barcode())) limma_barcode()
  })

  lrt_barcode <- function(){
    if(!is.null(lrt()) && !is.null(input$gene_set_comp) && !is.null(input$gene_set_down)
       && !is.null(genesymbollist()) && !is.null(fry_table())){
      if(!is.null(genesymbollist())) get_barcode_edgeR(lrt(), input$gene_set_comp, input$gene_set_down,genesymbollist(), contrast())
    }
  }

  output$lrt_barcode_plot <- renderPlot({
    req(genesymbollist())
    if(!is.null(lrt_barcode())) lrt_barcode()
  })

  output$download_lrt_barcode_plot <- downloadHandler(
    filename = function() {
      paste('lrt_barcode-', input$gene_set_comp,'-',input$gene_set_down,'-',Sys.Date(), '.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      (lrt_barcode())
      dev.off()
    }
  )

  output$download_lrt_barcode_plot_button <- renderUI({
    req(lrt())
    req(input$gene_set_comp)
    req(input$gene_set_down)

    downloadButton("download_lrt_barcode_plot")
  })

  qlf_barcode <- function(){
    if(!is.null(qlf()) && !is.null(input$gene_set_comp) && !is.null(input$gene_set_down)
       && !is.null(genesymbollist()) && !is.null(fry_table())){
      if(!is.null(genesymbollist())) get_barcode_edgeR(qlf(), input$gene_set_comp, input$gene_set_down,genesymbollist(), contrast())
    }
  }

  output$qlf_barcode_plot <- renderPlot({
    req(genesymbollist())
    if(!is.null(qlf_barcode())) qlf_barcode()
  })

  output$download_qlf_barcode_plot <- downloadHandler(
    filename = function() {
      paste('qlf_barcode-', input$gene_set_comp,'-',input$gene_set_down,'-',Sys.Date(), '.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      (qlf_barcode())
      dev.off()
    }
  )

  output$download_qlf_barcode_plot_button <- renderUI({
    req(qlf())
    req(input$gene_set_comp)
    req(input$gene_set_down)
    downloadButton("download_qlf_barcode_plot")
  })


  output$limma_barcode_plot <- renderPlot({
    req(genesymbollist())
    if(!is.null(limma_barcode())) limma_barcode()
  })

  # genesymb <- reactiveValues()
  # observeEvent(input$gene_threshold, {
  #   if ( !is.null(genesymbollist())){
  #     genesymb <- names(genesymbollist())
  #     updateSelectizeInput(session, 'gene_set_down', choices = genesymb,
  #                          server = TRUE)
  #   }
  # })

  output$gene_set_down <- renderUI({
    req(genesymbollist())
    selectizeInput('gene_set_down', label = h4("Select gene"), choices = names(genesymbollist()))
  })


  output$download_limma_barcode_plot <- downloadHandler(
    filename = function() {
      paste('limma_barcode-', input$gene_set_comp,'-',input$gene_set_down,'-',Sys.Date(), '.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      (limma_barcode())
      dev.off()
    }
  )

  output$download_limma_barcode_plot_button <- renderUI({
    req(efit())
    req(input$gene_set_comp)
    req(input$gene_set_down)
    downloadButton("download_limma_barcode_plot")
  })

}

# Run the app
shinyApp(ui = ui, server = server)
