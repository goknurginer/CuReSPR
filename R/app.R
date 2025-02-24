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
                              tags$div(
                                style = "position: fixed; top: 0; right: 0; width: auto; text-align: center; background-color: #428bca; padding: 10px; z-index: 1000; color: white;",
                                tags$span("This website is supported by     "),
                                tags$img(src = "cass.png", height = "30px", width = "auto")
                              ),
                              # Modal 1: Enter Experimental Design
                              h2(tags$a("Enter the details of the experiment",
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
                                          # Modal 6:
                                          h3(tags$a("Select a method for counting",
                                          href = "#", onclick = "Shiny.setInputValue('showModal6', true);")),
                                          helpText("Click the title above for detailed instructions."),
                                          selectInput(inputId = "method", label = "", choices = c("Rsubread", "MAGeCK", "WEHI")),
                                          actionButton(inputId = "guidecounts", "Get the guide counts"),
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
                  # Modal 7
                  h3(tags$a(
                    "1. Create a data object",
                    href = "#", onclick = "Shiny.setInputValue('showModal7', true);"
                  )),
                  helpText("Click the title above for detailed instructions."),
                  actionButton("create_dgelist", "Create data object"),

                  # Modal 8
                  h3(tags$a(
                    "2. Check the quality of the experiment",
                    href = "#", onclick = "Shiny.setInputValue('showModal8', true);"
                  )),
                  helpText("Click the title above for detailed instructions."),
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
                  ),
        # Filtering Section
        # Modal 9:
        h3(tags$a("3. Filtering", href = "#", onclick = "Shiny.setInputValue('showModal9', true);" )),
        helpText("Click the title above for detailed instructions."),

        # Modal 10
        h4(tags$a("Select control groups",href = "#", onclick = "Shiny.setInputValue('showModal10', true);")),
        helpText("Click the title above for detailed instructions."),
        uiOutput("filter3_drop_down"),
        conditionalPanel(
          condition = "typeof input.filter3_what !== 'undefined' && input.filter3_what.length > 0",
          selectInput("filter_what", label = h4("Filter Type"),
                      choices = list("No Filter" = "none",
                                     "Filter Out all Zeros Counts" = "zero",
                                     "Strict filtering" = "edgeR",
                                     "Permissive filtering" = "edgeR2",
                                     "Cancer sensitive filtering" = "filter3")),
          checkboxInput("all_filters", label = "Show results for all filters", value = FALSE),
          checkboxInput("show_mds", label = "Show MDS plot", value = FALSE)
        ),

        # Normalisation Section
        # Modal 11
        h3(tags$a(
          "4. Normalisation",
          href = "#", onclick = "Shiny.setInputValue('showModal11', true);"
        )),
        helpText("Click the title above for detailed instructions."),
        selectInput("norm_what", label = h4("Normalisation Type"),
                    choices = list(
                                   "none",
                                   "TMM",
                                   "TMMwsp",
                                   "RLE",
                                   "upperquartile"
                                   )),
        uiOutput("norm_quant"),
        checkboxInput("all_norms", label = "Show results for all normalisation methods", value = FALSE),
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
        )#,
        #uiOutput("to_dim")
      )
    )
  ),
tabPanel("Analysis",
  sidebarLayout(
    sidebarPanel(
      # Select Software for Analyzing the Screen
      # Modal 12
      h3(tags$a(
          "1. Select the tool for analysing your screen",
          href = "#", onclick = "Shiny.setInputValue('showModal12', true);"
      )),
      helpText("Click the title above for detailed instructions."),
      selectInput(
                "software", "",
                choices = c("edgeR", "MAGeCK")
              ),
      conditionalPanel(
      condition = "input.software == 'MAGeCK'",  # Display when MAGeCK is selected
      # Empty content for MAGeCK
      ),

      conditionalPanel(
        condition = "input.software == 'edgeR'",
      # Check biological variation
      # Modal 13
      h3(tags$a(
      "2. View biological variation coefficient (BCV) plot",
      href = "#", onclick = "Shiny.setInputValue('showModal13', true);" )),
      helpText("Click the title above for detailed instructions."),
      checkboxInput("bcv", label = "Show BCV plot", value = FALSE),
      # Modal 14
      h3(tags$a(
          "3. Select the feature you want to analyse",
          href = "#", onclick = "Shiny.setInputValue('showModal14', true);"
      )),
      helpText("Click the title above for detailed instructions."),
      selectInput(
                "feature", "",
                choices = c("Guide level analysis", "Gene level analysis")
              ),
      conditionalPanel(
          condition = "input.feature == 'Guide level analysis'",
      # Fitting Model Section
      # Modal 15
      h3(tags$a(
      "4. Select a statistical model to fit",
      href = "#", onclick = "Shiny.setInputValue('showModal15', true);")),
      helpText("Click the title above for detailed instructions."),

      selectInput("model", "",
                  choices = list("Ebayes", "Generalised Linear Model", "Generalised Linear Model (Quasi Likelihood)")),

      # DE Genes Section
      # Modal 16
      h3(tags$a(
      "5. Differential expression analysis",
      href = "#", onclick = "Shiny.setInputValue('showModal16', true);")),
      helpText("Click the title above for detailed instructions."),

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
      ),

      conditionalPanel(
          condition = "input.feature == 'Gene level analysis'",
          # Gene Set Test Section
          # Modal 17
          h2(tags$a(
            "4. Select a threshold",
            href = "#", onclick = "Shiny.setInputValue('showModal17', true);")),
            helpText("Click the title above for detailed instructions."),
          numericInput("gene_threshold", label = h4("Threshold value"), value = 2, step = 1, min = 2),
          uiOutput("gene_set_contrast"),
          uiOutput('gene_set_down')
      )
      ),
    ),
    mainPanel(
      conditionalPanel(condition = "input.bcv == '1'",
                       h2("BCV plot"),
                       plotOutput("plot_bcv_disp"),
                       uiOutput("download_bcv_button")
      ),
      conditionalPanel(
          condition = "input.feature == 'Guide level analysis'",
      conditionalPanel(condition = "input.feature == 'Guide level analysis'",
                       h2("Guide level analysis results")
      ),

      conditionalPanel(condition = "input.model == 'Ebayes'",
                       h3("Ebayes results for differentially expressed guides"),
                       dataTableOutput("limma_table"),
                       uiOutput("download_limma_table_button")
      ),
      conditionalPanel(condition = "input.model == 'Generalised Linear Model'",
                       h3("GLM results for differentially expressed guides"),
                       dataTableOutput("edgeR_table_lrt"),
                       uiOutput("download_lrt_table_button")
      ),
      conditionalPanel(condition = "input.model == 'Generalised Linear Model (Quasi Likelihood)'",
                       h3("GLMQLH results for differentially expressed guides"),
                       dataTableOutput("edgeR_table_qlf"),
                       uiOutput("download_qlf_table_button")
      ),
      #uiOutput("to_de"),

      # DE Genes Outputs
      conditionalPanel(condition = "input.model == 'Ebayes' && input.DE_check == 'Compare DE genes in different contrasts'",
                       h3("Examine Differentially Expressed Genes"),
                       plotOutput("limma_ven"),
                       uiOutput("download_limma_ven_button"),
                       dataTableOutput("limma_com_gene"),
                       uiOutput("download_limma_com_button")
      ),
      conditionalPanel(condition = "input.model == 'Ebayes' && input.DE_check == 'Check DE genes in each contrast'",
                       fluidRow(plotOutput("limma_md")),
                       uiOutput("download_limma_md_button"),
                       #plotOutput("limma_heatmap", height = 700),
                       #uiOutput("download_limma_heatmap_button")
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
                       #plotOutput("edgeR_heatmap_lrt", height = 800),
                       #uiOutput("download_lrt_heatmap_button")
      ),
      conditionalPanel(condition = "input.model == 'Generalised Linear Model (Quasi Likelihood)' && input.DE_check == 'Check DE genes in each contrast'",
                       plotOutput("edgeR_md_qlf"),
                       uiOutput("download_qlf_md_button"),
                       #plotOutput("edgeR_heatmap_qlf", height = 800),
                       #uiOutput("download_qlf_heatmap_button")
      )
      ),
      #uiOutput("to_gene_set"),

      # Gene Set Test Outputs
      conditionalPanel(
        condition = "input.feature == 'Gene level analysis'",
      h2("Gene level analysis results"),
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
)
library(shiny)

# Define server logic
server <- function(input, output, session) {
  # Modals ####
  # Modal1: Enter the details of the experiment
observeEvent(input$showModal1, {
  showModal(modalDialog(
    title = "Enter the details of the experiment",
    "Please upload your guide library, sample information and count matrix or fastq files.",
    easyClose = TRUE,
    footer = modalButton("Close")
  ))
  runjs('Shiny.setInputValue("showModal1", null)')  # Reset the input value
})

# Modal2: Guide RNA Library Details
observeEvent(input$showModal2, {
  showModal(modalDialog(
    title = "Guide RNA Library Details",
    HTML("Please make sure that the file you're uploading follows the required format.
          The column names in your file should be:
          <ul>
            <li><strong>'SgRNA_ID'</strong> (the identifier for each guide)</li>
            <li><strong>'SgRNA_Sequence'</strong> (the sequence of the guide RNA)</li>
            <li><strong>'Gene_ID'</strong> (the identifier for the gene targeted by the guide)</li>
          </ul>"),
    easyClose = TRUE,
    footer = modalButton("Close")
  ))
  runjs('Shiny.setInputValue("showModal2", null)')  # Reset the input value
})

# Modal3: Sample Information Upload Details
observeEvent(input$showModal3, {
  showModal(modalDialog(
    title = "Sample Information Upload Details",
    HTML("Please make sure that the file you're uploading follows the required format.
          The column names in your file should be:
          <ul>
            <li><strong>'Fastqnames'</strong> (the name of the fastq file)</li>
            <li><strong>'Groups'</strong> (the experimental groups)</li>
            <li><strong>'Biorep'</strong> (biological replicate if applicable)</li>
            <li><strong>'Techrep'</strong> (technical replicate if applicable)</li>
          </ul>"),
    easyClose = TRUE,
    footer = modalButton("Close")
  ))
  runjs('Shiny.setInputValue("showModal3", null)')  # Reset the input value to allow multiple clicks
})

# Modal4: Count Matrix Upload Details
observeEvent(input$showModal4, {
  showModal(modalDialog(
    title = "Count Matrix Upload Details",
    HTML("Please make sure that the file you're uploading follows the required format.
          The column names in your file should be:
          <ul>
            <li><strong>'SgRNA_Sequence'</strong> (the sequence of the guide RNA)</li>
            <li><strong>'Gene_ID'</strong> (the gene ID)</li>
            <li><strong>Sample names</strong> (the expression values across the samples)</li>
          </ul>"),
    easyClose = TRUE,
    footer = modalButton("Close")
  ))
  runjs('Shiny.setInputValue("showModal4", null)')  # Reset the input value to allow multiple clicks
})

# Modal5: Fastq Files Upload Details
observeEvent(input$showModal5, {
  showModal(modalDialog(
    title = "Fastq Files Upload Details",
    HTML("Please make sure that the files you're uploading are in the correct format.
          Ensure that the fastq files are named properly and that they are paired if necessary."),
    easyClose = TRUE,
    footer = modalButton("Close")
  ))
  runjs('Shiny.setInputValue("showModal5", null)')  # Reset the input value to allow multiple clicks
})

# Modal6: Method Selection for Counting
observeEvent(input$showModal6, {
  showModal(modalDialog(
    title = "Method Selection for Counting",
    HTML("Here are the details about selecting a counting method:
          You can choose one of the following methods for counting the guides:
          <ul>
            <li><strong>Rsubread</strong>: Uses Rsubread for counting guides.</li>
            <li><strong>MAGeCK</strong>: Uses MAGeCK for counting guides.</li>
            <li><strong>WEHI</strong>: A custom guide counting method developed at WEHI.</li>
          </ul>
          After selecting a method, click the <strong>'Get the guide counts'</strong> button to proceed with the counting process."),
    easyClose = TRUE,
    footer = modalButton("Close")
  ))
  runjs('Shiny.setInputValue("showModal6", null)')  # Reset the input value to allow multiple clicks
})

observeEvent(input$showModal7, {
  showModal(modalDialog(
    title = "Create a Data Object Details",
    HTML("
      <p><strong>Here are the details about creating a data object:</strong></p>
      <p>We will create an edgeR <em>DGEList</em> object using the data uploaded in the <strong>Data Upload</strong> tab.
         The <em>DGEList</em> object contains essential information like:
      </p>
      <ul>
        <li><strong>Counts</strong>: The raw read counts for each guide or gene across the samples.</li>
        <li><strong>Samples</strong>: Metadata for each sample, such as conditions, experimental groups, etc.</li>
        <li><strong>Associated Metadata</strong>: Additional information such as experimental replicates or conditions.</li>
      </ul>
      <p>The <em>DGEList</em> object serves as the foundational object for downstream analyses in <strong>edgeR</strong>,
      including:
      <ul>
        <li><strong>Differential Expression Analysis</strong>: Identifying genes or guides that are differentially expressed across conditions.</li>
        <li><strong>Filtering Data</strong>: Removing lowly expressed guides or genes to improve analysis.</li>
        <li><strong>Normalization</strong>: Adjusting for library size or other factors to make the data comparable across samples.</li>
      </ul>
      <p>Once the <em>DGEList</em> object is created, you will be ready to proceed with further analyses, such as
      identifying significant genes or guides that are associated with your experimental conditions.</p>
    "),
    easyClose = TRUE,
    footer = modalButton("Close")
  ))
  runjs('Shiny.setInputValue("showModal7", null)')  # Reset the input value to allow multiple clicks
})

# Modal 8: Quality Check Details
observeEvent(input$showModal8, {
  showModal(modalDialog(
    title = "Quality Check Details",
    HTML("
      <p>Here are the steps for checking the quality of the experiment:</p>
      <ul>
        <li><strong>View guides distribution</strong>: This shows the distribution of the number of guides across the entire experiment. It's helpful for understanding how well the guides are represented across the samples.</li>
        <li><strong>View guide distribution per gene</strong>: You can view the distribution of guides for each gene in the dataset. This option is useful for identifying whether certain genes are overrepresented in your dataset, which could indicate potential biases.</li>
        <li><strong>View gene abundances across samples</strong>: This option shows the sum of counts for each gene across all samples. It allows you to identify the overall distribution of gene expression and check for any anomalies or inconsistencies across samples.</li>
      </ul>
      <p>Once you've selected a quality check method, proceed by viewing the corresponding visualizations to make adjustments as necessary.</p>
    "),
    easyClose = TRUE,
    footer = modalButton("Close")
  ))
  runjs('Shiny.setInputValue("showModal8", null)')  # Reset the input value
})

observeEvent(input$showModal9, {
  showModal(modalDialog(
    title = "Filtering Details",
    HTML("
      <p><strong>Filtering</strong> helps to refine the dataset by removing unwanted or irrelevant data points. This process improves the quality of the analysis by reducing noise and focusing on the most relevant data.</p>
      <p>By filtering out lowly expressed guides or genes and other irrelevant data, we ensure that the subsequent analysis will focus on the data that best represents the biological process of interest.</p>
    "),
    easyClose = TRUE,
    footer = modalButton("Close")
  ))

  runjs('Shiny.setInputValue("showModal9", null)')  # Reset the input value to allow multiple clicks
})

# Modal 10:
observeEvent(input$showModal10, {
  showModal(modalDialog(
    title = "Select Control Groups and Then Type of Filtering",
    HTML("
      <p><strong>Here are the details about selecting control groups and type of filtering:</strong></p>
      <p>First, you will select the control groups. Then type of filtering from the options below. Options may take up to 1 minute to load, so please be patient. Don't leave the page while the options are being processed.</p>
      <ul>
        <li><strong>No Filter:</strong> This option shows the raw count figures without any filtering applied. It's useful for seeing the data as is before any adjustments.</li>
        <li><strong>Filter Out all Zeros Counts:</strong> Filters out the guides with zero counts across all samples. This is useful to remove guides that aren't being expressed in any of the samples, reducing noise in your analysis.</li>
        <li><strong>Strict Filtering:</strong> Uses edgeR's <em>FilterByExp</em> function to filter out lowly expressed guides. <em>FilterByExp</em> applies a statistical method to remove guides with low counts that are unlikely to show meaningful results in differential expression analysis.</li>
        <li><strong>Permissive Filtering:</strong> A less strict filtering method compared to the strict approach, which allows more guides to remain in the dataset. This is useful when you want to retain a larger set of guides for further analysis, even if they have lower expression.</li>
        <li><strong>Cancer Sensitive Filtering:</strong> Uses a new filtering approach developed for cancer-specific data with highly variable genes. This method leverages the count distribution in the control groups to identify and retain the most relevant guides for cancer research, particularly in datasets with high variability.</li>
      </ul>
      <p>Once you've selected a filtering method, you can proceed to view the results and make adjustments if necessary. You can also choose to display an MDS plot for better visualization of the results.</p>
    "),
    easyClose = TRUE,
    footer = modalButton("Close")
  ))
  runjs('Shiny.setInputValue("showModal10", null)')  # Reset the input value to allow multiple clicks
})

# Modal 11: Normalisation
observeEvent(input$showModal11, {
  showModal(modalDialog(
    title = "Normalisation Details",
    HTML("
      <p><strong>Normalisation</strong> is the process of adjusting the data to account for technical differences and biases, ensuring that the results reflect true biological variations. In this step, you can apply one of the following normalisation methods to your filtered data:</p>
      <ul>
        <li><strong>TMM (Trimmed Mean of M-values)</strong>: A robust method for normalizing RNA-seq data, which minimizes the effect of outliers and performs well for datasets with varying library sizes.</li>
        <li><strong>TMMwsp (Weighted Sum of Pairs)</strong>: A variation of TMM that improves on the standard TMM method by better handling extreme outliers.</li>
        <li><strong>RLE (Relative Log Expression)</strong>: Normalizes data based on the relative expression levels of genes across samples. This method is particularly useful for datasets with large differences in library size.</li>
        <li><strong>Upper Quartile</strong>: Normalizes the data by scaling the counts to the upper quartile of the distribution. This method is robust to large differences in expression levels and can be useful for datasets with highly variable expression.</li>
      </ul>
      <p>Once the normalisation method is chosen, proceed to <strong>Analysis</strong> for differential expression analysis.</p>
    "),
    easyClose = TRUE,
    footer = modalButton("Close")
  ))

  runjs('Shiny.setInputValue("showModal11", null)')  # Reset the input value to allow multiple clicks
})

# Modal 12
observeEvent(input$showModal12, {
  showModal(modalDialog(
    title = "Select Tool for Analysis",
    HTML("
      <p><strong>Here are the tools available for analyzing your CRISPR screen:</strong></p>
      <ul>
        <li><strong>MAGeCK</strong>: A widely used tool for analyzing CRISPR screen data. MAGeCK uses a model-based analysis approach to identify enriched or depleted guides, helping you detect genes that are important for your experiment. It's especially useful for identifying genetic interactions and essential genes in functional genomics studies.</li>
        <li><strong>edgeR</strong>: A powerful tool for differential expression analysis, primarily used for RNA-seq data. In the context of CRISPR screens, edgeR can be used to identify differentially enriched or depleted guides across conditions, helping you pinpoint genes involved in your biological process. It works by modeling count data and using statistical methods to detect differences in expression between samples.</li>
      </ul>
      <p>Once you've selected a tool, you can proceed with the analysis using that method to analyze the CRISPR screen data.</p>
    "),
    easyClose = TRUE,
    footer = modalButton("Close")
  ))

  runjs('Shiny.setInputValue("showModal12", null)')  # Reset the input value to allow multiple clicks
})

# Modal 13
observeEvent(input$showModal13, {
  showModal(modalDialog(
    title = "View Biological Variation Coefficient (BCV) Plot",
    HTML("
      <p><strong>Biological Variation Coefficient (BCV) Plot:</strong></p>
      <p>The BCV plot is a diagnostic plot used to visualize the variability of gene expression data across different conditions in RNA-seq experiments. It is particularly useful in the context of differential expression analysis in **CRISPR screens** and **RNA-seq data**. The BCV plot helps assess the biological variation between different groups, providing insight into how well the data fits the underlying statistical model.</p>
      <p><strong>What BCV plots show:</strong></p>
      <ul>
        <li><strong>X-axis</strong>: The number of genes (or guides in CRISPR screens) ordered by expression level.</li>
        <li><strong>Y-axis</strong>: The biological coefficient of variation (BCV), which is a measure of relative variability across the samples.</li>
      </ul>
      <p>The BCV plot helps determine whether there is large variability among genes across your conditions, which can affect the quality of your differential expression analysis.</p>
      <p>Once you check the box to display the BCV plot, it may take some time for the plot to be generated depending on the data size. Please don't leave this page until the plot is shown.</p>
    "),
    easyClose = TRUE,
    footer = modalButton("Close")
  ))

  runjs('Shiny.setInputValue("showModal13", null)')  # Reset the input value to allow multiple clicks
})

# Modal 14
observeEvent(input$showModal14, {
  showModal(modalDialog(
    title = "Select the Feature You Want to Analyze",
    HTML("
      <p><strong>Select Feature for Analysis:</strong></p>
      <p>In this step, you need to choose which level of analysis you want to perform for your data. You can either focus on:</p>
      <ul>
        <li><strong>Gene Level Analysis</strong>: This approach analyzes the gene expression or activity across all the guides targeting each gene. It focuses on the **overall gene behavior** across the entire dataset. This is useful if you want to identify the biological significance of specific genes and how they are regulated under different experimental conditions.</li>
        <li><strong>Guide Level Analysis</strong>: This method looks at the **individual guides targeting each gene**, allowing you to focus on the performance of the specific CRISPR guides. It helps assess which individual guides have a strong effect on gene activity and which guides may be underperforming or ineffective.</li>
      </ul>
      <p><strong>How to Choose:</strong></p>
      <ul>
        <li>If you are interested in **broad gene expression analysis** across the experiment, select <strong>Gene Level Analysis</strong>.</li>
        <li>If you want to focus on **individual guide performance**, especially in CRISPR screens, select <strong>Guide Level Analysis</strong>.</li>
      </ul>
      <p>Once you've made your selection, you can proceed with the appropriate analysis method.</p>
    "),
    easyClose = TRUE,
    footer = modalButton("Close")
  ))

  runjs('Shiny.setInputValue("showModal14", null)')  # Reset the input value to allow multiple clicks
})

# Modal 15
observeEvent(input$showModal15, {
  showModal(modalDialog(
    title = "Select a Statistical Model to Fit",
    HTML("
      <p><strong>Choosing the Right Statistical Model for Your Data:</strong></p>
      <p>In the context of CRISPR screens and RNA-seq data, the statistical model you choose plays a crucial role in analyzing the data accurately. Below are the three available models for fitting the data:</p>
      <ul>
        <li><strong>Ebayes</strong>: This model is used in the <strong>limma</strong> package, and it applies empirical Bayes moderation to estimate variance. It is particularly useful when you have multiple groups or conditions and need to control for the large variation in gene expression across those conditions. It is generally suitable for smaller datasets where normality assumptions hold.</li>
        <li><strong>Generalised Linear Model (GLM)</strong>: This is a flexible and robust statistical model that generalizes linear regression to account for non-normal data distributions. GLMs are especially useful when the data doesn’t follow a normal distribution, like in the case of count data (e.g., CRISPR screens, RNA-seq data). It can be used for various distributions such as Poisson or negative binomial.</li>
        <li><strong>Generalised Linear Model (Quasi Likelihood)</strong>: This variant of GLM is used when the assumption of constant variance does not hold, such as in RNA-seq data where the variance tends to increase with the mean. This model is useful when dealing with overdispersed data where the variance exceeds the mean.</li>
      </ul>
      <p><strong>How to Choose a Model:</strong></p>
      <ul>
        <li>If you're working with RNA-seq data or CRISPR screens with count data and wish to use a robust method for differential expression, you may prefer the <strong>GLM</strong> or <strong>Quasi Likelihood</strong> models.</li>
        <li>If you have smaller datasets or your data fits normal assumptions, you can use the <strong>Ebayes</strong> method.</li>
      </ul>
      <p>Once you've selected a model, you can proceed with fitting the model and analyzing the results.</p>
    "),
    easyClose = TRUE,
    footer = modalButton("Close")
  ))

  runjs('Shiny.setInputValue("showModal15", null)')  # Reset the input value to allow multiple clicks
})

# Modal 16
observeEvent(input$showModal15, {
  showModal(modalDialog(
    title = "Differential Expression Genes",
    HTML("
      <p><strong>Overview of Differential Expression Genes (DEGs):</strong></p>
      <p>In CRISPR screens and RNA-seq analysis, the identification of differentially expressed genes (DEGs) is a key step in understanding which genes are associated with the experimental condition.</p>
      <p>In this step, you can choose one of the following options for examining DEGs:</p>
      <ul>
        <li><strong>Check DE genes in each contrast</strong>: This option allows you to examine the DEGs for each individual contrast. You'll be able to view genes that show differential expression for specific comparisons.</li>
        <li><strong>Compare DE genes in different contrasts</strong>: This option compares DEGs between two or more contrasts to identify common or unique genes across multiple conditions.</li>
      </ul>
      <p><strong>How to Use:</strong></p>
      <ul>
        <li>Select the option you prefer from the dropdown menu for differential expression analysis. Once you choose, additional input fields will appear to specify the contrasts and conditions you wish to analyze.</li>
        <li>If you choose Check DE genes in each contrast, you'll be prompted to select the specific contrasts to assess individual DEGs.</li>
        <li>If you choose Compare DE genes in different contrasts, you'll be asked to specify which two contrasts you want to compare in order to find genes that are differentially expressed between them.</li>
      </ul>
      <p>Once you've selected your contrast(s), proceed to examine the DEGs and explore results like volcano plots, heatmaps, and more.</p>
    "),
    easyClose = TRUE,
    footer = modalButton("Close")
  ))

  runjs('Shiny.setInputValue("showModal16", null)')  # Reset the input value to allow multiple clicks
})

# Modal 17
observeEvent(input$showModal17, {
  showModal(modalDialog(
    title = "Gene Set Test",
    HTML("
      <p><strong>Overview of Select a threshold:</strong></p>
      <p>Selecting a threshold allows you to focus on specific genes that have a number of guides greater than or equal to a user-defined threshold. This is particularly useful in CRISPR screens, where certain genes may have multiple guides targeting them, and you want to assess the effects of those genes with more than one guide.</p>

      <p><strong>How to Use:</strong></p>
      <ul>
        <li><strong>Enter the threshold value:</strong> The threshold defines the minimum number of guides that a gene must have in order to be included in the analysis. Genes with fewer than this number of guides will not be considered in the test.</li>
        <li><strong>Gene Set Contrast:</strong> After setting the threshold, you can select the contrast you want to compare for the gene set test. This helps in identifying which genes are significantly affected under specific experimental conditions.</li>
        <li><strong>Download Results:</strong> After the analysis, you can download the results for further investigation and reporting.</li>
      </ul>

      <p><strong>Why Set a Threshold?</strong></p>
      <ul>
        <li>Setting a threshold helps ensure that genes with only a few guides (and potentially unreliable results) are excluded from the analysis.</li>
        <li>Genes with multiple guides provide a stronger and more reliable indication of gene activity and effects.</li>
      </ul>

      <p>Once you've set the threshold and selected the contrast, proceed to view the results for the gene set analysis.</p>
    "),
    easyClose = TRUE,
    footer = modalButton("Close")
  ))

  runjs('Shiny.setInputValue("showModal17", null)')  # Reset the input value to allow multiple clicks
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
    runjs('window.scrollTo(0, 0);')
  })
  observeEvent(input$gotopreprocessing, {
    updateTabsetPanel(session, "main", "Preprocessing")
    runjs('window.scrollTo(0, 0);')
  })
  #observeEvent(input$deDone, {
   # updateTabsetPanel(session,  "main", "Gene set test")
  #})
  #observeEvent(input$bcvDone, {
  #  updateTabsetPanel(session,  "main", "Fitting model")
  #})
  #observeEvent(input$modDone, {
  #  updateTabsetPanel(session,  "main", "DE genes")
  #})
  #observeEvent(input$filterDone, {
  #  updateTabsetPanel(session,  "main", "Normalisation")
  #})
  #observeEvent(input$normDone, {
  #  updateTabsetPanel(session,  "main", "Fitting model")
  #})
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
    #actionButton(inputId = "filterDone", label = paste("Proceed to Normalisation with ", filter, sep = ""))
  })
  #output$to_dim <- renderUI({
  #  actionButton(inputId = "normDone", label = paste("Proceed to Analysis with ", input$norm_what, sep = ""))
  #})
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
    dge$samples$biorep <- dge$samples$Biorep
    dge$samples$Biorep <- NULL
    dge$samples$techrep <- dge$samples$Techrep
    dge$samples$Techrep <- NULL

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
               label = "Select all controlled group for filtering purpose",
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


      plot_density(edgeR_object(), paste("No Filter (",dim," guides present)", sep = ""))
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
        return(calcNormFactors(filtered_dge(), method = "TMM"))
  })

  norm_dge_tmmwsp <- reactive({
        return(calcNormFactors(filtered_dge(), method = "TMMwsp"))
  })

  norm_dge_RLE <- reactive({
        return(calcNormFactors(filtered_dge(), method = "RLE"))
  })

  norm_dge_quant <- reactive({
        return(calcNormFactors(filtered_dge(), method = "upperquartile", p=input$quant_num))
  })

  norm_dge_none <- reactive({
    if( ! is.null(filtered_dge()) &&  !is.null(input$norm_what))
        return(calcNormFactors(filtered_dge(), method = "none"))
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

observeEvent(design(), {
  # Print the design matrix to the console
  print(design())
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

  observeEvent(contrast(), {
  # Print the design matrix to the console
  print(contrast())
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
  observeEvent(efit(), {
    print(efit())  # Print the efit to the console
  })

  limma_df <- function(){
    if(!is.null(efit())){
      return(limma_table(efit()))
    }
  }

  limma_tab <- function(){
    if(!is.null(limma_df())){
      limma_df()
    }
  }



  output$limma_table <-  DT::renderDataTable({
    if(!is.null(limma_tab())) datatable(limma_tab(),rownames = TRUE)
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



  #output$to_de <- renderUI({
  #  actionButton(inputId = "modDone", label = paste("Proceed to examine DE genes with ", input$model, sep = ""))
  #})

  #output$to_gene_set <- renderUI({
  #  actionButton(inputId = "deDone", label = paste("Proceed to gene set test ", sep = ""))
  #})

  #observeEvent(input$deDone, {
  #  updateTabsetPanel(session,  "main", "Gene set test")
  #})


  #observeEvent(input$bcvDone, {
  #  updateTabsetPanel(session,  "main", "Fitting model")
  #})

  #observeEvent(input$modDone, {
  #  updateTabsetPanel(session,  "main", "DE genes")
  #})


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

observeEvent(genesymbollist(), {
  print(genesymbollist())  # Print the genesymbollist to the console
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
