library(shiny)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(refund.shiny)
library(tidyverse)

options(shiny.reactlog = TRUE)
options(shiny.maxRequestSize = 100 * 1024^2)

# Define UI ----
ui <- navbarPage("CuReSPR", id = "main", theme = shinytheme("cerulean"),
                 tabsetPanel(id = "inTabset",
                             #----------------------- ui Data Upload ------------------------------------
                             tabPanel("Data Upload",
                                      sidebarLayout(
                                        sidebarPanel(
                                          h3("Details of the experiment"),
                                          helpText("First, define the number of comparison groups.
                   Then, upload your fastq files and assign each fastq file to its group.
                   Specify any biological or technical replicates, if they exist, and indicate whether the files are paired-end or single-end.
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
                                                           actionButton("nextupload", "Next")
                                          ),
                                          conditionalPanel(condition = "input.nextupload > 0",
                                                           p("Groups are ", textOutput("groups", inline = TRUE))
                                          )
                                        ),
                                        mainPanel(
                                          conditionalPanel(condition = "input.nextupload > 0",
                                                           h4("Upload guide RNA library"),
                                                           fileInput("uploadguides", label = "", accept = c('.tsv', '.csv', '.txt'), multiple = TRUE),
                                                           hr(),
                                                           h4("Upload fastq files"),
                                                           fileInput("upload", label = "", accept = c('.fastq', 'fastq.gz'), multiple = TRUE),
                                                           checkboxInput("paired", label = "Fastq files are paired-end"),
                                                           checkboxInput("tech", "There are technical replicates"),
                                                           checkboxInput("bio", "There are biological replicates"),
                                                           actionButton("nextdatatable", "Next")
                                          ),
                                          conditionalPanel(condition = "input.nextdatatable > 0",
                                                           hr(),
                                                           h4("Enter sample details"),
                                                           helpText("Please enter the details about the samples in the following table."),
                                                           actionButton("gotocounting", "Go to counting"),
                                                           DT::dataTableOutput("dataTable"),
                                                           verbatimTextOutput('sel')
                                          )
                                        )
                                      )
                             ),
                             #----------------------- ui Creating Count Matrix ---------------------------------------
                             tabPanel("Counting",
                                      sidebarLayout(
                                        sidebarPanel(
                                          h4("Select your method of counting"),
                                          selectInput("counting", "", choices = c("Rsubread", "MAGeCK", "WEHI")),
                                          actionButton("guidecounts", "Get the guide counts"),
                                        ),
                                        mainPanel(
                                          conditionalPanel(condition = "input.guidecounts > 0",
                                                           uiOutput("dynamic_ui")  # Placeholder for dynamic UI components
                                          )
                                        )
                                      )
                             ),
                             #----------------------- ui Pre-proccessing ---------------------------------------
                             tabPanel("Preproccessing",
                                      h4("Testing counting"),
                                      fluidPage(sidebarPanel())
                             ),
                             #----------------------- ui Differential Analysis ---------------------------------------
                             tabPanel("Differential Analysis",
                                      h4("Testing counting"),
                                      fluidPage(sidebarPanel())
                             ),
                             #----------------------- ui Pathway Analysis ---------------------------------------
                             tabPanel("Pathway Analysis",
                                      h4("Testing counting"),
                                      fluidPage(sidebarPanel())
                             )
                 )
)

# Define server logic ----
server <- function(input, output, session) {
  # Reactive value to store the state of button click
  button_clicked <- reactiveVal(FALSE)

  # Observe counting dropdown change and reset button click state
  observeEvent(input$counting, {
    button_clicked(FALSE)
  })

  # Observe guidecounts button click and set reactive value to TRUE
  observeEvent(input$guidecounts, {
    button_clicked(TRUE)
  })

  observeEvent(input$gotocounting, {
    updateTabsetPanel(session, "inTabset", selected = "Counting")
  })

  # Render the dynamic UI based on button click state
  output$dynamic_ui <- renderUI({
    if (button_clicked()) {
      tagList(
        DT::dataTableOutput("count_table"),
        downloadButton("download", "Download")
      )
    }
  })

  # Reactive value to store the dataframe
  myData <- reactiveVal(data.frame(Fastq = character(), Size = numeric(), Group = character()))

  # Observe the got to datatable button
  observeEvent(input$nextdatatable, {
    test <- sapply(1:nrow(input$upload), function(i) {
      as.character(selectInput(paste0("sel", i), "", choices = unique(req(values())), width = "100px"))
    })
    newEntry <- data.frame(Fastq = input$upload[, 1],
                           Size = input$upload[, 2],
                           Group = test,
                           stringsAsFactors = FALSE)
    myData(rbind(myData(), newEntry))
  })

  # Determine the base data, including FastqPair if paired input is TRUE
  base_data <- reactive({
    if (as.logical(input[['paired']])) {
      myData() %>% add_column(FastqPair = rep(1, nrow(myData())))
    } else {
      myData()
    }
  })

  # Add columns based on tech and bio inputs
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

  # Render the dataframe as a table
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

  count_data <- reactive({
    file_path <- switch(input$counting,
                        "Rsubread" = "/Users/giner.g/Documents/Github/CuReSPR/datasets/T8/rsubread/counts_rsubread.csv",
                        "MAGeCK" = "/Users/giner.g/Documents/Github/CuReSPR/datasets/T8/mageck/counts_mageck.csv",
                        "/Users/giner.g/Documents/Github/CuReSPR/datasets/T8/wehi/counts_wehi.csv")
    data <- read.table(file_path, sep = "\t", header = TRUE)
    datatable(data)
  })

  # Define output variables
  output$groupnames <- renderUI({
    groupnames <- lapply(1:n(), function(i) {
      textInput(paste0("group", i), label = paste0("Group ", i))
    })
    do.call(tagList, groupnames)
  })

  output$groups <- renderText({
    req(values())
  })

  output$count_table <- renderDT({
    req(button_clicked())  # Ensure this runs only after button is clicked
    count_data()
  })

  output$download <- downloadHandler(
    filename = function() {
      paste0(input$counting, "-guide-counts-", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(button_clicked())  # Ensure this runs only after button is clicked
      vroom::vroom_write(count_data(), file)
    }
  )
}

# Run the app ----
shinyApp(ui = ui, server = server)
