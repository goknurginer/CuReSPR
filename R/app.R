library(shiny)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(refund.shiny)
library(tidyverse)
rHandsontableOutput("mytable")
renderRHandsontable({})
library(rhandsontable)

up = NULL
# source("global.R")
options(shiny.maxRequestSize=100*1024^2)
# Define UI ----
ui <- navbarPage("CuReSPR", id = "main",
                 theme = shinytheme("cerulean"),
#----------------------- ui Data Upload ------------------------------------
                 tabPanel("Data Upload",
                          sidebarLayout(
                            sidebarPanel(
                              h3("Experiment Details Input"),
                              helpText("First, define the number of comparison groups. Then, upload your fastq files and assign each to its group. Specify any biological or technical replicates, if they exist, and indicate whether the files are paired-end or single-end. Review and submit them for the counting step."),
                              hr(),
                              h4("Set group numbers"),
                              helpText("Enter number of groups in your experiment."),
                              numericInput("num",
                                           label = "",
                                           value = 0,
                                           min = 0),
                              actionButton("nextnum", "Next"),
                              conditionalPanel(
                                condition = "input.nextnum",
                                hr(),
                                h4("Assign group names"),
                                helpText("Enter names for each group."),
                                uiOutput("groupnames"),
                                actionButton("nextupload", "Next")
                              ),
                              conditionalPanel(
                                condition = "input.nextupload",
                                hr(),
                                h4("Upload fastq files"),
                                fileInput("upload",
                                          label = "",
                                          accept = c('.fastq','fastq.gz'),
                                          multiple = TRUE), # This option does not work on older browsers, including Internet Explorer 9 and earlier.
                                checkboxInput("paired", label = "Fastq files are paired-end"),
                                checkboxInput("tech", "There are technical replicates"),
                                checkboxInput("bio", "There are biological replicates")
                              ),
                            ),
                            mainPanel(
                              conditionalPanel(
                                condition = "input.nextupload",
                              p("Groups are ", textOutput("groups", inline = TRUE))),
                              DT::dataTableOutput("my_datatable")
                            )
                          )
                 ),
#----------------------- ui Creating Count Matrix ---------------------------------------
                 tabPanel("Count the sgRNAs",
                          h4("testing counting"),
                          fluidPage(
                            sidebarPanel(
                            )
                          )
                 ),
#----------------------- ui Pre-proccessing ---------------------------------------
                 tabPanel("Preproccessing",
                          h4("testing counting"),
                          fluidPage(
                            sidebarPanel(
                            )
                          )
                 ),
#----------------------- ui Differential Analysis ---------------------------------------
                 tabPanel("Differential Analysis",
                          h4("testing counting"),
                          fluidPage(
                            sidebarPanel(
                            )
                          )
                 ),
#----------------------- ui Pathway Analysis ---------------------------------------
                 tabPanel("Pathway Analysis",
                          h4("testing counting"),
                          fluidPage(
                            sidebarPanel(
                            )
                          )
                 ),
)

# Define server logic ----
server <- function(input, output) {

  uploaded_data <- reactive({
    return(input$upload)
  })
  modifiedData <- reactive({
    req(uploaded_data())
    data <- uploaded_data()
    data$test <- req(values())
    return(data)
  })

  #----- Groups Names -----
  n <- reactive({input$num})

  output$groupnames <- renderUI({
    groupnames <- lapply(1:n(), function(i) {
      textInput(paste0("group", i),
                label = paste0("Group ", i))
    })
    do.call(tagList, groupnames)
  })

  values <- reactive({unlist(lapply(1:n(), function(i) {
    input[[paste0("group", i)]]}))})
  output$groups <- renderText({
    req(values())
  })

  # #----- assign group names to fastqs -----
  output$my_datatable <- renderDataTable({



    data <- data.frame(Fastq = modifiedData()[,1],Size = modifiedData()[,2],Test = modifiedData()[["test"]])

    if(
      !as.logical(input[['tech']])
      & !as.logical(input[['bio']])
    ){
      v <- reactive({data})
    }

    else if (
      as.logical(input[['tech']])
      & !as.logical(input[['bio']])
    ){
      v <- reactive({data %>% add_column(TechRep = rep(0,nrow(modifiedData())))})
    }

    else if(
      !as.logical(input[['tech']])
      & as.logical(input[['bio']])
    ){
      v <- reactive({data %>% add_column(BioRep = rep(0,nrow(modifiedData())))})
    }

    else{
      v <- reactive({data %>%
          add_column(TechRep = rep(0,nrow(modifiedData()))) %>%
          add_column(BioRep = rep(0,nrow(modifiedData())))
      })
    }

    for (i in 1:nrow(v())) {
      data$species_selector[i] <- as.character(selectInput(paste0("sel", i), "", choices = unique(iris$Species), width = "100px"))
    }

    DT::datatable(
      v(),
      editable = TRUE,
      escape = FALSE,
      selection = 'none',
      options = list(dom = 't', paging = FALSE, ordering = FALSE),
      callback = JS("table.rows().every(function(i, tab, row) {
        var $this = $(this.node());
        $this.attr('id', this.data()[0]);
        $this.addClass('shiny-input-container');
      });
      Shiny.unbindAll(table.table().node());
      Shiny.bindAll(table.table().node());"))
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)


