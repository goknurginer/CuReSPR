library(shiny)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(refund.shiny)
library(tidyverse)

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
                                uiOutput("groupInputs"),
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
                                condition = "input.upload",
                                p("Groups are ", textOutput("groups", inline = TRUE))),
                              DTOutput("my_datatable")
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

  #----- Groups Input -----
  n <- reactive({input$num})

  output$groupInputs <- renderUI({
    groupInputs <- lapply(1:n(), function(i) {
      textInput(paste0("group", i),
                label = paste0("Group ", i))
    })
    do.call(tagList, groupInputs)
  })

  values <- reactive({unlist(lapply(1:n(), function(i) {
    input[[paste0("group", i)]]}))})
  output$groups <- renderText({
    req(values())
  })

  # #----- assign group names to fastqs -----
  output$my_datatable <- renderDT({
    data <- data.frame(Fastq = input$upload[,1],
               Size = input$upload[,2],
               Groups = seq(req(values())))
    if(!as.logical(input[['tech']]) & !as.logical(input[['bio']]))
      v <- reactive({
        data
      })

    else if (as.logical(input[['tech']]) & !as.logical(input[['bio']]))
    v <- reactive({
      data %>%
        add_column(TechRep = rep(0,nrow(input$upload)))
    })

    else if(!as.logical(input[['tech']]) & as.logical(input[['bio']]))
    v <- reactive({
      data %>%
        add_column(BioRep = rep(0,nrow(input$upload)))
    })

    else
    v <- reactive({
      data %>%
        add_column(TechRep = rep(0,nrow(input$upload))) %>%
        add_column(BioRep = rep(0,nrow(input$upload)))
    })
    DT::datatable(v(), editable = TRUE)
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)


