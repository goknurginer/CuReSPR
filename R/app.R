library(shiny)
library(shinyWidgets)
library(shinythemes)
library(DT)
# source("global.R")
library(refund.shiny)
options(shiny.maxRequestSize=100*1024^2)
# Define UI ----
ui <- navbarPage("CuReSPR", id = "main",
                 theme = shinytheme("cerulean"),
#----------------------- ui Data Upload ------------------------------------
                 tabPanel("Data Upload",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Set group numbers"),
                              numericInput("num",
                                           label = "",
                                           value = 0,
                                           min = 0),
                              conditionalPanel(
                                condition = "input.num > '0'",
                                hr(),
                                h4("Assign group names to each group number"),
                                uiOutput("textInputs"),
                                checkboxInput("paired", label = "Select if the fastq files are paired-end"),
                                actionButton("fastqupload", "Upload fastq files"),
                              ),
                              conditionalPanel(
                                condition = "input.fastqupload",
                                fileInput("upload",
                                          label = "",
                                          accept = c('.fastq','fastq.gz'),
                                          multiple = TRUE) # This option does not work on older browsers, including Internet Explorer 9 and earlier.
                              ),
                            ),
                            mainPanel(
                              #DT::DTOutput("files"),
                              tableOutput("selected_file_table"),
                              textOutput("groups")
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
  #----- numericInput -----
  n <- reactive({input$num})
  output$textInputs <- renderUI({
    textInputs <- lapply(1:n(), function(i) {
      textInput(paste0("group", i),
                label = paste0("Group ", i))
    })
    do.call(tagList, textInputs)
  })

  values <- reactive({unlist(lapply(1:n(), function(i) {
    input[[paste0("group", i)]]}))})
  output$groups <- renderText({
    req(values())
  })
  # groups <- as.character(unlist(values))
  #output$groups <- renderText(paste0("Groups are: ", values))


#----- fileInput fastq files -----
  # output$files <- DT::renderDT({
  #   DT::datatable(input$upload, selection = c("single"))
  # })
  output$selected_file_table <- renderTable({
    req(input$upload[,1:2])
  })
}
# Run the app ----
shinyApp(ui = ui, server = server)


