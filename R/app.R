library(shiny)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(refund.shiny)
library(tidyverse)
options(shiny.reactlog=TRUE)

# source("global.R")
options(shiny.maxRequestSize=100*1024^2)
# Define UI ----
ui <- navbarPage("CuReSPR", id = "main",
                 theme = shinytheme("cerulean"),
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
                                checkboxInput("bio", "There are biological replicates"),
                                actionButton("nextuploadguides", "Next")
                              ),
                              conditionalPanel(
                                condition = "input.nextuploadguides",
                                hr(),
                                h4("Upload guide RNA library"),
                                fileInput("uploadguides",
                                          label = "",
                                          accept = c('.tsv','.csv','.txt'),
                                          multiple = TRUE), # This option does not work on older browsers, including Internet Explorer 9 and earlier.
                                actionButton("nextuploadguides", "Next")
                              ),
                            ),
                            mainPanel(
                              conditionalPanel(
                                condition = "input.nextupload",
                                p("Groups are ", textOutput("groups", inline = TRUE))),
                              DT::dataTableOutput("my_datatable"),
                              # actionButton("nextgroups", "Next"),
                              # conditionalPanel(
                              #   condition = "input.nextgroups",
                              #   verbatimTextOutput('sel'),
                              #   textOutput("result")),
                            )
                          )
                 ),
#----------------------- ui Creating Count Matrix ---------------------------------------
tabPanel("Counting",
         sidebarLayout(
           sidebarPanel(
             h4("Select your method of counting"),
             selectInput("counting",
               "",
               choices = c("Rsubread","MAGeCK","WEHI"),
             ),
           ),
           mainPanel(
             tableOutput("count_table"),
             downloadButton("download", "Download"),
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

  modified_data <- reactive({
    req(uploaded_data())
    data <- uploaded_data()
    for (i in 1:nrow(data)) {
      data$group[i] <- as.character(selectInput(paste0("sel", i),
                                                "",
                                                choices = unique(req(values())),
                                                width = "100px"))
    }
    return(data)
  })

  #----- Groups Names -----
  n <- reactive({
    input$num
    })

  m <- reactive({
    nrow(req(modified_data()))
  })

  output$groupnames <- renderUI({
    groupnames <- lapply(1:n(),
                         function(i) {
                           textInput(paste0("group", i),
                                     label = paste0("Group ", i))})
    do.call(tagList, groupnames)
  })

  values <- reactive({
    unlist(lapply(1:n(),
                  function(i) {
                    input[[paste0("group", i)]]}))
    })

  output$groups <- renderText({
    req(values())
  })

  # #----- assign group names to fastqs -----
  output$my_datatable <- renderDT({
    data <- data.frame(Fastq = modified_data()[,1],
                       Size = modified_data()[,2],
                       Group = modified_data()[["group"]])
    if(as.logical(input[['paired']])){
      data <- data %>% add_column(FastqPair = rep(1,nrow(modified_data())))
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
        v <- reactive({data %>% add_column(TechRep = rep(1,nrow(modified_data())))})
      }

      else if(
        !as.logical(input[['tech']])
        & as.logical(input[['bio']])
      ){
        v <- reactive({data %>% add_column(BioRep = rep(1, nrow(modified_data())))})
      }

      else{
        v <- reactive({data %>%
            add_column(TechRep = rep(1, nrow(modified_data()))) %>%
            add_column(BioRep = rep(1, nrow(modified_data())))
        })
      }
    }

    else {
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
        v <- reactive({data %>% add_column(TechRep = rep(1,nrow(modified_data())))})
      }

      else if(
        !as.logical(input[['tech']])
        & as.logical(input[['bio']])
      ){
        v <- reactive({data %>% add_column(BioRep = rep(1, nrow(modified_data())))})
      }

      else{
        v <- reactive({data %>%
            add_column(TechRep = rep(1, nrow(modified_data()))) %>%
            add_column(BioRep = rep(1, nrow(modified_data())))
        })
      }
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

  count_data <- reactive({
    if (input$counting=="Rsubread"){
      data <- read_tsv("/Users/giner.g/Documents/Github/CuReSPR/counts/Base1.tsv")
      data
    }
    else if (input$counting=="MAGeCK"){
      data <- read_tsv("/Users/giner.g/Documents/Github/CuReSPR/counts/High1.tsv")
      data
    }
    else {
      data <- read.csv("/Users/giner.g/Documents/Github/CuReSPR/counts/count_matrix_WEHI.csv")
      data
    }
  })

  output$count_table <- renderTable({
    count_data()
  })

  output$download <- downloadHandler(
    filename = function() {
      paste0(input$counting, ".csv")
    },
    content = function(file) {
      vroom::vroom_write(count_data(), file)
    }
  )

#   output$sel = renderPrint({
#     str(sapply(1:m(), function(i) input[[paste0("sel", i)]]))
#   })
# #
#   observe({
#     print(m())
#   })
#
#     output$result <- renderText({
#       paste("You chose", req(input$sel1))
#     })
}

# Run the app ----
shinyApp(ui = ui, server = server)


