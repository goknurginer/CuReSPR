data_upload_ui <- function(id) {
  ns <- NS(id)

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
               numericInput(ns("num"), label = "", value = 0, min = 0),
               actionButton(ns("nextnum"), "Next"),
               conditionalPanel(condition = sprintf("input['%s'] > 0", ns("nextnum")),
                                hr(),
                                h4("Assign group names"),
                                helpText("Enter names for each group."),
                                uiOutput(ns("groupnames")),
                                actionButton(ns("nextupload"), "Next")
               ),
               conditionalPanel(condition = sprintf("input['%s'] > 0", ns("nextupload")),
                                p("Groups are ", textOutput(ns("groups"), inline = TRUE))
               )
             ),
             mainPanel(
               conditionalPanel(condition = sprintf("input['%s'] > 0", ns("nextupload")),
                                h4("Upload guide RNA library"),
                                fileInput(ns("uploadguides"), label = "", accept = c('.tsv', '.csv', '.txt'), multiple = TRUE),
                                hr(),
                                h4("Upload fastq files"),
                                fileInput(ns("upload"), label = "", accept = c('.fastq', 'fastq.gz'), multiple = TRUE),
                                checkboxInput(ns("paired"), label = "Fastq files are paired-end"),
                                checkboxInput(ns("tech"), "There are technical replicates"),
                                checkboxInput(ns("bio"), "There are biological replicates"),
                                actionButton(ns("nextdatatable"), "Next")
               ),
               conditionalPanel(condition = sprintf("input['%s'] > 0", ns("nextdatatable")),
                                hr(),
                                h4("Enter sample details"),
                                helpText("Please enter the details about the samples in the following table."),
                                actionButton(ns("gotocounting"), "Go to counting"),
                                DT::dataTableOutput(ns("dataTable")),
                                verbatimTextOutput(ns('sel'))
               )
             )
           )
  )
}
