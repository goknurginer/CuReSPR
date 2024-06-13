counting_ui <- function(id) {
  ns <- NS(id)

  tabPanel("Counting",
           sidebarLayout(
             sidebarPanel(
               h4("Select your method of counting"),
               selectInput(ns("counting"), "", choices = c("Rsubread", "MAGeCK", "WEHI")),
               actionButton(ns("guidecounts"), "Get the guide counts"),
             ),
             mainPanel(
               conditionalPanel(condition = sprintf("input['%s'] > 0", ns("guidecounts")),
                                uiOutput(ns("dynamic_ui"))
               )
             )
           )
  )
}
