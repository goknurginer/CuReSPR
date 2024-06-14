library(shiny)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(refund.shiny)
library(tidyverse)

options(shiny.reactlog = TRUE)
options(shiny.maxRequestSize = 100 * 1024^2)

path <- "/Users/giner.g/Documents/Github/CuReSPR/datasets/T8"

# Source module scripts
source("../modules/data_upload.R")
source("../modules/counting.R")

# Define UI
ui <- navbarPage("CuReSPR", id = "main", theme = shinytheme("cerulean"),
                 tabsetPanel(id = "inTabset",
                             data_upload_ui("data_upload"),
                             counting_ui("counting"),
                             tabPanel("Preprocessing", h4("Testing counting"), fluidPage(sidebarPanel())),
                             tabPanel("Differential Analysis", h4("Testing counting"), fluidPage(sidebarPanel())),
                             tabPanel("Pathway Analysis", h4("Testing counting"), fluidPage(sidebarPanel()))
                 )
)

# Define server logic
server <- function(input, output, session) {
  data_upload_server("data_upload", session)
  counting_server("counting", path = path)
}

# Run the app
shinyApp(ui = ui, server = server)
