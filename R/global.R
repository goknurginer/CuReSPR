# Load the necessary packages ####
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(refund.shiny)
library(tidyverse)
library(readr)
library(edgeR)

# Set maximum upload size to 1 GB ####
options(shiny.maxRequestSize = 1000 * 1024^2)

# Functions ####
# read_file_and_render function ####
# Define a function to read files and render DataTable
read_file_and_render <- function(file_input) {
  req(file_input)  # Ensure the file is uploaded

  # Extract file extension
  file_ext <- tools::file_ext(file_input$name)

  # Read file based on extension and detect separator if necessary
  df <- switch(file_ext,
               "csv" = read.table(file_input$datapath, check.names = FALSE, sep = ",", header = TRUE),
               "tsv" = read.table(file_input$datapath, check.names = FALSE, sep = "\t", header = TRUE),
               "txt" = {
                 # Detect separator for .txt files
                 first_line <- readLines(file_input$datapath, n = 1)
                 if (grepl("\t", first_line)) {
                   read.table(file_input$datapath, check.names = FALSE, sep = "\t", header = TRUE)
                 } else {
                   read.table(file_input$datapath, check.names = FALSE, sep = " ", header = TRUE)
                 }
               },
               stop("Unsupported file type"))

  # Set column names for the guide library if applicable
  if (file_ext %in% c("tsv", "csv", "txt")) {
    #colnames(df) <- c("sgRNA_ID", "sgRNA_sequence", "gene_ID")  # Assigning appropriate names
  }
  df
}
