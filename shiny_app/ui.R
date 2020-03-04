#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Are faculty representative of their students?"),
  
  pickerInput(
    inputId = "school",
    label = "Choose a School", 
    choices = sort(unique(demographics$Institution.Name)),
    options = list(
      `live-search` = TRUE,
       title = "All")
  ),
  hr(),
  
  fluidRow(
    column(4, formattableOutput("eth_table")),
    column(8, plotOutput("eth_plot"))
  )

  ## Display some basic info about the school? At least the info I already have in the data. 
  
))
