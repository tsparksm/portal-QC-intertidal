#### SETUP ####
library(shiny)
library(here)
library(tidyverse)
library(kcmarine)
library(plotly)
library(lubridate)
library(htmlwidgets)
source(here("src", "utility_functions.R"))

# Suppress warnings (just for cleanliness)
options(warn = -1)

ui <- fluidPage(
  
)

server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
