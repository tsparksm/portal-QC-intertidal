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

# Define initial data and data date
if (!file.exists(data_fpath_rda)) {
  update_discrete()
}
initial_data <- process_discrete(load_discrete())
old_date <- substr(file.info(data_fpath_rda)$mtime, 1, 10)

# Determine initial year, station for plot
initial_years <- sort(unique(initial_data$Year), 
                      decreasing = TRUE)
initial_year <- initial_years[1]
initial_stations <- sort(unique(initial_data$Locator))
initial_station <- initial_stations[1]

ui <- fluidPage(
  # Application title
  titlePanel("Monitoring Portal - Intertidal QC"), 
  
  # Row of buttons
  fluidRow(
    # BUTTON - refresh downloaded discrete data
    column(2, 
           actionButton("refresh_data", "Download data")), 
    # TEXT - display date of discrete data file
    column(4, 
           textOutput("date_data"))
  ),
  
  # Checkboxes
  fluidRow(
    column(3, 
           checkboxInput("log", 
                         "Nutrients on log scale", 
                         value = FALSE)),
    column(3, 
           checkboxInput("include_bad", 
                         "Include bad data (shown as squares)", 
                         value = FALSE))
  ), 
  
  tags$hr(),
  
  # Select station to plot; year to highlight
  fluidRow(
    column(2, 
           selectInput("sites", 
                       label = "Site:", 
                       choices = initial_stations, 
                       selected = initial_station)), 
    column(2, 
           selectInput("years", 
                       label = "Year:", 
                       choices = initial_years, 
                       selected = initial_year))
  ), 
  
  # Plots!
  fluidRow(
    column(4, plotlyOutput("plot_T")), 
    column(4, plotlyOutput("plot_S"))
  ), 
  fluidRow(
    column(4, plotlyOutput("plot_entero")), 
    column(4, plotlyOutput("plot_fecal"))
  ), 
  fluidRow(
    column(4, plotlyOutput("plot_NH3")), 
    column(4, plotlyOutput("plot_NNN"))
  ), 
  fluidRow(
    column(4, plotlyOutput("plot_totalN")), 
    column(4, plotlyOutput("plot_P"))
  )
)

server <- function(input, output, session) {

}

# Run the application 
shinyApp(ui = ui, server = server)
