
source("src/main_ui.R")
source("src/main_server.R")
source("requirements/packages.R")


# 1. Installing packages from requirements/packages.R

run_setup()


# 2. Load packages
    
library(shiny)              # Main framework for the web app            
library(bslib)              # For modern UI components (cards, value boxes...)
library(DT)                 # For displaying data tables
library(dplyr)              # For manipulating data
library(readxl)             # For reading Excel spreadsheets
library(openxlsx)           # For creating and writing Excel spreadsheets
library(tidyr)              # For tidying up data
library(rmarkdown)          # For creating reports or documents (used for modelling output for allocation algorithm)
library(shinyWidgets)       # For input controls (pickerInput)
library(sortable)           # For sorting feature (dragging categories)
library(plotly)             # For plotting interactive graphs (shortfall graph)
library(htmlwidgets)        # For activating JS in graphical visualizations 
library(ompr)
library(ompr.roi)
library(ROI)
library(ROI.plugin.highs)
library(magrittr)



# 3. Load UI

ui <- main_ui_layout()



# 4. Load Server

server <- function(input, output, session) {
  
  # --- Main Reactive Values ----
  values <- reactiveValues(
    funding_sources = data.frame(
      source_id = character(),
      funding_source = character(),
      allowed_categories = character(),
      valid_from = as.Date(character()),
      valid_to = as.Date(character()),
      amount = numeric(),
      notes = character()
    ),
    expenses = data.frame(
      priority = integer(),
      expense_id = character(),
      expense_name = character(),
      expense_category = character(),
      planned_amount = numeric(),
      latest_payment_date = as.Date(character()),
      notes = character()
    ),
    allocation_result = data.frame(
      source_id = character(),
      expense_id = character(),
      expense_category = character(),
      allocated_amount = numeric()
    ),
    funding_summary = data.frame(
      source_id = character(),
      initial_amount = numeric(),
      used_amount = numeric(),
      remaining_amount = numeric()
    ),
    expense_status = data.frame(
      expense_id = character(),
      expense_category = character(),
      planned_amount = character(),
      latest_payment_date = as.Date(character()),
      filled_amount = numeric(),
      is_filled = logical(),
      status = character()
    ),
    full_budget_allocation_df = data.frame(
      source_id = character(),
      expense_id = character(),
      expense_category = character(),
      allocated_amount = numeric(),
      planned_amount = numeric(),
      latest_payment_date = as.Date(character()),
      status = character()
    )
  )
  
  main_server_logic(input, output, session, values)
  main_output(input, output, session, values)
  
}



# 5. Run Shiny App
shinyApp(ui, server)



