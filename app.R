
source("src/main_ui.R")
source("src/main_server.R")
source("requirements/packages.R")


# 1. Installing packages

run_setup()


# 2. Load packages
library(circlize)
library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(readxl)
library(openxlsx)
library(tidyr)
library(rmarkdown)
library(shinyjs)
library(tinytex)
library(RColorBrewer)
library(palmerpenguins)
library(shinyWidgets)
library(sortable)
library(ggplot2)
library(plotly)


# library(Matrix)



# 3. Load UI

ui <- main_ui_layout()



# 4. Load Server
server <- function(input, output, session) {
  # If test.R exists, source it into the server environment so it can set `values`
  if (file.exists("test.R")) {
  source("test.R", local = TRUE)
  } else {
  # Otherwise, initialize `values` as empty data frames
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
      expense_id = character(),
      source_id = character(),
      expense_category = character(),
      expense_amount = numeric(),
      allocated_amount = numeric(),
      latest_payment_date = as.Date(character()),
      allocation_status = logical()
    ),
    funding_summary = data.frame(
      source_id = character(),
      funding_source = character(),
      initial_amount = numeric(),
      used_amount = numeric(),
      remaining_amount = numeric()
    )
  )
  }
  
  main_server_logic(input, output, session, values)
  main_output(input, output, session, values)
  
}



# 5. Run Shiny App
shinyApp(ui, server)



