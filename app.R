
source("src/main_ui.R")
source("src/main_server.R")
source("requirements/packages.R")


# 1. Installing packages

run_setup()


# 2. Load packages

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

# library(gurobi)
# library(Matrix)


# 3. Load UI

ui <- main_ui_layout()



# 4. Load Server
server <- function(input, output, session) {
  
  # Memory (to be filled)
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
      item_id = character(),
      expense_category = character(),
      planned_amount = numeric(),
      latest_payment_date = as.Date(character()),
      notes = character(),
      old_index = integer()
    ),
    allocation_result = data.frame()
  )
  
  main_server_logic(input, output, session, values)
  main_output(input, output, session, values)
  
}



# 5. Run Shiny App
shinyApp(ui, server)



