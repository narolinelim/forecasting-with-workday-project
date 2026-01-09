
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


# 3. Load UI

ui <- main_ui_layout()



# 4. Load Server
server <- function(input, output, session) {
  
  # Memory (to be filled)
  values <- reactiveValues(
    funding_sources = data.frame(
      source_id = character(),
      allowed_categories = list(),
      valid_from = as.Date(character()),
      valid_to = as.Date(character()),
      amount = numeric()
    ),
    expense = data.frame(
      item_id = character(),
      expense_category = character(),
      planned_amount = numeric(),
      latest_payment_date = as.Date(character())
    ),
    allocation_result = data.frame()
  )
  
  main_server_logic(input, output, session, values)
  main_output()
  
}



# 5. Run Shiny App
shinyApp(ui, server)



