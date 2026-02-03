source("src/main_ui.R")
source("src/main_server.R")
source("requirements/packages.R")


# 1. Installing packages
options(shiny.launch.browser = TRUE)
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
library(lubridate)
library(chorddiag)
library(ompr)
library(ompr.roi)
library(ROI)
library(ROI.plugin.highs)
library(magrittr)
library(rlang)


# 3. Load UI
ui <- main_ui_layout()


# 4. Load Server
server <- function(input, output, session) {
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



