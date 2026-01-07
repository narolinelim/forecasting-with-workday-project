
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
  values <- reactiveValues(NULL)
  
  main_server_logic(input, output, session, values)
  main_output()
  
}



# 5. Run Shiny App
shinyApp(ui, server)



