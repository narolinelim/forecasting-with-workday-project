
# Loading Packages

packages <- c("shiny", "DT", "openxlsx", "readxl", "writexl", "shinyjs", "rmarkdown", "dplyr", "tidyr", "bslib")

new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]

if (length(new_packages)) install.packages(new_packages)

library(shiny)
library(DT)
library(openxlsx)
library(readxl)
library(writexl)
library(shinyjs)
library(rmarkdown)
library(dplyr)
library(tidyr)
library(bslib)


# ----------------------
# PRE-PROCESSING
# ----------------------

# DATA PARSING (reading excel...)



# DATA CLEANING




# ---------------------
# LOGIC
# ---------------------

# CALCULATIONS




# ALGORITHM






# ----------------------
# EXTRAS
# ----------------------

# DATA VALIDATION





# HELPER FUNCTIONS (formatting...)





# TEMPLATE CREATION




# UI





# Server side
server <- function(input, output, session) {
  
  
  
}

















