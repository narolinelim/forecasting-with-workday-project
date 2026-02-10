
options(repos = c(CRAN = "https://cloud.r-project.org"))

REQUIRED_PACKAGES <- c("shiny", "bslib", "DT", "dplyr", "readxl", 
                       "openxlsx", "tidyr", "rmarkdown", "shinyWidgets", 
                       "sortable", "plotly", "htmlwidgets", "ompr",
                       "ompr.roi", "ROI", "ROI.plugin.highs",
                       "lubridate", "remotes", "magrittr", "shinyjs")

run_setup <- function() {
  
  # Identify which packages are not yet installed on the system
  new_packages <- REQUIRED_PACKAGES[!(REQUIRED_PACKAGES %in% installed.packages()[,"Package"])]
  
  # Install missing packages if any are found
  if(length(new_packages)) install.packages(new_packages)
  
  # Github packages (for chordDiag)
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  
  if (!requireNamespace("chorddiag", quietly = TRUE)) {
    remotes::install_github("mattflor/chorddiag") 
  }
  
}