
options(repos = c(CRAN = "https://cloud.r-project.org"))

REQUIRED_PACKAGES <- c("magrittr", "shiny", "bslib", "DT", "dplyr", "readxl", 
                       "openxlsx", "tidyr", "rmarkdown", "shinyWidgets", 
                       "sortable", "plotly", "htmlwidgets", "ompr",
                       "ompr.roi", "ROI", "ROI.plugin.highs",
                       "lubridate", "remotes", "shinyjs", "jsonlite")

run_setup <- function() {
  
  # Identify which packages are not yet installed on the system
  new_packages <- REQUIRED_PACKAGES[!(REQUIRED_PACKAGES %in% installed.packages()[,"Package"])]
  
  # Install missing packages if any are found
  if(length(new_packages)) install.packages(new_packages, type = "binary")
  
  # Github packages from remotes (for chordDiag)
  if (!requireNamespace("chorddiag", quietly = TRUE)) {
    remotes::install_github("mattflor/chorddiag") 
  }
  
}
