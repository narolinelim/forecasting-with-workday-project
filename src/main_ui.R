
source("src/ui/dashboard-ui.R")
source("src/ui/forecast-page-ui.R")
source("src/ui/edit-page-ui.R")
source("src/ui/input-page-ui.R")

main_ui_layout <- function() {
  #' Contains all UI components from ui folder
  #' 
  #' Divides the app into four main pages
  #' 1. Funding (Default)
  #' 2. Expense
  #' 3. Forecast
  #' 4. Dashboard
  
  # App Custom Bootstrap Theme
  custom_theme <- bs_theme(
    version = 5,
    base_font = font_google(
      "Inter",
      wght = c(400, 500, 600, 700, 800, 900)
      )
  )
  useShinyjs()
  
  # Sidebar Layout
  main_platform <- page_sidebar(
    title = div(
      img(src = "wehi_logo.png",
               height = "60px",
               width = "auto",
               style = "margin: 25px;"),
      "Forecasting Budget Tool",
      style = "font-size: 25px; font-weight: 600; color: #575756;"
    ),
    theme = custom_theme,
    
    # Sidebar Tab Contents
    sidebar = sidebar(
      width = 270,
      
      actionButton("input_tab", "Input", class = "input_tab_btn", class = "tab-buttons"),
      actionButton("edit_tab", "Review", class = "edit_tab_btn", class = "tab-buttons"),
      actionButton("forecast_tab", "Forecast", class = "forecast_tab_btn", class = "tab-buttons"),
      actionButton("dashboard_tab", "Dashboard", class = "dashboard_tab_btn", class = "tab-buttons")
    ),
    
    uiOutput("tab_content"),
    tags$body(tags$link(rel = "stylesheet", href = "style.css"))
  )

  return (main_platform)
}
