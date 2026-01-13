
# Contains all ui components from ui folder
# Divided into four sections (pages)
# 1. Dashboard (default)
# 2. Forecast
# 3. Funding 
# 4. Expense

source("src/ui/dashboard.R")
source("src/ui/forecast-page.R")
source("src/ui/funding-page.R")
source("src/ui/expense-page.R")

main_ui_layout <- function() {
  
  # Custom theme Bootstrap
  custom_theme <- bs_theme(
    version = 5,
    base_font = font_google(
      "Inter",
      wght = c(400, 500, 600, 700, 800, 900)
      )
  )
  
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
    useShinyjs(),
    
    sidebar = sidebar(
      width = 300,
      
      actionButton("dashboard_tab", "Dashboard", class = "dashboard_tab_btn", class = "tab-buttons"),
      actionButton("forecast_tab", "Forecast", class = "forecast_tab_btn", class = "tab-buttons"),
      actionButton("funding_tab", "Funding", class = "funding_tab_btn", class = "tab-buttons"),
      actionButton("expense_tab", "Expense", class = "expense_tab_btn", class = "tab-buttons")
    ),
    
    uiOutput("tab_content"),
    
    
    tags$body(tags$link(rel = "stylesheet", href = "style.css"))
    
    
  )
    
    
  
  return (main_platform)
  

}