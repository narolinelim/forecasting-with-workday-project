
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
    version = 5
  )
  
  main_platform <- page_sidebar(
    title = div(
      img(src = "wehi_logo.png",
               height = "60px",
               width = "auto",
               style = "margin: 25px;"),
      "Forecasting Budget Tool",
      style = "font-size: 25px;"
    ),
    
    useShinyjs(),
    
    sidebar = sidebar(
      width = 300,
      
      actionButton("dashboard_tab", "Dashboard", class = "dashboard_tab_btn", class = "tab-buttons"),
      actionButton("forecast_tab", "Forecast", class = "forecast_tab_btn", class = "tab-buttons"),
      actionButton("funding_tab", "Funding", class = "funding_tab_btn", class = "tab-buttons"),
      actionButton("expense_tab", "Expense", class = "expense_tab_btn", class = "tab-buttons")
    ),
    
    uiOutput("tab_content"),
    
    
    tags$body(
      tags$style(HTML("
      
        /* TABS */
        
        .tab-buttons {
          border: none;
          border-radius: 25px;
          background-color: #FDD9D9;
        }
      
        /* DASHBOARD */
      
        .main.bslib-gap-spacing.html-fill-container {
          background-color: #F5F5F5 !important;
          margin: 0 !important;
          padding: 0 !important;
        }
        
        .result-container {
          margin: 30px 250px;
          padding-bottom: 50px;
        }
        
        .content-title {
          font-size: 30px;
          font-weight: bold;
        }
        
        .info-containers {
          margin-top: 20px;
        }
        
        #dashboard-box {
          margin-bottom: 40px;
        }
        
        #budget-box {
          margin-bottom: 120px;
        }
      
        .budget_download_btn {
          background-color: #D1FFE4;
          color: black;
          float: right;
          border: none;
          border-radius: 15px;
        }
        
        .exit_session_btn {
          width: 100%;
          background-color: #FFDCDC;
          color: black;
          border: none;
          border-radius: 15px;
        }
        
        .value-box-row2 {
          height: auto;
        }
        
        .circos-graph {
          width: 100%;
          height: auto;
          object-fit: contain;
          display: block;
        }
        
        
        /* FORECAST PAGE */
        
        .card-title {
          font-size: 20px;
          margin-left: 10px;
          margin-bottom: 0;
        }
        
        #upload-container {
          display: flex;
          justify-content: space-between;
          align-items: center;
          margin: 15px 10px 0 10px;
        }
        
        #left-upload {
          display: flex;
          align-items: center;
          gap: 20px;
          flex: 1;
          min-width: 0;
        }
        
        #left-upload .shiny-input-container {
          width: 500px;
        }
        
        #left-upload .input-group {
          display: flex;
          align-items: center;
          width: 100%;
          flex: 1;
          min-width: 0;
        }
        
        #left-upload .input-group-btn.input-group-prepend {
          flex: 0 0 auto;
        }
        
        .btn.btn-default.btn-file {
          border: none;
          border-radius: 15px !important;
          background-color: #0080FF;
          color: white;
          overflow: hidden;
        }
        
        #left-upload .btn-file:hover {
          background-color: #404040;
        }
        
        #left-upload input[type='text'] {
          width: auto;
          margin-left: 12px;
          border-radius: 10px;
          max-width: 40ch;
          margin-top: 5px;
        }
        
        
        #left-upload .shiny-file-input-progress {
          margin-top: 8px;
          width: auto;
        }
        
        #right-download {
          flex-shrink: 0;
          align-self: flex-start;
        }
        
        .template-download-btn {
          border: none;
          border-radius: 15px;
          background-color: #F0F0F0;
        }
        
        .generate_forecast_btn {
          width: 100%;
          background-color: #D1FFE4;
          color: black;
          border: none;
          border-radius: 15px;
        }
        
        /* FUNDING AND EXPENSE PAGES */
        
        .input-title-container {
          display: flex;
          justify-content: space-between;
          align-items: center;
          width: 100%;
        }

                      
      "))
    )
  
    
    
  )
    
    
  
  return (main_platform)
  

}