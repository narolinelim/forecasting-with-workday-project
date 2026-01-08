

# Contains all server components from server folder

source("src/server/allocation-algorithm.R")
source("src/server/data-processing.R")
source("src/server/io.R")
source("src/server/sorting.R")
source("src/server/graph.R")


main_server_logic <- function(input, output, session, values) {
  
  
  # Current page
  current_view <- reactiveVal("forecast")
  
  observeEvent(input$dashboard_tab, current_view("dashboard"))
  observeEvent(input$forecast_tab, current_view("forecast"))
  observeEvent(input$funding_tab, current_view("funding"))
  observeEvent(input$expense_tab, current_view("expense"))
  
  
  # Switching between tabs
  output$tab_content <- renderUI({
    switch(current_view(),
           "dashboard" = dashboard_ui(),
           "forecast" = forecast_ui(),
           "funding" = funding_ui(),
           "expense" = expense_ui()
    )
  })
  
  output$empty_table <- renderDT({})
  
  
}

