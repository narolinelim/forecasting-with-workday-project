

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
  
  
  # Exiting Session pop-up
  observeEvent(input$exit_session, {
    showModal(
      tagAppendAttributes(
        modalDialog(
          title = "Exiting Session",
          "All data from this session will be deleted",
          easyClose = TRUE,
          footer = tagList(
            actionButton("return_to_session", "Return", class = "session-btn"),
            actionButton("end_session", "Exit Session", class = "session-btn")
          )
        ),
        class = "exit-session-popup"
      )
    )
  })
  
  # Render priority mode
  output$priority_card <- renderUI({
    
    if (input$select_priority == "Manual Priority") {
      manual_priority_ui()
    } else {
      column_priority_ui()
    }
    
  })
  
  
  # Event: Adding New Funding
  observeEvent(input$add_funding, {
    showModal(upload_funding_modal())
  })
  

  # Event: Adding New Expense
  
  observeEvent(input$add_expense, {
    showModal(upload_expense_modal())
  })
  
  
  
  # Sample table outputs (for viewings only)
  output$sample_budget_table <- renderDT({datatable(penguins)})
  
  output$sample_funding_table <- renderDT({datatable(penguins)})
  
  output$sample_expense_table <- renderDT({datatable(penguins)})
  
  output$sample_manual_table <- renderDT({datatable(penguins)})
  
}








