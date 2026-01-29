# UI for Dashboard
dashboard_ui <- function(total_balance) {
  div(
    class = "result-container",

    # Main Dashboard Section
    div(
      "Dashboard",
      class = "content-title",
      class = "dashboard-box",
      
      div(
        class = "info-containers",
        layout_columns(
          col_widths = c(6, 6),
          
          value_box(
            title = "Total Balance",
            uiOutput("total_balance"),
            "Balance before the expense",
            full_screen = FALSE,
            class = "info-box"
          ),
          
          value_box(
            title = "Number of Shortfalls",
            uiOutput("shortfall_number"),
            "Total number of shortfalls",
            full_screen = FALSE,
            class = "info-box"
          )
        ),
        
        card(
          class = "graphic-output",
          p("Expense and Shortfall Breakdown", style = "font-size: 16px;"),
            
          card(
            id = "shortfall-graph",
            card_header("Shortfall Breakdown"),
            card_body(
              height = "auto",
              uiOutput("shortfall_plot")
            )
          ),
            
          card(
            id = "expense-graph",
            card_header("Expense Breakdown"),
            height = "auto",
            uiOutput("circos_container")
          ),
        
          style = "font-weight: normal;"
        )
      )
    ),
    
    # Budget Allocation Section
    div(
      "Budget Allocation",
      class = "content-title",
      class = "dashboard-box",
      
      div(
        class = "info-containers",
        card(
          div(
            DTOutput("budget_allocation_table")
          ),
          full_screen = TRUE,
          style = "padding: 16px; font-weight: 400; font-size: 16px;"
        )
      )
    ),
    
    # Unallocated Funding Section
    div(
      "Unallocated Funding",
      class = "content-title",
      class = "dashboard-box",
      
      div(
        class = "info-containers",
        card(
          div(
            DTOutput("unallocated_funding_table")
          ),
          full_screen = TRUE,
          style = "padding: 16px; font-weight: 400; font-size: 16px;"
        )
      )
    ),
    downloadButton("budget_download", "Download Budget Allocation", class = "budget_download_btn"),
    actionButton("exit_session", "Exit Session", class = "exit_session_btn")
  )
}