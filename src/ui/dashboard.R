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
            total_balance,
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
          layout_columns(
            col_widths = c(6, 6),
            
            card(
              id = "shortfall-graph",
              full_screen = TRUE,
              card_header("Shortfall Breakdown"),
              card_body_fill(
                height = 600,
                plotlyOutput("shortfall_plot", height = "100%") 
              )
            ),
              
            card(
              id = "expense-graph",
              full_screen = TRUE,
              card_header("Expense Breakdown"),
              card_body_fill(
                height = 600,
                uiOutput("circos_container")
              )
            )
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
            DTOutput("sample_budget_table")
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
            DTOutput("sample_leftover_table")
          ),
          full_screen = TRUE,
          style = "padding: 16px; font-weight: 400; font-size: 16px;"
        )
      )
    ),
    actionButton("budget_download", "↓ Download Budget Allocation", class = "budget_download_btn"),
    actionButton("exit_session", "Exit Session", class = "exit_session_btn")
  )
}