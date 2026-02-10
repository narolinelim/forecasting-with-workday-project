

dashboard_ui <- function(total_balance) {
  # ---- Layout of the Dashboard Page ----
  
  div(
    class = "result-container",

    ## ---- 1. Main Dashboard Section ----
    div(
      "Dashboard",
      class = "content-title",
      class = "dashboard-box",
      
      div(
        class = "info-containers",
        
        ### --- 1.1. Total Balance & Total Number Of Shortfalls ----
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
            
          ### ---- 1.2. Shortfall Graph ----
          card(
            id = "shortfall-graph",
            card_header("Shortfall Breakdown"),
            card_body(
              height = "auto",
              uiOutput("shortfall_plot")
            )
          ),
            
          ### ---- 1.3. Allocation Chord Diagram ----
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
    
    ## ---- 2. Budget Allocation Section ----
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
    
    
    ## ----- 3. Unallocated Funding Section -----
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
    
    ## ---- 4. Button Components ----
    downloadButton("budget_download", "Download Budget Allocation", class = "budget-download-btn"),
    actionButton("exit_session", "Exit Session", class = "exit-session-btn")
  )
  
}

