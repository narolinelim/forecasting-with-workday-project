

# UI for Dashboard


dashboard_ui <- function() {
  
  div(
    class = "result-container",
    
    div(
      "Dashboard",
      class = "content-title",
      id = "dashboard-box",
      
      div(
        class = "info-containers",
        
        layout_columns(
          col_widths = c(6, 6),
          
          value_box(
            title = "Total Balance",
            "12000",
            "Balance before the expense",
            full_screen = FALSE,
            class = "info-box"
          ),
          
          value_box(
            title = "Ending Balance",
            "0",
            "Balance after all expenses",
            full_screen = FALSE,
            class = "info-box"
          )
        
        ),
        
        div(
          class = "value-box-row2",
          layout_columns(
            col_widths = c(6, 6),
            
            value_box(
              title = "Expense Breakdown",
              value = "Circos graph",
              class = "info-box",
              #tags$img(src = "circos.png", class = "circos-graph"),
              full_screen = TRUE
            ),
            
            value_box(
              title = "Shortfall Percentage",
              "N/A",
              full_screen = FALSE,
              class = "info-box"
            )
          )
        )
      )
      
    ),
    
    div(
      "Budget Allocation",
      class = "content-title",
      id = "budget-box",
      
      div(
        id = "budget-container",
        class = "info-containers",
        
        card(
          card_header(
            "This is budget table"
          )
        ),
        
        actionButton("budget_download", "Download Budget Allocation",
                     class = "budget_download_btn")
      )

    ),
    

    actionButton("exit_session", "Exit Session", class = "exit_session_btn"),

      
  )

}