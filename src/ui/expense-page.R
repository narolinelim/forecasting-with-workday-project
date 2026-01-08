

# UI for Expense Page

expense_ui <- function() {
  
  div(
    class = "result-container",
    
    div(
      id = "expense-container",
      
      div(
        class = "input-title-container",
        
        div(
          "Expense",
          class = "content-title"
        ),
        
        div(
          actionButton("add_expense", "Add Expense", class = "add_expense_btn")
        )
        
      ),
      
      card(
        div(
          p("this is expense table")
        ),
        
        class = "info-containers"
      )
      
    )
    
  )
  
}