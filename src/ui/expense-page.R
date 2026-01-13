

# UI for Expense Page

expense_ui <- function() {
  
  # note: content-title messing with bold text!
  
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
          actionButton("add_expense", "+ Add Expense", class = "add_data_btn")
        )
        
      ),
      
      card(
        div(
          DTOutput("sample_expense_table")
        ),
        full_screen = TRUE,
        class = "info-containers",
        style = "padding: 16px;"
      )
      
    ),
    
    actionButton("initial_download", "↓ Download Excel file", class = "initial-excel-download")
    
  )
  
}


upload_expense_modal <- function() {
  
  tagAppendAttributes(
    modalDialog(
      title = div(
        "Add New Expense",
        div("Add new expense manually", class = "modal-subtitle")
      ),
      easyClose = TRUE,
      footer = actionButton("add_expense_confirm", "Add Expense", class = "add-expense-confirm"),
    
      div(
        id = "expense-form",
        
        # Expense Categories
        div(
          id = "expense-categories",
          
          div("Expense Categories", class = "data-input-headers"),
          
          div(
            selectizeInput(
              "expense_type",
              label = NULL,
              choices = list("Salary", "Travel"),
              multiple = FALSE,
              options = list(
                placeholder = "Select expense type...",
                onInitialize = I('function() { this.setValue(""); }')
              )
            )
          )
        ),
        
        # Amount
        div(
          id = "expense-amount",
          class = "elongated-input",
          
          div("Amount", class = "data-input-headers"),
          
          textInput(
            "expense_amount",
            label = NULL,
            placeholder = "Enter expense amount..."
          )
        ),
        
        
        # Latest Payment Date
        div(
          id = "latest-payment-date",
          class = "date-valid",
          div("Latest Payment Date", class = "data-input-headers"),
          airDatepickerInput(
            inputId = "latest_payment_date",
            label = NULL,
            placeholder = "Select latest payment date...",
            autoClose = TRUE,
            addon = "none"
          )
        ),
        
        # Note
        div(
          id = "expense-note",
          class = "elongated-input",
          
          div("Note", class = "data-input-headers"),
          
          textInput(
            "expense_note",
            label = NULL,
            placeholder = "Enter note... (optional)"
          )
        )
        
      )
    ),
    class = "add-expense-popup"
  )
  
  
}






