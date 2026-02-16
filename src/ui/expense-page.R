

expense_ui <- function() {
  # ---- Layout of the Expense Page ----
  
  div(
    class = "result-container",
    
    div(
      
      div(
        class = "input-title-container",
        
        ## ---- 1. Expense Title Section ----
        div(
          "Expense",
          class = "content-title"
        ),
        div(
          actionButton("add_expense", "+ Add Expense", class = "add-data-btn")
        )
      ),
      
      ## ---- 2. Expense Data Table ----
      card(
        div(
          div(
            class = "delete-expense",
            actionButton("delete_expense", "Delete Selected Row(s)", class = "delete-data-btn")
          ),
          DTOutput("expense_table")
        ),
        full_screen = TRUE,
        class = "info-containers",
        style = "padding: 16px;"
      )
    ) 
  
  )
}


upload_expense_modal <- function(categories) {  
  # ---- Layout of the Add Expense Modal Popup ----
  #'
  #' @param categories: available input expense categories from previous inputs
  
  
  tagAppendAttributes(
    modalDialog(
      title = div(
        "Add New Expense",
        div("Add new expense manually", class = "modal-subtitle")
      ),
      easyClose = TRUE,
      footer = actionButton("add_expense_confirm", "Add Expense", class = "add-expense-confirm"),
    
      div(
        class = "expense-form",

        ## ---- 1. Expense Name ----
        div(
          id = "expense-name",
          class = "elongated-input",
          div("Expense Name", class = "data-input-headers"),
          textInput(
            "expense_name_input",
            label = NULL,
            value = "",
            placeholder = "Enter expense name..."
          )
        ),
        
        ## ---- 2. Expense Categories ----
        div(
          class = "expense-categories",
          
          div("Expense Categories", class = "data-input-headers"),
          
          div(
            selectizeInput(
              "expense_type",
              label = NULL,
              choices = categories,
              multiple = FALSE,
              options = list(
                create = TRUE,
                placeholder = "Select expense type...",
                openOnFocus = TRUE
              )
            )
          )
        ),
        
        ## ---- 3. Expense Amount ----
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
        
        
        ## ---- 4. Payment Date ----
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
        
        ## ---- 5. Expense Note ----
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





