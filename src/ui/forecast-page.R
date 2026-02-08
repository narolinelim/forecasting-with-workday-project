

forecast_ui <- function() {
  # ---- Layout of the Forecast Page ----
  
  div(
    class = "result-container",
    
    div(
      div("Forecast", class = "content-title"),
      
      div(
        class = "info-containers",
        
        ## ---- 1. Upload Excel File Section ----
        card(
          id = "upload-card",
          
          div(
            p("Upload the Excel file", class = "card-title"),
            
            div(
              id = "upload-container",
              
              div(
                id = "left-upload",
                
                fileInput(
                  inputId = "spreadsheet_upload",
                  label = NULL,
                  buttonLabel = "Choose File",
                  placeholder = "No file chosen",
                  accept = c(".xlsx", ".xls")
                )
              ),
              
              div(
                id = "right-download",
                downloadButton("download_template", "Download Template",
                                 class = "template-download-btn")
              )
            )
          )
        ),
        
        ## ---- 2. Setting Expense Priority Section ----
        card(
          id = "set-priority-card",
          full_screen = TRUE,
          
          div(
            p("Set Priority", class = "card-title"),
            
            div(
              class = "select_priority_input_type",
              selectInput(
                "select_priority",
                label = NULL,
                choices = c("Column Priority", "Manual Priority", "None"),
                selected = "None"
              )
            ),
            uiOutput("priority_card")
          ),
          style = "padding: 0; font-weight: normal;"
        )
      ),
      actionButton("generate_forecast", "Generate Forecast", class = "generate_forecast_btn")
    )
  )
}


manual_priority_ui <- function() {
  # ---- Manual Expense Priority Section ----
  
  div(
    DTOutput("sample_manual_table"),        
    actionButton("save_manual_order", "Save order", class = "btn-primary"),
    actionButton("cancel_manual_order", "Cancel", class = "btn-default"),
    style = "padding: 16px; font-weight: 400; font-size: 16px;"
  )
}


column_priority_ui <- function() {
  # ---- Column Expense Priority Section ----
  
  div(
    
    div(
      id = "priority-container",
      
      ## ---- 1. First Priority Card ----
      div(
        class = "priority-cards",
        card(
          id = "first-priority",
          class = "card-style",
          
          div(
            p("1st Priority", style = "margin-bottom: 5px; font-size: 16px;"),
            
            div(
              class = "select_priority_dropdown",
              pickerInput( # selectInput
                "select_first_priority_item",
                label = NULL,
                choices = c("Payment Date", "Categories"),
                options = list(style = "btn-outline-secondary")
          
              )
            ),
            uiOutput("first_priority")
          ),
          style = "padding: 0;"
        )
      ),
      
      ## ---- 2. Second Priority Card ----
      div(
        class = "priority-cards",
        card(
          id = "second-priority",
          class = "card-style",
          
          div(
            p("2nd Priority", style = "margin-bottom: 5px; font-size: 16px;"),
            
            div(
              class = "select_priority_dropdown",
              
              pickerInput( # selectInput
                "select_second_priority_item",
                label = NULL,
                choices = c("Categories", "Payment Date", "None"),
                # selected = "None",
                options = list(style = "btn-outline-secondary")
              )
            ),
            uiOutput("second_priority")
          ),
          style = "padding: 0;"
        )
      )
      
    ),
    
    hr(),
    
    ## ---- 3. Resulting Expense Data Table From Setting Priority ----
    div(
      p("Result Table", class = "card-title"),
      
      div(
        DTOutput("sample_expense_table"),
        style = "padding: 16px; font-weight: 400; font-size: 16px;"
      )
      # ,
      # div(
      #   actionButton("save_column_order", "Save order", class = "btn-primary"),
      #   actionButton("cancel_column_order", "Cancel", class = "btn-default"),
      #   style = "padding: 0 16px 16px 16px;"
      # )
    )
  )

  
}


payment_date_view <- function() {
  # ---- Column Priority: Payment Date View ----

  div(
    id = "payment-date-option",
    
    radioButtons(
      inputId = "payment-date-options",
      label = NULL,
      choiceNames = list(
        div(
          class = "radio-row",
          
          div(
            p("Earliest Payment Date", class = "radio-title"),
            p("Sort expenses by earliest payment date", class = "radio-body")
          )
          
        ),
        
        div(
          class = "radio-row",
          
          div(
            p("Latest Payment Date", class = "radio-title"),
            p("Sort expenses by latest payment date", class = "radio-body")
            
          )
        )
      ),
      choiceValues = c("earliest_payment_date", "latest_payment_date")
    )
  )
}


categories_view <- function(categories) {  
  # ---- Column Priority: Categories View ----
  #'
  #' @param categories: all unique funding categories
  
  div(
    class = "categories-container",
    tagList(
      
      div(
        class = "categories-table",
        
        div(
          class = "categories-header",
          
          div("Allowed Categories", class = "categories-title")
        ),
        
        div(
          class = "allowed-categories",
          rank_list(
            text = NULL,
            labels = categories,
            input_id = "drag_categories"
          )
        )
      )
    )
  )
}


none_priority_ui <- function() {
  # ---- Column Priority: No Priority Selected ----
  
  div(
    p("No priority selected. Expenses' priority will be listed in this order.", 
      style = "padding: 16px; font-weight: 400; font-size: 16px;"),
    DTOutput("sample_expense_table")
  )
}









