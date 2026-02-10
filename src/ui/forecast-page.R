

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
          class = "upload-card",
          
          div(
            p("Upload the Excel file", class = "card-title"),
            
            div(
              class = "upload-container",
              
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
          class = "set-priority-card",
          full_screen = TRUE,
          
          div(
            p("Set Priority", class = "card-title"),
            
            div(
              class = "select-priority-input-type",
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
      actionButton("generate_forecast", "Generate Forecast", class = "generate-forecast-btn")
    )
  )
}


manual_priority_ui <- function() {
  # ---- Manual Expense Priority Section ----
  
  div(
    DTOutput("manual_table"),        
    
    conditionalPanel(
      condition = "output.data_uploaded == true",
      
      div(
        class = "manual-buttons",
        
        actionButton("save_manual_order", "Save order", class = "save-manual-btn"),
        actionButton("cancel_manual_order", "Cancel", class = "cancel-manual-btn"),
        downloadButton("download_excel", "Download Excel file", class = "excel-dl-btn-manual")
      )
    ),
    
    style = "padding: 16px; font-weight: 400; font-size: 16px;"
  )
}


column_priority_ui <- function() {
  # ---- Column Expense Priority Section ----
  
  div(
    
    div(
      class = "priority-container",
      
      ## ---- 1. First Priority Card ----
      div(
        class = "priority-cards",
        card(
          id = "first-priority",
          class = "card-style",
          
          div(
            p("1st Priority", style = "margin-bottom: 10px; font-size: 16px;"),
            
            div(
              class = "select-priority-dropdown",
              pickerInput(
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
            p("2nd Priority", style = "margin-bottom: 10px; font-size: 16px;"),
            
            div(
              class = "select-priority-dropdown",
              pickerInput(
                "select_second_priority_item",
                label = NULL,
                choices = c("Categories", "Payment Date", "None"),
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
        DTOutput("expense_table"),
        
        conditionalPanel(
          condition = "output.data_uploaded == true",
          
          downloadButton("download_excel", "Download Excel file", class = "excel-dl-btn")
        ),
        
        style = "padding: 16px; font-weight: 400; font-size: 16px;"
      )
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
      style = "padding: 16px; font-weight: 400; font-size: 16px; text-align: center"),
    
    DTOutput("expense_table"),
    
    conditionalPanel(
      condition = "output.data_uploaded == true",
      
      downloadButton("download_excel", "Download Excel file", class = "excel-dl-btn")
    ),
    
    style = "padding: 16px; font-weight: 400; font-size: 16px;"
  )
}









