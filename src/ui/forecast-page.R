

# UI for Forecast Page


forecast_ui <- function() {
  
  div(
    class = "result-container",
    
    div(
      div("Forecast", class = "content-title"),
      
      div(
        class = "info-containers",
        
        
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
                  accept = c(".xlsx")
                )
              ),
              
              div(
                id = "right-download",
                actionButton("download_template", "↓ Download Template",
                                 class = "template-download-btn")
              )
            )
          )
        ),
        
        
        
        # Priority Card
        
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
                choices = c("Column Priority", "Manual Priority")
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


# Manual Priority View

manual_priority_ui <- function() {
  
  div(
    DTOutput("sample_manual_table"),
    style = "padding: 16px; font-weight: 400; font-size: 16px;"
  )
}


# Column Priority View

column_priority_ui <- function() {
  
  div(
    id = "priority-container",
    
    div(
      class = "priority-cards",
      
      card(
        id = "first-priority",
        class = "card-style",
        
        div(
          p("1st Priority", style = "margin-bottom: 5px; font-size: 16px;"),
          
          div(
            class = "select_priority_dropdown",
            
            selectInput(
              "select_first_priority_item",
              label = NULL,
              choices = c("Latest Payment Date", "Categories")
            )
          ),
          
          uiOutput("first_priority")
        ),
        style = "padding: 0;"
      )
    ),
    
    div(
      class = "priority-cards",
      card(
        id = "second-priority",
        class = "card-style",
        
        div(
          p("2nd Priority", style = "margin-bottom: 5px; font-size: 16px;"),
          
          div(
            class = "select_priority_dropdown",
            
            selectInput(
              "select_second_priority_item",
              label = NULL,
              choices = c("Categories", "Latest Payment Date", "None")
            )
          ),
          
          uiOutput("second_priority")
        ),
        style = "padding: 0;"
      )
    )
  )
  
  
}




# Column Priority: Latest Payment Date View

latest_payment_date_view <- function() {
  DTOutput("sample_table")
}


categories <- list("Salary", "Travel", "Equipment", "Cheese")



# Column Priority: Allowed Categories View

categories_view <- function() {
  
  div(
    
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
            input_id = "drag_categories",
            options = sortable_options(
              swap = TRUE
            )
          )
          
        )
        
      )
      
    )
    
  )
  
}









