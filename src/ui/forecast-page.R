

# UI for Forecast Page


forecast_ui <- function() {
  
  div(
    class = "result-container",
    
    div(
      "Forecast",
      class = "content-title",
      
      div(
        class = "info-containers",
        
        # Upload Spreadsheet Card
        
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
              selectInput(
                "select_priority",
                label = NULL,
                choices = c("Column Priority", "Manual Priority")
              ),
              style = "margin-top: 5px;"
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
  
  div(
    DTOutput("sample_manual_table"),
    style = "padding: 16px; font-weight: 400; font-size: 16px;"
  )
}


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
            selectInput(
              "select_first_priority_item",
              label = NULL,
              choices = c("Latest Payment Date", "Categories")
            )
          )
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
            selectInput(
              "select_second_priority_item",
              label = NULL,
              choices = c("Categories", "Latest Payment Date")
            )
          )
        ),
        
        style = "padding: 0;"
      )
    )
  )
  
  
  
}











