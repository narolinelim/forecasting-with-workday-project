

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
                actionButton("download_template", "Download Template",
                                 class = "template-download-btn")
              )
            )
            
    
          )
        ),
        
        
        
        # Priority Card
        
        card(
          id = "set_priority_card",
          
          div(
            p("Set Priority", class = "card-title"),
            
            div(
              selectInput(
                "select_priority",
                label = NULL,
                choices = list("Manual Priority", "Column Priority"),
                selectize = TRUE,
              ),
              style = "margin-left: 10px; margin-top: 5px;"
            ),
            
            DTOutput("empty_table")
            
          )
          
        )
        
      ),
      
      actionButton("generate_forecast", "Generate Forecast", class = "generate_forecast_btn")
    )
    
  )

  
}


manual_priority_ui <- function() {
  
}


column_priority_ui <- function() {
  
}





