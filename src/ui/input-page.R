input_ui <- function() {
  # ---- Layout of the Input Page ----

  div(
    class = "result-container",
    
    div(
      
      div(
        class = "input-title-container",
        
        ## ---- 1. Expense Title Section ----
        div(
          "Input Data",
          class = "content-title"
        ),
        div(
          downloadButton("download_sample_spreadsheet", "Download Filled Template", class = "add-data-btn")
        )
      ),
      
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
      )
    )
  )
}