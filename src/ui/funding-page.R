# UI for Funding Page
funding_ui <- function() {
  
  div(
    class = "result-container",
    
    div(
      id = "funding-container",
      
      div(
        class = "input-title-container",
        
        div(
          "Funding",
          class = "content-title"
        ),
        
        div(
          actionButton("add_funding", "+ Add Funding", class = "add_data_btn")
        )
      ),
      
      card(
        div(
          div(
            class = "delete-funding",
            actionButton("delete_funding", "Delete Selected Funding", class = "delete-data-btn")
          ),
          DTOutput("sample_funding_table")
        ),
        full_screen = TRUE,
        class = "info-containers",
        style = "padding: 16px;"
      )
    )
  )
}


upload_funding_modal <- function() {
  
  tagAppendAttributes(
    modalDialog(
      title = div(
        "Add New Funding",
        div("Add new funding manually", class = "modal-subtitle")
      ),
      easyClose = TRUE,
      footer = actionButton("add_funding_confirm", "Add Funding", class = "add-funding-confirm"),
      
      div(
        id = "funding-form",
        
        # Funding Source Name
        div(
          id = "funding-source",
          class = "elongated-input",
          
          div("Source Name", class = "data-input-headers"),

          textInput(
            "source_name_input", 
            label = NULL,
            placeholder = "Enter funding source..."
          )
        ),
        
        
        # Funding Allowed Categories
        div(
          id = "funding-allowed-categories",
          
          div("Allowed Categories", class = "data-input-headers"),
          
          div(
            selectizeInput(
              "add_allowed_categories",
              label = NULL,
              choices = list("Salary", "Travel"),
              multiple = TRUE,
              options = list(
                placeholder = "Select allowed categories..."
              )
            )
          )
        ),
        
        # Validity Date
        div(
          id = "date-container",
          
          div(
            class = "date-valid",
            div("Valid From", class = "data-input-headers"),
            airDatepickerInput(
              inputId = "valid_from_date",
              label = NULL,
              placeholder = "Select start date...",
              autoClose = TRUE,
              addon = "none"
            )
          ),
          
          div(
            class = "date-valid",
            div("Valid To", class = "data-input-headers"),
            airDatepickerInput(
              inputId = "valid_to_date",
              label = NULL,
              placeholder = "Select end date...",
              autoClose = TRUE,
              addon = "none"
            )
          )
        ),
        
        # Amount
        div(
          id = "funding-amount",
          class = "elongated-input",
          
          div("Amount", class = "data-input-headers"),
          
          textInput(
            "funding_amount",
            label = NULL,
            placeholder = "Enter funding amount..."
          )
        ),
        
        # Note
        div(
          id = "funding-note",
          class = "elongated-input",
          
          div("Note", class = "data-input-headers"),
          
          textInput(
            "funding_note",
            label = NULL,
            placeholder = "Enter note... (optional)"
          )
        )
      )
    ),
    class = "add-funding-popup"
  ) 
}










