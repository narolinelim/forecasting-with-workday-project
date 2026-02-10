

funding_ui <- function() {
  # ---- Layout of the Funding Page ----
  
  div(
    class = "result-container",
    
    div(
      
      div(
        class = "input-title-container",
        
        ## ---- 1. Funding Title Section ----
        div(
          "Funding",
          class = "content-title"
        ),
        div(
          actionButton("add_funding", "+ Add Funding", class = "add-data-btn")
        )
      ),
      
      ## ---- 2. Funding Data Table ----
      card(
        div(
          div(
            class = "delete-funding",
            actionButton("delete_funding", "Delete Selected Row(s)", class = "delete-data-btn")
          ),
          DTOutput("funding_table")
        ),
        full_screen = TRUE,
        class = "info-containers",
        style = "padding: 16px;"
      )
    )
  )
}


upload_funding_modal <- function(categories) {
  # ---- Layout of the Add Funding Modal Popup ----
  #'
  #' @param categories: avaialble input funding categories from previous inputs
  
  
  tagAppendAttributes(
    modalDialog(
      title = div(
        "Add New Funding",
        div("Add new funding manually", class = "modal-subtitle")
      ),
      easyClose = TRUE,
      footer = actionButton("add_funding_confirm", "Add Funding", class = "add-funding-confirm"),
      
      div(
        class = "funding-form",
        
        ## ---- 1. Funding Source Name ----
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
        
        ## ---- 2. Funding Allowed Categories ----
        div(
          class = "funding-allowed-categories",
          
          div("Allowed Categories", class = "data-input-headers"),
          
          div(
            selectizeInput(
              "add_allowed_categories",
              label = NULL,
              choices = categories,
              multiple = TRUE,
              options = list(
                create = TRUE,
                placeholder = "Select allowed categories...",
                openOnFocus = TRUE
              )
            )
          )
        ),
        
        ## ---- 3. Validity Date ----
        div(
          class = "date-container",
          
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
        
        ## ---- 4. Funding Amount ----
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
        
        ## ---- 5. Funding Note ----
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










