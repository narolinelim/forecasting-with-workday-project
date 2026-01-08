

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
          actionButton("add_funding", "Add Funding", class = "add_funding_btn")
        )
        
      ),
      
      card(
        div(
          p("this is funding table")
        ),
        
        class = "info-containers"
      )
      
    )
  
  )
  
}