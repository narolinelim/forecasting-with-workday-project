
source("src/server/dashboard_server.R")
source("src/server/forecast_page_server.R")
source("src/server/input_review_server.R")

main_server_logic <- function(input, output, session, values) {
  # ---- 1. GENERAL LOGIC ----

  ## ---- Current page ----
  current_view <- reactiveVal("input_review")

  ## ---- Data Validation ----
  errors <- reactiveVal(NULL)
  observe({
    req(values$funding_sources)
    req(values$expenses)
    validation_errors <- c()

    funding_errors <- data_validation(values$funding_sources, type = "funding")
    expense_errors <- data_validation(values$expenses, type = "expense")

    validation_errors <- c(funding_errors, expense_errors)

    errors(validation_errors)
  })

  observe({
    for (error in errors()) {
      if (grepl("Warning", error)) {
        showNotification(error, type = "warning", duration = 3)
      } else {
        showNotification(error, type = "error", duration = 3)
      }
    }
    errors(NULL)
  }) %>%
    bindEvent(errors(), ignoreInit = TRUE)

  ## ---- EVENT: Navigation between tabs ----
  observeEvent(input$dashboard_tab, current_view("dashboard"))
  observeEvent(input$forecast_tab, current_view("forecast"))
  observeEvent(input$input_review_tab, current_view("input_review"))

  ## ---- OUTPUT: Switching between tabs ----
  output$tab_content <- renderUI({
    switch(
      current_view(),
      "dashboard" = dashboard_ui(),
      "forecast" = forecast_ui(),
      "input_review" = input_review_ui()
    )
  })
  
  ## ---- REACTIVE: All Unique Available Categories ----
  available_categories <- reactiveVal(NULL)

  
  # ---- 2. FORECAST PAGE LOGIC ----
  forecast_server(input, output, session, values, current_view, available_categories)

  # ---- 3. INPUT & REVIEW PAGE LOGIC ----
  input_review_server(input, output, session, values, current_view, available_categories)

  # ---- 4. DASHBOARD PAGE LOGIC ----
  dashboard_server(input, output, session, values, current_view)
  
  
}



