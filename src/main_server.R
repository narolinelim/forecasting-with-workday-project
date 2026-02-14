
source("src/server/dashboard.R")
source("src/server/forecast-page.R")
source("src/server/input-page.R")
source("src/server/edit-page.R")



main_server_logic <- function(input, output, session, values) {
  # ---- 1. GENERAL LOGIC ----

  ## ---- Current page ----
  current_view <- reactiveVal("forecast")

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
        showNotification(error, type = "warning", duration = NULL)
      } else {
        showNotification(error, type = "error", duration = NULL)
      }
    }
    errors(NULL)
  }) %>%
    bindEvent(errors(), ignoreInit = TRUE)

  ## ---- EVENT: Navigation between tabs ----
  observeEvent(input$dashboard_tab, current_view("dashboard"))
  observeEvent(input$forecast_tab, current_view("forecast"))
  observeEvent(input$input_tab, current_view("input"))
  observeEvent(input$edit_tab, current_view("edit"))

  ## ---- OUTPUT: Switching between tabs ----
  output$tab_content <- renderUI({
    switch(
      current_view(),
      "dashboard" = dashboard_ui(
        total_balance = sum(values$funding_sources$amount)
      ),
      "forecast" = forecast_ui(),
      "input" = input_ui(),
      "edit" = edit_ui()
    )
  })

  
  # ---- 2. INPUT PAGE LOGIC ----
  input_server(input, output, session, values, current_view)

  # ---- 3. FORECAST PAGE LOGIC ----
  forecast_server(input, output, session, values, current_view)

  # ---- 4. EDIT PAGE LOGIC ----
  edit_server(input, output, session, values, current_view)

  # ---- 5. DASHBOARD PAGE LOGIC ----
  dashboard_server(input, output, session, values, current_view)
  
  
}



