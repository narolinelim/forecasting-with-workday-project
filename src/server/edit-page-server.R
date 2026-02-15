source("src/server/components/edit-rows.R")
source("src/server/components/data-processing.R")

edit_server <- function(input, output, session, values, current_view) {
  
  ## ---- EVENT: Upload Expenses and Funding Data ----
  observeEvent(input$spreadsheet_upload, {
    req(input$spreadsheet_upload)
    path <- input$spreadsheet_upload$datapath
    
    tryCatch(
      {
        data_list <- read_excel_data(path)
        funding_sources_df <- data_list$funding_sources
        expense_df <- data_list$expenses
        
        values$funding_sources <- funding_sources_df
        values$expenses <- expense_df
        
        showNotification("Data saved successfully", type = "message", duration = 3)
        
        current_view("forecast")
      },
      error = function(e) {
        showNotification(paste("Upload failed:", e$message), type = "error", duration = 3, ignoreInit = FALSE)
      }
    )
  })
  
  # ---- 3. FUNDING PAGE SERVER LOGIC ----

  ## ---- EVENT: Add Funding Button Form ----
  observeEvent(input$add_funding, {
    showModal(upload_funding_modal(categories = available_categories()))
  })

  ## ---- EVENT: Add Funding To Storage ----
  observeEvent(input$add_funding_confirm, {
    add_funding_button(input, values)
    removeModal()
  })

  ## ---- EVENT: Delete Funding Button ----
  observeEvent(input$delete_funding, {
    selected <- input$funding_table_rows_selected
    values$funding_sources <- delete_row(values$funding_sources, selected)
  })

  ## ---- OUTPUT: Funding Data Table ----

  ### --- Displaying funding data table headers ----
  display_funding_names <- c(
    priority = "Priority",
    source_id = "Source ID",
    funding_source = "Funding Source",
    allowed_categories = "Allowed Categories",
    valid_from = "Valid From",
    valid_to = "Valid To",
    amount = "Amount",
    notes = "Notes"
  )

  ### --- Render funding data table ----
  output$funding_table <- renderDT(
    {
      req(values$funding_sources)
      df <- values$funding_sources

      # Format date and allowed_categories for display
      df <- df %>%
        mutate(
          valid_from = format(as.Date(valid_from), "%d-%m-%Y"),
          valid_to = format(as.Date(valid_to), "%d-%m-%Y")
        )
      df$allowed_categories <- sapply(df$allowed_categories, function(x) {
        paste(x, collapse = ", ")
      })

      colnames(df) <- display_funding_names[names(df)]

      datatable(
        df,
        options = list(
          dom = "<'row align-items-center'
                <'col-sm-6'l>
                <'col-sm-6 text-end'B>
              >
              <'row'<'col-sm-12'f>>
              t
              <'row'
                <'col-sm-5'i>
                <'col-sm-7'p>
              >",
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    },
    server = FALSE
  )

  # ---- 4. EXPENSE PAGE SERVER LOGIC ----

  ## ---- EVENT: Add Expense Button Form ----
  observeEvent(input$add_expense, {
    showModal(upload_expense_modal(categories = available_categories()))
  })

  ## ---- EVENT: Add Expense To Storage ----
  observeEvent(input$add_expense_confirm, {
    add_expense_button(input, values)
    removeModal()
  })

  ## ---- EVENT: Delete Expense Button ----
  observeEvent(input$delete_expense, {
    selected <- input$expense_table_rows_selected
    values$expenses <- delete_row(values$expenses, selected)
  })

  ## ---- OUTPUT: Expense Data Table ----

  ### ---- Displaying expense data table headers ----
  display_expense_names <- c(
    priority = "Priority",
    expense_id = "Expense ID",
    expense_name = "Expense Name",
    expense_category = "Expense Category",
    planned_amount = "Planned Amount",
    latest_payment_date = "Latest Payment Date",
    notes = "Notes"
  )

  ### ---- Render expense data table ----
  output$expense_table <- renderDT(
    {
      req(values$expenses)
      df <- values$expenses

      # Format date for display
      df <- df %>%
        mutate(
          latest_payment_date = format(as.Date(latest_payment_date), "%d-%m-%Y")
        )
      colnames(df) <- display_expense_names[names(df)]

      datatable(
        df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "<'row align-items-center'
                <'col-sm-6'l>
                <'col-sm-6 text-end'B>
              >
              <'row'<'col-sm-12'f>>
              t
              <'row'
                <'col-sm-5'i>
                <'col-sm-7'p>
              >"
        ),
        rownames = FALSE
      )
    },
    server = FALSE
  )
}