

# Contains all server components from server folder

source("src/server/allocation-algorithm.R")
source("src/server/data-processing.R")
source("src/server/io.R")
source("src/server/sorting.R")
source("src/server/graph.R")
source("src/server/edit-rows.R")

main_server_logic <- function(input, output, session, values) {
  # Current page
  current_view <- reactiveVal("forecast")
  

  # --- EVENTS: Navigation between tabs ---
  observeEvent(input$dashboard_tab, current_view("dashboard"))
  observeEvent(input$forecast_tab, current_view("forecast"))
  observeEvent(input$funding_tab, current_view("funding"))
  observeEvent(input$expense_tab, current_view("expense"))

  # --- OUTPUT: Switching between tabs ---
  output$tab_content <- renderUI({
    switch(
      current_view(),
      "dashboard" = dashboard_ui(),
      "forecast" = forecast_ui(),
      "funding" = funding_ui(),
      "expense" = expense_ui()
    )
  })

  # --- OUTPUT: Generating Forecast Notification ---
  observe({
    showNotification(
      "Allocation Finished. Go to Dashboard for result.",
      duration = 3
    )
  }) |>
    bindEvent(input$generate_forecast)

  # --- EVENTS: Exit Session Popup ---
  observeEvent(input$exit_session, {
    showModal(
      tagAppendAttributes(
        modalDialog(
          title = "Exiting Session",
          "All data from this session will be deleted",
          easyClose = TRUE,
          footer = tagList(
            actionButton("return_to_session", "Return", class = "session-btn"),
            actionButton("end_session", "Exit Session", class = "session-btn")
          )
        ),
        class = "exit-session-popup"
      )
    )
  })

  # --- EVENT: Return to session ---
  observeEvent(input$return_to_session, {
    removeModal()
    current_view("dashboard")
  })

  # Render priority mode
  output$priority_card <- renderUI({
    if (input$select_priority == "Manual Priority") {
      manual_priority_ui()
    } else {
      if (!is.null(pending_order())) {
        showNotification("Unsaved manual order discarded", type = "message", duration = 3)
        pending_order(NULL)
      }
      column_priority_ui()
    }
  })

  # Render first priority
  output$first_priority <- renderUI({
    if (input$select_first_priority_item == "Payment Date") {
      payment_date_view()
    } else {
      categories_view()
    }
  })

  # Render second priority
  output$second_priority <- renderUI({
    if (input$select_second_priority_item == "Payment Date") {
      payment_date_view()
    } else if (input$select_second_priority_item == "Categories") {
      categories_view()
    } else if (input$select_second_priority_item == "None") {
      div("No second priority.", class = "no-second-priority")
    }
  })

  # Dragging feature for categories priority
  observeEvent(input$drag_categories, {
    input$drag_categories
  })

  # --- EVENT: Manual Row Reordering ---
  # Temp order and proxy table
  pending_order <- reactiveVal(NULL)
  proxy <- dataTableProxy("sample_manual_table")

  # Update temp order when user drags rows
  observeEvent(input$newOrder, {
    new_idx <- match(input$newOrder, values$expenses$priority)
    pending_order(values$expenses[new_idx, ])
  })

  # Save manual order
  observeEvent(input$save_manual_order, {
    values$expenses <- row_reorder(input$newOrder, values$expenses, proxy, id_col = "priority")
    pending_order(NULL)
    showNotification("Manual order saved", type = "message", duration = 3)
  })

  # Cancel manual order
  observeEvent(input$cancel_manual_order, {
    pending_order(NULL)
    showNotification("Manual order cancelled", type = "message", duration = 3)
  })

  # --- EVENT: Upload Expenses and Funding Data ---
  observeEvent(input$spreadsheet_upload, {
    req(input$spreadsheet_upload)
    path <- input$spreadsheet_upload$datapath
    
    tryCatch({
      data_list <- read_excel_data(path)
      funding_sources_df <- data_list$funding_sources
      expense_df <- data_list$expense

      values$funding_sources <- funding_sources_df
      values$expenses <- expense_df
      showNotification("Data saved successfully", type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("Upload failed:", e$message), type = "error", duration = 3)
    })
  })
  
  

  # ----------------------------
  # SORTING LOGIC
  # This section handles sorting of expenses based on user selection:
  # - Manual sorting (drag-and-drop order from UI)
  # - Sort by column (user-defined criteria and category order)
  # The output 'values$expenses_sorted' includes 'final_order' column
  # and will be used as input for the allocation algorithm
  # ----------------------------

  # ----------------------------
  # 1. Watch for changes in sorting mode from UI
  # 'sorting_mode' should come from a dropdown: "manual" or "by_rules"
  # ----------------------------
  observeEvent(input$sorting_mode, {
    # Get the selected sorting mode
    mode_selected <- input$sorting_mode

    # Get the processed expenses data from previous step
    # Must include 'original_index' for tie-breaking
    expenses_data <- values$expenses_data

    # ----------------------------
    # 2. Manual sorting
    # ----------------------------
    if (mode_selected == "manual") {
      # TODO: Retrieve user's drag-and-drop order from UI
      # This should be a dataframe 'manual_order' with column 'id'
      # matching expenses_data$id
      manual_order <- NULL # placeholder

      # Call the sorting function
      expenses_sorted <- main_sorting_expenses(
        expenses_data = expenses_data,
        mode = "manual",
        manual_order = manual_order
      )
    } else if (mode_selected == "by_rules") {
      # ----------------------------
      # 3. Sort by column (by_rules)
      # ----------------------------
      # TODO: Retrieve user's ordering rules from UI
      # Example format:
      # list(
      #   criteria = c("latest_payment_date", "category"),
      #   category_order = c("salary", "travel", "research")
      # )
      ordering_rules <- NULL # placeholder

      # Call the sorting function
      expenses_sorted <- main_sorting_expenses(
        expenses_data = expenses_data,
        mode = "by_rules",
        ordering_rules = ordering_rules
      )
    } else {
      stop("Invalid sorting mode selected")
    }

    # ----------------------------
    # 4. Save sorted expenses to reactive values
    # The sorted dataframe includes 'final_order' column and will be
    # used as input for the allocation algorithm
    # ----------------------------
    values$expenses_sorted <- expenses_sorted

    # ----------------------------
    # TODOs for future integration:
    # - Connect 'manual_order' to the actual UI drag-and-drop result
    # - Connect 'ordering_rules' to the UI ordering rules drag-and-drop
    # ----------------------------
  })

  # --- EVENTS: Add Funding Button ---
  # Adding new funding form
  observeEvent(input$add_funding, {
    showModal(upload_funding_modal())
  })

  observeEvent(input$add_funding_confirm, {
    add_funding_button(input, values)
    removeModal()
  })

  # --- EVENTS: Delete Funding Button ---
  # Delete button pop up
  output$delete_funding_btn <- renderUI({
    selected <- input$sample_funding_table_rows_selected
    if (length(selected) > 0) {
      actionButton("delete_funding", "Delete Selected Funding", class = "delete-data-btn")
    }
  })

  # Deleting selected funding
  observeEvent(input$delete_funding, {
    selected <- input$sample_funding_table_rows_selected
    values$funding_sources <- delete_row(values$funding_sources, selected)
  })

  # --- EVENTS: Add Expense Button ---
  # Adding new expense form
  observeEvent(input$add_expense, {
    showModal(upload_expense_modal())
  })

  observeEvent(input$add_expense_confirm, {
    add_expense_button(input, values)
    removeModal()
  })

  # --- EVENTS: Delete Expense Button ---
  # Delete button pop up
  output$delete_expense_btn <- renderUI({
    selected <- input$sample_expense_table_rows_selected
    if (length(selected) > 0) {
      actionButton("delete_expense", "Delete Selected Expense", class = "delete-data-btn")
    }
  })

  # Deleting selected expense
  observeEvent(input$delete_expense, {
    selected <- input$sample_expense_table_rows_selected
    values$expenses <- delete_row(values$expenses, selected)
  })

  # Sample table outputs (for viewings only)
  # output$sample_budget_table <- renderDT({
  #   datatable(penguins)
  # })

  # output$sample_leftover_table <- renderDT({
  #   datatable(penguins)
  # })

  output$sample_funding_table <- renderDT({
    
    datatable(
      values$funding_sources,
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
  }, server = FALSE)
  

  output$sample_expense_table <- renderDT({
    datatable(
      values$expenses |> select(-old_index),
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
  }, server = FALSE)

  output$sample_manual_table <- renderDT({
    if(!is.null(pending_order())) {
      df <- pending_order()
    }
    else {
      df <- values$expenses
    }
    datatable(
      df |> select(-old_index),
      extensions = 'RowReorder',
      selection = 'none',
      callback = JS(row_reorder_callback),
      options = list(
        rowReorder = TRUE,
        pageLength = 100
      ),
      rownames = FALSE
    )
  })
  
  output$sample_priority_table <- renderDT({
    datatable(
      values$expenses |> select(-old_index),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = '<"row"<"col-sm-12"l>><"row"<"col-sm-12"f>>rtip'
      ),
      rownames = FALSE
    )
  })

  # output$sample_table <- renderDT({
  #   datatable(
  #     values$expenses(),
  #     options = list(
  #       pageLength = 10,
  #       scrollY = "300px",
  #       scrollX = TRUE,
  #       dom = '<"row"<"col-sm-12"l>><"row"<"col-sm-12"f>>rtip'
  #     )
  #   )
  # })
}





