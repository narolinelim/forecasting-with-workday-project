

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
  
  clicked_month <- reactiveVal(NULL)


  # --- EVENTS: Navigation between tabs ---
  observeEvent(input$dashboard_tab, current_view("dashboard"))
  observeEvent(input$forecast_tab, current_view("forecast"))
  observeEvent(input$funding_tab, current_view("funding"))
  observeEvent(input$expense_tab, current_view("expense"))

  # --- OUTPUT: Switching between tabs ---
  output$tab_content <- renderUI({
    switch(
      current_view(),
      "dashboard" = dashboard_ui(total_balance = sum(values$funding_sources$amount)),
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

  # --- EVENT: End Session ---
  observeEvent(input$end_session, {
    showNotification("Session Ended. All data cleared.", type = "message", duration = 3)
    removeModal()
    session$reload()
  })

  # --- EVENT: Return to session ---
  observeEvent(input$return_to_session, {
    removeModal()
    current_view("dashboard")
  })

  # Render priority mode
  output$priority_card <- renderUI({
    if (isTruthy(input$select_priority) && input$select_priority == "Manual Priority") {
      manual_priority_ui()
    } else {
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
    if (is.null(pending_order())) {
      showNotification("No manual order to cancel", type = "warning", duration = 3)
      return()
    }
    pending_order(NULL)
    showNotification("Manual order cancelled", type = "message", duration = 3)
  })

  # When leaving manual priority view, clear pending order
  observeEvent(input$select_priority, {
    if (!is.null(pending_order()) && input$select_priority != "Manual Priority") {
      pending_order(NULL)
      showNotification("Unsaved manual order discarded", type = "message", duration = 3)
    }
  }, ignoreNULL = FALSE)

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
  # The output 'values$expenses_sorted' includes final order column
  # and will be used as input for the allocation algorithm
  # ----------------------------


  # ----------------------------
  # New logic for column sorting 0120
  # ----------------------------
  # 1.Dynamically generate the sorting rules list
  current_ordering_rules <- reactive({
    list(
      p1_item        = input$select_first_priority_item,
      p1_date_dir    = input$`payment-date-options`, 
      p2_item        = input$select_second_priority_item,
      p2_date_dir    = input$`payment-date-options`, 
      category_order = input$drag_categories
    )
  })

  

  observe({
    # Debug print
    cat("\n[Server Signal] Observer Triggered! Mode:", input$select_priority, "\n")
    
    # Get current rules
    rules <- current_ordering_rules()
    
    # Retrieve data
    data_to_sort <- if(!is.null(values$expenses) && nrow(values$expenses) > 0) {
      values$expenses
    } else {
      # mock data
      data.frame(
        priority = 1:5,
        expense_id = c("EXP001", "EXP002", "EXP003", "EXP004", "EXP005"),
        expense_name = c("Staff A", "Trip", "Staff B", "Laptop", "Snacks"),
        expense_category = c("Salary", "Travel", "Salary", "Equipment", "Cheese"),
        planned_amount = c(5000, 200, 4500, 1200, 50),
        latest_payment_date = as.Date(c("2024-03-01", "2024-01-15", "2024-02-10", "2024-01-15", "2024-03-15")),
        notes = c("", "Conf. in Sydney", "Monthly", "Laptop", "Kitchen"),
        stringsAsFactors = FALSE
      )
    }
    
    # Decide the mode based on the user’s selection
    target_mode <- if(isTruthy(input$select_priority) && input$select_priority == "Column Priority") "by_rules" else "manual"
    
    # Execute sorting
    sorted_result <- main_sorting_expenses(
      expenses_data = data_to_sort,
      mode = target_mode,
      ordering_rules = rules
    )
    
    # Write back values
    if (!is.null(values$expenses) && nrow(values$expenses) > 0) {
      values$expenses <- sorted_result
    }
    
  }) %>% bindEvent(
    input$select_priority,
    input$select_first_priority_item,
    input$select_second_priority_item,
    input$`payment-date-options`, 
    input$drag_categories,
    # Ensure it runs even if some inputs are empty
    ignoreInit = FALSE,
    ignoreNULL = FALSE
  )

  observe({
    print(str(input$spreadsheet_upload))
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
  # Deleting selected expense
  observeEvent(input$delete_expense, {
    selected <- input$sample_expense_table_rows_selected
    values$expenses <- delete_row(values$expenses, selected)
  })

  # --- OUTPUT: Data Tables ---
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

  display_expense_names <- c(
    priority = "Priority",
    expense_id = "Expense ID",
    expense_name = "Expense Name",
    expense_category = "Expense Category",
    planned_amount = "Planned Amount",
    latest_payment_date = "Latest Payment Date",
    notes = "Notes"
  )

  output$sample_funding_table <- renderDT({
    req(values$funding_sources)
    df <- values$funding_sources
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
  }, server = FALSE)
  

  output$sample_expense_table <- renderDT({
    req(values$expenses)
    df <- values$expenses
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
  }, server = FALSE)

  output$sample_manual_table <- renderDT({
    if(!is.null(pending_order())) {
      df <- pending_order()
    }
    else {
      df <- values$expenses
    }

    colnames(df) <- display_expense_names[names(df)]
    datatable(
      df,
      extensions = 'RowReorder',
      selection = 'none',
      callback = JS(row_reorder_callback),
      options = list(
        rowReorder = TRUE,
        pageLength = 100,
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
  })

  # --- OUTPUT: Dashboard Graphs ---
  output$shortfall_plot <- renderPlotly({
    shortfall_data <- create_shortfall_bar()
    shortfall_data$shortfall_plot
  })
  
  output$shortfall_number <- renderUI({
    shortfall_data <- create_shortfall_bar()
    shortfall_data$total_shortfalls
  })
  
  
  observeEvent(event_data("plotly_click"), {
    clicked_bar <- event_data("plotly_click")
    req(clicked_bar)
    clicked_month(clicked_bar$x)
  })
  
  output$circos_container <- renderUI({
    cm <- clicked_month()
    
    if (is.null(cm)) {
      tags$p("click on a month to see circos plot")
    } else {
      plotOutput("circos_plot", height = "400px")
    }
  })
}





