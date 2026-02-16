source("src/server/components/allocation-algorithm.R")
source("src/server/components/data-processing.R")
source("src/server/components/output.R")
source("src/server/components/sorting.R")
source("src/server/components/graph.R")

forecast_server <- function(input, output, session, values, current_view, available_categories) {
  # ---- 2. FORECAST PAGE SERVER LOGIC ----

  ## ---- OUTPUT: Render priority mode ----
  output$priority_card <- renderUI({
    if (
      isTruthy(input$select_priority) &&
        input$select_priority == "Manual Priority"
    ) {
      manual_priority_ui()
    } else if (
      isTruthy(input$select_priority) &&
        input$select_priority == "Column Priority"
    ) {
      column_priority_ui()
    } else {
      none_priority_ui()
    }
  })

  ## ---- OUTPUT: Render first priority ----
  output$first_priority <- renderUI({
    if (input$select_first_priority_item == "Payment Date") {
      payment_date_view()
    } else {
      categories_view(categories = available_categories())
    }
  })

  ## ---- OUTPUT: Render second priority ----
  output$second_priority <- renderUI({
    if (input$select_second_priority_item == "Payment Date") {
      payment_date_view()
    } else if (input$select_second_priority_item == "Categories") {
      categories_view(categories = available_categories())
    } else if (input$select_second_priority_item == "None") {
      div(
        "No second priority.",
        style = "text-align: center; margin: 30px 0 20px 0;"
      )
    }
  })

  
## ---- EVENT: Mutual Exclusion for Priority Dropdowns ----
  
  observeEvent(input$select_first_priority_item, {
    ### ---- 1. Get the current value of the 1st Priority ----
    p1_val <- input$select_first_priority_item

    ### ---- 2. Define all available choices for the 2nd Priority ----
    p2_choices <- c("Payment Date", "Categories", "None")

    ### ---- 3. Identify the item to disable (the one already selected in P1) ----
    disabled_choices <- p2_choices[p2_choices == p1_val]

    ### ---- 4. Update the 2nd Priority pickerInput state ----
    updatePickerInput(
      session = session,
      inputId = "select_second_priority_item",
      choices = p2_choices,
      choicesOpt = list(
        # Disable the item selected in P1
        disabled = p2_choices %in% disabled_choices,
        # Make the disabled option appear gray
        style = ifelse(
          p2_choices %in% disabled_choices,
          "color: rgba(0,0,0,0.3); background: #f8f9fa;",
          ""
        )
      )
    )

    ### ---- 5. Safety Check: If P2 was already set to the newly disabled item, reset it to "None" ----
    if (input$select_second_priority_item == p1_val) {
      updatePickerInput(
        session,
        "select_second_priority_item",
        selected = "None"
      )
    }
  })

  ## ---- EVENT: Dragging feature for categories priority ----
  ### ---- Store the order of categories after dragging ----
  drag_order <- reactiveVal(NULL)
  observeEvent(input$drag_categories, {
    drag_order(input$drag_categories)
  })
  
  observe({
    #### ---- From expenses categories ----
    req(values$expenses)
    req(values$funding_sources)
    
    available_categories(NULL) # Reset available categories before recalculating
    
    exp_cats <- unique(values$expenses$expense_category)
    fund_cats <- unique(unlist(values$funding_sources$allowed_categories))
    
    cats <- sort(unique(c(exp_cats, fund_cats)))
    cats <- cats[!is.na(cats) & cats != ""] # Remove NA and empty categories
    
    dr <- drag_order()
    if (!is.null(dr)) {
      # Preserve user-defined order
      dr_filtered <- dr[dr %in% cats]
      extras <- setdiff(cats, dr_filtered)
      available_categories(c(dr_filtered, sort(extras)))
    } else {
      available_categories(cats)
    }
  }) %>%
    bindEvent(
      values$expenses,
      values$funding_sources
    )

  ## ---- EVENT: Manual Row Reordering ----

  ### ---- Temp order and proxy table ----
  pending_order <- reactiveVal(NULL)
  proxy <- dataTableProxy("manual_table")

  ### ---- Update temp order when user drags rows ----
  observeEvent(input$newOrder, {
    new_idx <- match(input$newOrder, values$expenses$priority)
    pending_order(values$expenses[new_idx, ])
  })

  ### ---- Data upload state for showing optional buttons ----
  output$data_uploaded <- reactive({
    !is.null(values$expenses) && nrow(values$expenses) > 0
  })
  outputOptions(output, "data_uploaded", suspendWhenHidden = FALSE)

  ### ---- Save manual order ----
  observeEvent(input$save_manual_order, {
    req(pending_order())
    values$expenses <- row_reorder(
      input$newOrder,
      values$expenses,
      proxy,
      id_col = "priority"
    )

    pending_order(NULL)
    showNotification("Manual order saved", type = "message", duration = 3)
  })

  ### ---- Cancel manual order ----
  observeEvent(input$cancel_manual_order, {
    if (is.null(pending_order())) {
      showNotification(
        "No manual order to cancel",
        type = "warning",
        duration = 3
      )
      return()
    }
    pending_order(NULL)
    showNotification("Manual order cancelled", type = "message", duration = 3)
  })

  ### ---- When leaving manual priority view, clear pending order ----
  observeEvent(
    input$select_priority,
    {
      if (
        !is.null(pending_order()) && input$select_priority != "Manual Priority"
      ) {
        pending_order(NULL)
        showNotification(
          "Unsaved manual order discarded",
          type = "message",
          duration = 3
        )
      }
    },
    ignoreNULL = FALSE
  )

  ## ---- EVENT: Logic for column sorting ----
  ### ---- Dynamically generate the sorting rules list ----
  current_ordering_rules <- reactive({
    # Ensure UI is initialized
    req(input$select_first_priority_item)

    category_order <- if (!is.null(drag_order())) {
      drag_order()
    } else {
      available_categories()
    }

    list(
      p1_item = input$select_first_priority_item,
      p1_date_dir = input$`payment-date-options`,
      p2_item = input$select_second_priority_item,
      p2_date_dir = input$`payment-date-options`,
      category_order = category_order
    )
  })

  ### ---- Apply sorting when relevant inputs change ----
  observe({
    # Do not proceed if no data is loaded
    req(values$expenses)
    req(nrow(values$expenses) > 0)
    # Get current rules
    rules <- current_ordering_rules()

    # Retrieve data
    data_to_sort <- values$expenses

    # Execute sorting
    sorted_result <- col_ordering(
      expenses_data = data_to_sort,
      ordering_rules = rules
    )

    # Write back values
    if (!is.null(values$expenses) && nrow(values$expenses) > 0) {
      values$expenses <- sorted_result
    }
  }) %>%
    bindEvent(
      input$select_priority,
      input$select_first_priority_item,
      input$select_second_priority_item,
      input$`payment-date-options`,
      input$drag_categories,
      # Ensure it runs even if some inputs are empty
      ignoreInit = FALSE,
      ignoreNULL = FALSE
    )

  ## ---- OUTPUT: Manual Priority Data Table ----
  display_expense_names <- c(
    priority = "Priority",
    expense_id = "Expense ID",
    expense_name = "Expense Name",
    expense_category = "Expense Category",
    planned_amount = "Planned Amount",
    latest_payment_date = "Latest Payment Date",
    notes = "Notes"
  )

  output$manual_table <- renderDT({
    if (!is.null(pending_order())) {
      df <- pending_order()
    } else {
      df <- values$expenses
    }

    # Format date for display
    df <- df %>%
      mutate(
        latest_payment_date = format(as.Date(latest_payment_date), "%d-%m-%Y")
      )
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

  ## ---- EVENT: Activating Forecasting Button ----
  observeEvent(input$generate_forecast, {

    tryCatch(
      {
        req(values$funding_sources)
        req(values$expenses)
        allocation_data <- activate_allocation_algorithm(
          values$funding_sources,
          values$expenses
        )

        req(allocation_data)
        values$allocation_result <- allocation_data$allocations
        values$funding_summary <- allocation_data$funds
        values$expense_status <- allocation_data$expenses
        values$full_budget_allocation_df <- allocation_data$full_allocation_data
        current_view("dashboard")

        showNotification(
          "Allocation Finished. Go to Dashboard for result.",
          type = "message",
          duration = 3
        )
      },
      error = function(e) {
        showNotification(
          paste("Allocation Failed:", e$message),
          type = "error",
          duration = 3
        )
      }
    )
  })
}