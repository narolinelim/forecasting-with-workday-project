source("src/server/allocation-algorithm.R")
source("src/server/data-processing.R")
source("src/server/output.R")
source("src/server/sorting.R")
source("src/server/graph.R")
source("src/server/edit-rows.R")

main_server_logic <- function(input, output, session, values) {
  # ---- 1. GENERAL LOGIC ----

  ## ---- Current page ----
  current_view <- reactiveVal("funding")

  clicked_month <- reactiveVal(NULL)

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
      showNotification(error, type = "error", duration = NULL)
    }
    errors(NULL)
  }) %>%
    bindEvent(errors(), ignoreInit = TRUE)

  ## ---- EVENT: Navigation between tabs ----
  observeEvent(input$dashboard_tab, current_view("dashboard"))
  observeEvent(input$forecast_tab, current_view("forecast"))
  observeEvent(input$funding_tab, current_view("funding"))
  observeEvent(input$expense_tab, current_view("expense"))

  ## ---- OUTPUT: Switching between tabs ----
  output$tab_content <- renderUI({
    switch(
      current_view(),
      "dashboard" = dashboard_ui(
        total_balance = sum(values$funding_sources$amount)
      ),
      "forecast" = forecast_ui(),
      "funding" = funding_ui(),
      "expense" = expense_ui()
    )
  })

  # ---- 2. DASHBOARD PAGE SERVER LOGIC ----

  ## ---- OUTPUT: Dashboard Graphs ----
  all_shortfall <- reactive(
    nrow(values$allocation_result) > 0 &&
      nrow(values$funding_summary) > 0 &&
      nrow(values$expense_status) > 0
  )

  shortfall_data <- reactive({
    req(all_shortfall)
    create_shortfall_bar(values)
  })

  output$shortfall_plot <- renderUI({
    if (!all_shortfall()) {
      tags$p(
        "No data available.",
        style = "font-size: 16px; text-align: center;"
      )
    } else {
      plotlyOutput("shortfall_bar_plot", height = "100%")
    }
  })

  output$shortfall_bar_plot <- renderPlotly({
    shortfall_data()$shortfall_plot
  })

  output$shortfall_number <- renderUI({
    if (!all_shortfall()) {
      tags$p("No data available", style = "font-size: 20px; color: red;")
    } else {
      shortfall_data()$total_shortfalls
    }
  })

  output$total_balance <- renderUI({
    if (!all_shortfall()) {
      tags$p("No data available", style = "font-size: 20px; color: red;")
    } else {
      shortfall_data()$total_balance
    }
  })

  observeEvent(event_data("plotly_click"), {
    clicked_bar <- event_data("plotly_click")
    req(clicked_bar)
    clicked_month(clicked_bar$x)
  })

  circos_month <- reactive({
    cm <- clicked_month()
    req(cm)
    cutoff <- ceiling_date(as.Date(paste0(cm, "-01")), "month")
    create_circos_plot(values, month = cutoff)
  })

  output$circos_plot <- renderChorddiag({
    circos_month()
  })

  output$circos_container <- renderUI({
    cm <- clicked_month()

    if (is.null(cm) && all_shortfall()) {
      tags$p(
        "Click on a month to see the allocation plot.",
        style = "font-size: 16px; text-align: center;"
      )
    } else if (!all_shortfall()) {
      tags$p(
        "No data available.",
        style = "font-size: 16px; text-align: center;"
      )
    } else {
      tagList(
        tags$p(
          paste("Allocation Month: ", format(as.Date(cm), "%b %Y")),
          style = "font-size: 16px; font-weight: 600;"
        ),
        chorddiagOutput("circos_plot", height = "600px", width = "100%")
      )
    }
  })

  ## ---- OUTPUT: Dashboard Result Tables ----
  display_budget_allocation_names <- c(
    source_id = "Source ID",
    expense_id = "Expense ID",
    expense_category = "Expense Category",
    allocated_amount = "Allocated Amount",
    planned_amount = "Expense Amount",
    latest_payment_date = "Payment Date",
    status = "Allocation Status"
  )

  display_unallocated_funding_names <- c(
    source_id = "Source ID",
    funding_source = "Funding Source",
    initial_amount = "Initial Amount",
    used_amount = "Used Amount",
    remaining_amount = "Remaining Amount"
  )

  output$budget_allocation_table <- renderDT({
    req(values$full_budget_allocation_df)
    df <- values$full_budget_allocation_df

    colnames(df) <- display_budget_allocation_names[names(df)]

    datatable(df)
  })

  output$unallocated_funding_table <- renderDT({
    funding_df <- values$funding_sources
    funding_summary_df <- values$funding_summary

    df <- funding_summary_df %>%
      left_join(
        funding_df %>%
          select(
            source_id,
            funding_source
          ),
        by = "source_id"
      ) %>%
      relocate(funding_source, .before = initial_amount)

    colnames(df) <- display_unallocated_funding_names[names(df)]

    datatable(df)
  })

  ## ---- EVENT: Exit Session Popup ----
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

  ## ---- EVENT: End Session ----
  observeEvent(input$end_session, {
    showNotification(
      "Session Ended. All data cleared.",
      type = "message",
      duration = 3
    )
    removeModal()
    session$reload()
  })

  ## ---- EVENT: Return to session ----
  observeEvent(input$return_to_session, {
    removeModal()
    current_view("dashboard")
  })

  # ---- 3. FORECAST PAGE SERVER LOGIC ----

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
      div("No second priority.", class = "no-second-priority")
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

  ### ---- Get available categories for dragging ----
  available_categories <- reactive({
    #### ---- From expenses categories ----
    exp_cats <- character(0)
    if (
      !is.null(values$expenses) &&
        is.data.frame(values$expenses) &&
        "expense_category" %in% names(values$expenses)
    ) {
      exp_cats_raw <- values$expenses$expense_category
      if (is.factor(exp_cats_raw)) {
        exp_cats_raw <- as.character(exp_cats_raw)
      }
      exp_cats <- as.character(exp_cats_raw)
      exp_cats <- trimws(exp_cats)
      exp_cats <- exp_cats[!is.na(exp_cats) & nzchar(exp_cats)]
    }

    #### ---- From funding allowed categories (list-column or comma-separated) ----
    fund_cats <- character(0)
    if (
      !is.null(values$funding_sources) &&
        is.data.frame(values$funding_sources) &&
        "allowed_categories" %in% names(values$funding_sources)
    ) {
      ac <- values$funding_sources$allowed_categories

      if (is.list(ac)) {
        fund_cats_raw <- unlist(ac, use.names = FALSE)
      } else {
        # atomic vector: might contain comma-separated strings
        fund_cats_raw <- as.character(ac)
        fund_cats_raw <- unlist(
          strsplit(fund_cats_raw[!is.na(fund_cats_raw)], ",\\s*"),
          use.names = FALSE
        )
      }

      fund_cats <- trimws(as.character(fund_cats_raw))
      fund_cats <- fund_cats[!is.na(fund_cats) & nzchar(fund_cats)]
    }

    ### ---- combine, dedupe, sort ----
    cats <- sort(unique(c(exp_cats, fund_cats)))

    dr <- drag_order()
    if (!is.null(dr)) {
      # Preserve user-defined order
      dr_filtered <- dr[dr %in% cats]
      extras <- setdiff(cats, dr_filtered)
      c(dr_filtered, sort(extras))
    } else {
      cats
    }
  })

  ## ---- EVENT: Manual Row Reordering ----
  ### ---- Temp order and proxy table ----
  pending_order <- reactiveVal(NULL)
  proxy <- dataTableProxy("sample_manual_table")

  ### ----Update temp order when user drags rows ----
  observeEvent(input$newOrder, {
    new_idx <- match(input$newOrder, values$expenses$priority)
    pending_order(values$expenses[new_idx, ])
  })

  ### ---- Save manual order ----
  observeEvent(input$save_manual_order, {
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

        showNotification(
          "Data saved successfully",
          type = "message",
          duration = 3
        )
      },
      error = function(e) {
        showNotification(
          paste("Upload failed:", e$message),
          type = "error",
          duration = 3
        )
      }
    )
  })

  ## ---- OUTPUT: Manual Priority Data Table ----
  output$sample_manual_table <- renderDT({
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
        values$allocation_result <- allocation_data$allocations
        values$funding_summary <- allocation_data$funds
        values$expense_status <- allocation_data$expenses
        values$full_budget_allocation_df <- allocation_data$full_allocation_data

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

  # ---- 4. FUNDING PAGE SERVER LOGIC ----

  ## ---- EVENT: Add Funding Button ----
  # Adding new funding form
  observeEvent(input$add_funding, {
    showModal(upload_funding_modal(categories = available_categories()))
  })

  observeEvent(input$add_funding_confirm, {
    add_funding_button(input, values)
    removeModal()
  })

  ## ---- EVENT: Delete Funding Button ----
  # Deleting selected funding
  observeEvent(input$delete_funding, {
    selected <- input$sample_funding_table_rows_selected
    values$funding_sources <- delete_row(values$funding_sources, selected)
  })

  ## ---- OUTPUT: Data Table ----
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

  output$sample_funding_table <- renderDT(
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

  # ---- 5. EXPENSE PAGE SERVER LOGIC ----

  ## ---- EVENT: Add Expense Button ----
  # Adding new expense form
  observeEvent(input$add_expense, {
    showModal(upload_expense_modal(categories = available_categories()))
  })

  observeEvent(input$add_expense_confirm, {
    add_expense_button(input, values)
    removeModal()
  })

  ## ---- EVENT: Delete Expense Button ----
  # Deleting selected expense
  observeEvent(input$delete_expense, {
    selected <- input$sample_expense_table_rows_selected
    values$expenses <- delete_row(values$expenses, selected)
  })

  ## ---- OUTPUT: Data Table ----
  display_expense_names <- c(
    priority = "Priority",
    expense_id = "Expense ID",
    expense_name = "Expense Name",
    expense_category = "Expense Category",
    planned_amount = "Planned Amount",
    latest_payment_date = "Latest Payment Date",
    notes = "Notes"
  )

  output$sample_expense_table <- renderDT(
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
