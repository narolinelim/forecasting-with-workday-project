
source("src/server/allocation-algorithm.R")
source("src/server/data-processing.R")
source("src/server/output.R")
source("src/server/sorting.R")
source("src/server/graph.R")
source("src/server/edit-rows.R")



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
      div("No second priority.", style = "text-align: center; margin: 30px 0 20px 0;")
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
  available_categories <- reactiveVal(NULL)
    
  observe({
    #### ---- From expenses categories ----
    req(values$expenses)
    req(values$funding_sources)
    
    available_categories(NULL)  # Reset available categories before recalculating
 
    exp_cats <- unique(values$expenses$expense_category)
    fund_cats <- unique(unlist(values$funding_sources$allowed_categories))

    cats <- sort(unique(c(exp_cats, fund_cats)))
    cats <- cats[!is.na(cats) & cats != ""]  # Remove NA and empty categories

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
          duration = 3,
          ignoreInit = FALSE
        )
      }
    )
  })

  ## ---- OUTPUT: Manual Priority Data Table ----
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
          paste("Allocation Failed: No data input."),
          type = "error",
          duration = 3
        )
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
  
  
  # ---- 5. DASHBOARD PAGE SERVER LOGIC ----
  
  ## ---- OUTPUT: Dashboard Information and Graphical Section ----
  
  ### ---- Data validation for dashboard output ----
  all_input_data <- reactive(
    nrow(values$funding_sources) > 0 &&
      nrow(values$expenses) > 0
  )
  
  ### ---- Data validation for dashboard output ----
  all_shortfall <- reactive(
    nrow(values$allocation_result) > 0 &&
      nrow(values$funding_summary) > 0 &&
      nrow(values$expense_status) > 0
  )
  
  ### ---- SECTION 1: SHORTFALL ----
  
  #### ---- Activating and creating shortfall data ----
  shortfall_data <- reactive({
    req(all_input_data)
    c <- create_shortfall_bar(values)
  })

  #### ---- 1. Shortfall Bar Graph ----
  output$shortfall_plot <- renderUI({
    if (!all_input_data()) {
      tags$p("No data available.", style = "font-size: 16px; text-align: center;")
    } else if (shortfall_data()$total_shortfalls == 0) {
      tags$p("No shortfall for this dataset.", style = "font-size: 16px; text-align: center;")
    } else {
      plotlyOutput("shortfall_bar_plot", height = "100%")
    }
  })
  
  output$shortfall_bar_plot <- renderPlotly({
    shortfall_data()$shortfall_plot
  })
  
  #### ---- 2. Total Number of Shortfalls ----
  output$shortfall_number <- renderUI({
    if (!all_input_data()) {
      tags$p("No data available", style = "font-size: 20px; color: red;")
    } else {
      shortfall_data()$total_shortfalls
    }
  })
  
  #### ---- 3. Total Funding Balance ----
  output$total_balance <- renderUI({
    if (!all_input_data()) {
      tags$p("No data available", style = "font-size: 20px; color: red;")
    } else {
      shortfall_data()$total_balance
    }
  })
  
  ### ---- SECTION 2: ALLOCATION ----
  
  #### ---- Keep tracks of which bar in the bar graph is clicked ----
  clicked_month <- reactiveVal(NULL)
  
  #### ---- EVENT: Change Clicked Month State ----
  observeEvent(event_data("plotly_click"), {
    clicked_bar <- event_data("plotly_click")
    req(clicked_bar)
    clicked_month(clicked_bar$x)
  })
  

  
  
  #### ---- Allocation Chord Diagram ----
  output$circos_container <- renderUI({
    cm <- clicked_month()

    
    if (is.null(cm) && all_input_data()) {
      
      # Default circos plot using the last month of the allocation period
      expense_df <- values$expenses
      funding_df <- values$funding_sources
      default_month <- max(floor_date(c(expense_df$latest_payment_date, funding_df$valid_to), "month"))
      cm <- clicked_month(default_month)
      
    } 

    if (!all_input_data()) {

      return (tags$p("No data available.", style = "font-size: 16px; text-align: center; padding: 16px;"))

    }
    
    expenses <- values$expenses
    funding <- values$funding_sources
    
    months_df <- shortfall_data()$months_df
    
    months_df <- months_df %>%
      mutate(
        year_date = year(Month),
        year_chr = format(Month, "%Y"),
        month = format(Month, "%B"),
        id = format(Month, "%Y-%m-%d")
      )
    
    
    # Extract distinct years for dynamic accordions
    distinct_years <- months_df %>%
      distinct(year_date, year_chr)
    
    
    cm_date <- as.Date(cm)
    
    circos_plot_id <- paste0("circos_", gsub("-", "_", as.character(cm)))
    
    # Sidebar for allocation navigation by month
    layout_sidebar(
      sidebar = sidebar(
        width = 250,
        open = "always",
        "Allocation By Month",
        style = "height: 800px; overflow-y: auto; font-size: 15px; font-weight: 800; text-align: center;",
        
        accordion(
          open = distinct_years$year_chr,
          
          lapply(seq_len(nrow(distinct_years)), function(each_year) {
            
            each_year_chr <- distinct_years$year_chr[each_year]
            each_year_date <- distinct_years$year_date[each_year]
            
            months_per_year <- months_df %>%
              filter(year_date == each_year_date)
            
            
            # One accordion panel for each distinct year
            accordion_panel(
              title = each_year_chr,
              value = each_year_chr,
              
              lapply(seq_len(nrow(months_per_year)), function(i) {
                
                month_id <- months_per_year$id[i]
                
                # All months within the year involved in allocation
                actionButton(
                  inputId = paste0(month_id),
                  label = months_per_year$month[i],
                  class = "circos-action-buttons"
                )
              })
            )
          })
          
        )
      ),
      

      tags$p(paste("Allocation Month: ", format(cm_date, "%b %Y")),
             style = "font-size: 16px; font-weight: 600; padding: 15px 15px 5px 15px;"),
      
      output[[circos_plot_id]] <- renderChorddiag({
        cm <- clicked_month()
        req(cm)
        
        cutoff <- ceiling_date(as.Date(paste0(cm, "-01")), "month")
        circos <- create_circos_plot(values, month = cutoff)
        
        
        # Activating zooming feature for circos plot
        onRender(circos, "
          function(el, x) {
            var svg = d3.select(el).select('svg');
            var g = svg.select('g');
            
            if (d3.zoom) {
              var zoom = d3.zoom()
                .on('zoom', function() {
                  g.attr('transform', d3.event.transform);
                });
              
              svg.call(zoom);
            } else if (d3.behavior && d3.behavior.zoom) {
              var zoom = d3.behavior.zoom()
                .on('zoom', function() {
                  g.attr('transform', 'translate(' + d3.event.translate + ')scale(' + d3.event.scale + ')');
                });
    
              svg.call(zoom);
            } 
        
          }
        ")
        
        
      })

    )
    
  })
  
  #### ---- Observe change in the month clicked ----
  observe({
    if (!all_input_data()) return()
    
    # validate shortfall data
    req(shortfall_data())
    req(shortfall_data()$months_df)
    
    months_df <- shortfall_data()$months_df
    months_chr_df <- months_df %>%
      mutate(
        month_chr = format(Month, "%Y-%m-%d")
      )
    
    # Create observeEvent for every single month 
    lapply(seq_len(nrow(months_chr_df)), function(each_month) {
      month_id <- months_chr_df$month_chr[each_month]
      month_date <- months_chr_df$Month[each_month]
      
      observeEvent(input[[month_id]], {
        clicked_month(month_date)
        
      })
    })
  })
  
  
  ## ---- OUTPUT: Dashboard Result Tables ----
  
  ### ---- 1. Budget Allocation Section ----
  
  #### ---- Display budget allocation data table headers ----
  display_budget_allocation_names <- c(
    source_id = "Source ID",
    expense_id = "Expense ID",
    expense_category = "Expense Category",
    allocated_amount = "Allocated Amount",
    planned_amount = "Expense Amount",
    latest_payment_date = "Latest Payment Date",
    status = "Allocation Status"
  )
  
  #### ---- Render budget allocation data table ----
  output$budget_allocation_table <- renderDT({
    req(values$full_budget_allocation_df)
    df <- values$full_budget_allocation_df %>%
      select(
        source_id,
        expense_id,
        expense_category,
        allocated_amount,
        planned_amount,
        latest_payment_date,
        status
      )
    
    colnames(df) <- display_budget_allocation_names[names(df)]
    
    datatable(df)
  })
  
  ### ---- 2. Unallocated Expense Section ----
  display_unallocated_expense_names <- c(
    expense_id = "Expense ID",
    expense_name = "Expense Name",
    expense_category = "Expense Category",
    planned_amount = "Expense Amount",
    latest_payment_date = "Latest Payment Date",
    notes = "Notes",
    status = "Allocation Status"
  )
  
  #### ---- Display unallocated expense data table headers ----
  
  #### ---- Render unallocated expense data table ----
  output$unallocated_expense_table <- renderDT({
    expense_status_df <- values$expense_status

    df <- expense_status_df %>%
      filter(status == "Unfunded") %>%
      select(
        expense_id,
        expense_name,
        expense_category,
        planned_amount,
        latest_payment_date,
        notes,
        status
      )

    colnames(df) <- display_unallocated_expense_names[names(df)]
    
    datatable(df)
  })
  
  
  ### ---- 3. Unallocated Funding Section ----
  
  #### ---- Display unallocated funding data table headers ----
  display_unallocated_funding_names <- c(
    source_id = "Source ID",
    funding_source = "Funding Source",
    initial_amount = "Initial Amount",
    used_amount = "Used Amount",
    remaining_amount = "Remaining Amount"
  )
  
  #### ---- Render unallocated funding data table ----
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
  
}



