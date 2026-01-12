

# Contains all server components from server folder

source("src/server/allocation-algorithm.R")
source("src/server/data-processing.R")
source("src/server/io.R")
source("src/server/sorting.R")
source("src/server/graph.R")


main_server_logic <- function(input, output, session, values) {
  
  
  # Current page
  current_view <- reactiveVal("dashboard")

  # Test data for manual priority table
  table_data <- reactiveVal({
    data.frame(
      Priority = 1:5,
      Project_Name = c(
        "Project Alpha",
        "Project Beta",
        "Project Gamma",
        "Project Delta",
        "Project Epsilon"
      ),
      Category = c(
        "Research",
        "Infrastructure",
        "Education",
        "Research",
        "Infrastructure"
      ),
      Amount = c(50000, 75000, 30000, 45000, 60000),
      Payment_Date = as.Date(c(
        "2026-03-15",
        "2026-04-01",
        "2026-02-20",
        "2026-05-10",
        "2026-03-25"
      )),
      stringsAsFactors = FALSE
    )
  })
  
  observeEvent(input$dashboard_tab, current_view("dashboard"))
  observeEvent(input$forecast_tab, current_view("forecast"))
  observeEvent(input$funding_tab, current_view("funding"))
  observeEvent(input$expense_tab, current_view("expense"))
  
  
  # Switching between tabs
  output$tab_content <- renderUI({
    switch(current_view(),
           "dashboard" = dashboard_ui(),
           "forecast" = forecast_ui(),
           "funding" = funding_ui(),
           "expense" = expense_ui()
    )
  })
  
  
  # Exiting Session pop-up
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
  
  
  # Return to session
  observeEvent(input$return_to_session, {
    removeModal()
    current_view("dashboard")
  })
  

  # --- FEATURES: Manual Priority ---
  # Render priority mode
  output$priority_card <- renderUI({
    
    if (input$select_priority == "Manual Priority") {
      manual_priority_ui()
    } else {
      column_priority_ui()
    }
    
  })

  # JavaScript callback for row reorder
  callback <- c(
    "table.on('row-reorder', function(e, details, edit){",
    "  var oldRows = [], newRows = [];",
    "  for(let i=0; i < details.length; ++i){",
    "    oldRows.push(details[i].oldData);",
    "    newRows.push(details[i].newData);",
    "  }",
    "  Shiny.setInputValue('manual_table_rowreorder', {old: oldRows, new: newRows});",
    "});"
  )

  # Create proxy for table updates
  proxy <- dataTableProxy("sample_manual_table")

  # Observe row reordering events
  observeEvent(input$manual_table_rowreorder, {
    req(input$manual_table_rowreorder)

    # Get old and new positions
    old <- unlist(input$manual_table_rowreorder$old)
    new <- unlist(input$manual_table_rowreorder$new)

    # Get current data
    dat <- table_data()

    # Reorder rows
    dat[new, ] <- dat[old, ]

    # Update Priority column to reflect new order
    dat$Priority <- 1:nrow(dat)

    # Update reactive value
    table_data(dat)

    # Replace data in table without resetting pagination
    replaceData(proxy, dat, resetPaging = FALSE)
  })


  
  # --- FEATURES: Column Priority ---
  # Render first priority
  output$first_priority <- renderUI({
    
    if (input$select_first_priority_item == "Latest Payment Date") {
      latest_payment_date_view()
    } else {
      categories_view()
    }
    
  })
  
  # Render second priority
  output$second_priority <- renderUI({
    
    if (input$select_second_priority_item == "Latest Payment Date") {
      latest_payment_date_view()
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
  
  
  # Adding new funding form
  observeEvent(input$add_funding, {
    showModal(upload_funding_modal())
  })
  

  # Adding new expense form
  observeEvent(input$add_expense, {
    showModal(upload_expense_modal())
  })
  
  
  
  # Sample table outputs (for viewings only)
  output$sample_budget_table <- renderDT({datatable(penguins)})
  
  output$sample_leftover_table <- renderDT({datatable(penguins)})
  
  output$sample_funding_table <- renderDT({datatable(penguins)})
  
  output$sample_expense_table <- renderDT({datatable(penguins)})

  output$sample_manual_table <- renderDT({
    datatable(
      table_data(),
      extensions = 'RowReorder',
      selection = 'none',
      callback = JS(callback),
      options = list(
        rowReorder = TRUE,
        pageLength = 25
      ),
      rownames = FALSE
    )
  })

  output$sample_table <- renderDT({datatable(penguins)})
  
}








