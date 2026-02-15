
source("src/server/components/graph.R")
source("src/server/components/output.R")

dashboard_server <- function(input, output, session, values, current_view) {
  # ---- 5. DASHBOARD PAGE SERVER LOGIC ----

  ## ---- OUTPUT: Dashboard Information and Graphical Section ----

  ### ---- Data validation for dashboard output ----
  all_input_data <- reactive(
    nrow(values$funding_sources) > 0 &&
      nrow(values$expenses) > 0
  )

  ### ---- Data validation for dashboard output ----
  all_shortfall <- reactive(
    nrow(values$allocation_result) > 0 ||
      nrow(values$funding_summary) > 0 ||
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
    if (!all_input_data() || !all_shortfall()) {
      tags$p(
        "No data available.",
        style = "font-size: 16px; text-align: center;"
      )
    } else if (shortfall_data()$total_shortfalls == 0) {
      tags$p(
        "No shortfall for this dataset.",
        style = "font-size: 16px; text-align: center;"
      )
    } else {
      plotlyOutput("shortfall_bar_plot", height = "100%")
    }
  })

  output$shortfall_bar_plot <- renderPlotly({
    shortfall_data()$shortfall_plot
  })

  #### ---- 2. Total Number of Shortfalls ----
  output$shortfall_number <- renderUI({
    if (!all_input_data() || !all_shortfall()) {
      tags$p("No data available", style = "font-size: 20px; color: red;")
    } else {
      shortfall_data()$total_shortfalls
    }
  })

  #### ---- 3. Total Funding Balance ----
  output$total_balance <- renderUI({
    if (!all_input_data() || !all_shortfall()) {
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
      default_month <- max(floor_date(
        c(expense_df$latest_payment_date, funding_df$valid_to),
        "month"
      ))
      cm <- clicked_month(default_month)
    }

    if (!all_input_data() || !all_shortfall()) {
      return(tags$p(
        "No data available.",
        style = "font-size: 16px; text-align: center; padding: 16px;"
      ))
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

      tags$p(
        paste("Allocation Month: ", format(cm, "%b %Y")),
        style = "font-size: 16px; font-weight: 600; padding: 15px 15px 5px 15px;"
      ),

      output[[circos_plot_id]] <- renderChorddiag({
        cm <- clicked_month()
        req(cm)

        cutoff <- ceiling_date(as.Date(paste0(cm, "-01")), "month")
        circos <- create_circos_plot(values, expense_month_status = shortfall_data()$expense_month_status, month = cutoff)

        # Activating zooming feature for circos plot
        onRender(
          circos,
          "
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
        "
        )
      })
    )
  })

  #### ---- Observe change in the month clicked ----
  observe({
    if (!all_input_data()) {
      return()
    }

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
