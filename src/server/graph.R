

# ---- SECTION 1: SHORTFALL BAR GRAPH ----
create_shortfall_bar <- function(values) {
  #' Create shortfall bar plot
  #'
  #' @param values: reactiveValues containing funding_sources, and expenses
  #'
  #' @return list of total_balance, shortfall plots, monthly_shortfall and total shortfall
  
  
  
  # Extracting allocation result from the reactive values
  df_allocations <- values$allocation_result
  funding <- values$funding_sources
  df_expenses_status <- values$expense_status
  expenses <- values$expenses
  
  
  ## ---- Step 1: Setting Up Full Allocation Data Frame ----
  allocation_with_funding_df <- df_allocations %>%
    left_join(
      funding %>%
        select(
          source_id,
          valid_from,
          valid_to
        ), by = "source_id"
    )
  
  full_allocation_df <- allocation_with_funding_df %>%
    left_join(
      df_expenses_status %>%
        select(
          expense_id,
          expense_amount = planned_amount,
          expense_date = latest_payment_date
        ), by = "expense_id"
    )
  
  df <- full_allocation_df %>%
    mutate(
      expense_date_month = floor_date(expense_date, "month"),
      valid_from_month = floor_date(valid_from, "month"),
      valid_to_month = floor_date(valid_to, "month"),
      overdue = case_when(
        expense_date >= valid_from & expense_date <= valid_to ~ "In Time",
        TRUE ~ "Overdue"
      )
    ) %>%
    arrange(expense_date)

  
  
  ## ---- Step 2: Create An Empty Monthly Baseline Data Frame ----
  months <- seq(
    from = min(floor_date(c(expenses$latest_payment_date, funding$valid_from), "month")),
    to = max(floor_date(c(funding$valid_to, expenses$latest_payment_date), "month")),
    by = "1 month"
  )
  months_df <- tibble(Month = months)
  
  
  ## ----- Step 3: Extracting All Distinct Expenses ----
  all_expense <- expenses %>%
    reframe(
      expense_id,
      expense_amount = planned_amount,
      expense_date_month = floor_date(latest_payment_date, "month")
    ) 
  
  
  ## ---- Step 4: Cumulative Allocation Data Frame For Each Expense Each Month ----
  funding_by_month <- df %>%
    rowwise() %>%
    mutate(Month = list(months[months >= valid_from_month])) %>%
    unnest(Month) %>%
    group_by(expense_id, Month) %>%
    summarise(
      cumulative_allocated = sum(allocated_amount, na.rm = TRUE),
      .groups = "drop"
    )
  
  
  ## ---- Step 5: Creating The Cartesian Product To Check Shortfall Every Month ---- 
  expense_month_grid <- all_expense %>%
    mutate(expense_date_month = floor_date(expense_date_month, "month")) %>%
    crossing(months_df) %>%
    filter(Month >= expense_date_month)
  
  
  
  ## ---- Step 6: Cumulative Shortfalls For Each Expense Across All Months ----
  expenses_month_status <- expense_month_grid %>%
    left_join(funding_by_month, by = c("expense_id", "Month")) %>%
    mutate(
      cumulative_allocated = replace_na(cumulative_allocated, 0),
      shortfall = cumulative_allocated - expense_amount,
      is_short = shortfall < 0,
      is_overdue = is_short & (Month > expense_date_month)
    )
  
  
  shortfall_num <- expenses_month_status %>%
    filter(is_short == TRUE)
  
  ### ---- 1. Total Number Of Expense Shortfalls ----
  total_shortfalls <- length(unique(shortfall_num$expense_id))
  
  ### ---- 2. Total Funding Balance ----
  total_balance <- sum(funding$amount)
  
  
  ## ---- Step 7: Final Monthly Shortfall Data Frame ----
  monthly_shortfall <- expenses_month_status %>%
    group_by(Month) %>%
    summarise(
      total_shortfall = sum(if_else(is_short, shortfall, 0), na.rm = TRUE),
      number_of_shortfalls = n_distinct(expense_id[is_short]),
      overdue_shortfall = sum(if_else(is_overdue, shortfall, 0), na.rm = TRUE),
      number_overdue = n_distinct(expense_id[is_overdue]),
      .groups = "drop"
    ) %>%
    right_join(months_df, by = "Month") %>%
    mutate(
      total_shortfall = replace_na(total_shortfall, 0),
      number_of_shortfalls = replace_na(number_of_shortfalls, 0L),
      overdue_shortfall = replace_na(overdue_shortfall, 0),
      number_overdue = replace_na(number_overdue, 0L)
    ) %>%
    arrange(Month)
  
  
  ## ---- Step 8: Shortfall Bar Graphs ----
  
  ### ---- Subplot 1: Number Of Expense Shortfalls ----
  shortfall_number_bar <- plot_ly(
    data = monthly_shortfall,
    x = ~Month,
    y = ~number_of_shortfalls,
    type = "bar",
    showlegend = FALSE,
    hovertemplate = paste(
      "Month: %{x|%b %Y}<br>",
      "Number of Shortfalls: %{y}<extra></extra>"
    )
  ) %>%
    layout(
      xaxis = list(showticklabels = FALSE)
    )
  
  ### ---- Subplot 2: Total Expense Shortfall Amount (Negative) ----
  shortfall_amount_bar <- plot_ly(
    data = monthly_shortfall,
    x = ~Month,
    y = ~total_shortfall,
    type = "bar",
    showlegend = FALSE,
    hovertemplate = paste(
      "Month: %{x|%b %Y}<br>",
      "Total Shortfall Amount: %{y}<extra></extra>"
    )
  ) %>%
    layout(
      xaxis = list(tickformat = "%b %Y")
    )
  
  
  ### ---- Combining Subplot 1 & Subplot 2 ----
  p <- subplot(
    shortfall_number_bar,
    shortfall_amount_bar,
    nrows = 2,
    shareX = TRUE,
    heights = c(0.5, 0.5)
  ) %>%
    layout(
      xaxis = list(
        title = "",
        tickformat = "%b %Y",
        tickmode = "linear",
        dtick = "M1",
        showticklabels = TRUE
      ),
      yaxis = list(domain = c(0.5, 1)),
      yaxis2 = list(domain = c(0, 0.5)),
      margin = list(t = 80, b = 60),
      annotations = list(
        list(
          text = "Number of Shortfalls",
          x = 0.5,
          y = 1.1,
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "bottom",
          showarrow = FALSE,
          font = list(size = 15)
        ),
        list(
          text = "Total Shortfall Amount",
          x = 0.5,
          y = -0.12,
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "top",
          showarrow = FALSE,
          font = list(size = 15)
        )
      )
    ) 
  
  p$x$source <- "A"
  p <- event_register(p, "plotly_click")
  
  return (list(
    total_balance = total_balance,
    shortfall_plot = p,
    total_shortfalls = total_shortfalls,
    monthly_shortfall = monthly_shortfall,
    months_df = months_df
  ))
  
}


# ---- SECTION 2: ALLOCATION PLOT (CHORD DIAGRAM) ---- 
create_circos_plot <- function(values, month) {
  #' Create a circos plot showing allocations up to a specified month
  #'
  #' @param values: reactiveValues containing funding_sources, expenses
  #' @param month: Date object specifying the month up to which allocations are considered
  #'
  #' @return: chorddiag object representing the circos plot
  df_allocations <- values$allocation_result
  funding <- values$funding_sources
  df_expenses_status <- values$expense_status
  expenses <- values$expenses

  
  ## ---- Step 1: Stage Full Allocation Data Frame ----
  allocation_with_funding_df <- df_allocations %>%
    left_join(
      funding %>%
        select(
          source_id,
          amount,
          valid_from,
          valid_to
        ), by = "source_id"
    )
  
  
  full_allocation_df <- allocation_with_funding_df %>%
    left_join(
      df_expenses_status %>%
        select(
          expense_id,
          expense_amount = planned_amount,
          expense_date = latest_payment_date
        ), by = "expense_id"
    )
  
  
  ### ---- 1. Controlling for funding when it becomes available ----
  funding_valid_from <- funding %>%
    filter(valid_from < month)
  
  if (nrow(funding_valid_from) > 0) {
    sources_ids <- unique(funding_valid_from$source_id)
  } else {
    sources_ids <- NULL
  }
  
  
  ordered_expenses <- expenses[order(expenses$expense_id),]
  expenses_ids <- unique(ordered_expenses$expense_id)
  sectors <- c(sources_ids, expenses_ids)
  
  
  ### ---- 2. Filtering allocations done before a certain month ----
  rows_until_month <- full_allocation_df %>%
    mutate(
      allocation_date = if_else(
        expense_date >= valid_from & expense_date <= valid_to,
        expense_date,
        valid_from
      )
    ) %>%
    filter(allocation_date < month)


  # Main allocation matrix
  mat <- matrix(0, nrow = length(sectors), ncol = length(sectors))
  rownames(mat) <- sectors
  colnames(mat) <- sectors
  

  ## ---- Step 2: Direct Expense And Funding Allocations ----
  
  if (nrow(rows_until_month) > 0) {
    

    for (i in 1:nrow(rows_until_month)) {
      mat[rows_until_month$source_id[i], rows_until_month$expense_id[i]] <- rows_until_month$allocated_amount[i]
      mat[rows_until_month$expense_id[i], rows_until_month$source_id[i]] <- rows_until_month$allocated_amount[i]
    }
    
    ## ---- Step 3: Allocation Cases For Chord Diagram ----
    
    ### ---- Case 1: Partial leftover expenses at the current time ----
    leftover_expenses <- rows_until_month %>%
      group_by(expense_id) %>%
      summarise(
        expense_amount = first(expense_amount),
        cumulative_allocation = sum(allocated_amount),
        leftover_expense = expense_amount - cumulative_allocation,
        .groups = "drop"
      )
    
    ### ---- Case 2: Expenses not fully allocated ----
    fully_unallocated_expense <- expenses %>%
      anti_join(leftover_expenses, by = "expense_id") %>%
      group_by(expense_id) %>%
      summarise(
        expense_amount = planned_amount,
        cumulative_allocation = 0,
        leftover_expense = planned_amount,
        .groups = "drop"
      )
    
    all_expenses_allocations <- bind_rows(leftover_expenses, fully_unallocated_expense)
    
    ### ---- Remaining Expense Allocations (Case 1 & 2) ----
    for (i in 1:nrow(all_expenses_allocations)) {
      mat[all_expenses_allocations$expense_id[i], all_expenses_allocations$expense_id[i]] <- all_expenses_allocations$leftover_expense[i]
    }
    
  } else {
    
    # Expenses Self-links (Fully Unallocated Expenses)
    for (i in 1:nrow(expenses)) {
      mat[expenses$expense_id[i], expenses$expense_id[i]] <- expenses$planned_amount[i]
    }
    
  }
  
  
  ## ---- Step 3: Funding Allocation Cases For Allocation Plot ----
  
  if (nrow(rows_until_month) > 0) {
    
    ### ---- Case 1: Funding not fully allocated ----
    unallocated_funding <- funding %>%
      anti_join(rows_until_month, by = "source_id") %>%
      reframe(
        source_id,
        remaining_amount = amount
      )
    
    
    ### ---- Case 2: Partial leftover funding at the current time ----
    leftover_funding <- rows_until_month %>%
      group_by(source_id) %>%
      reframe(
        funding_amount = first(amount),
        cumulative_allocation = sum(allocated_amount),
        remaining_amount = amount - cumulative_allocation,
        .groups = "drop"
      )
    
    all_funding_allocations <- bind_rows(unallocated_funding, leftover_funding)
    all_funding_allocations <- all_funding_allocations %>%
      reframe(
        source_id,
        remaining_amount
      )
    
    ### ---- Remaining Funding Allocations (Case 1 & 2) ----
    if (nrow(all_funding_allocations) > 0) {
      for (i in 1:nrow(all_funding_allocations)) {
        if (all_funding_allocations$source_id[i] %in% colnames(mat)) {
          mat[all_funding_allocations$source_id[i], all_funding_allocations$source_id[i]] <- all_funding_allocations$remaining_amount[i]
        }
      }
    }
    
  } else if (nrow(funding_valid_from) > 0) {
    
      for (i in 1:nrow(funding_valid_from)) {
        mat[funding_valid_from$source_id[i], funding_valid_from$source_id[i]] <- funding_valid_from$amount[i]
      }
    
  }
  
  
  
  ## ---- Step 4: Current Month Allocation Highlights ----
  
  ### ---- 1. Current Month Allocation Data Frame ----
  month_before_current <- month - months(1)
  
  rows_before_current_month <- rows_until_month %>%
    filter(allocation_date < month_before_current)

  current_month_allocation <- setdiff(rows_until_month, rows_before_current_month)

  
  ### ---- 2. Current Month Allocation Matrix ----
  current_month_mat <- matrix(0, nrow = length(sectors), ncol = length(sectors))
  rownames(current_month_mat) <- sectors
  colnames(current_month_mat) <- sectors
  
  if (nrow(current_month_allocation) > 0) {
    for (i in 1:nrow(current_month_allocation)) {
      current_month_mat[current_month_allocation$source_id[i], current_month_allocation$expense_id[i]] <- current_month_allocation$allocated_amount[i]
      current_month_mat[current_month_allocation$expense_id[i], current_month_allocation$source_id[i]] <- current_month_allocation$allocated_amount[i]
    }
  }
  
  ### ---- 3. Extracting Current Month Allocations Into Vectors ----
  data_id <- rownames(current_month_mat)
  n <- length(data_id)
  current_allocations <- c()
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (current_month_mat[i, j] > 0) {
        allocated <- paste0(data_id[i], "-", data_id[j])
        current_allocations <- c(current_allocations, allocated)
      }
    }
  }
  
  funding_length <- length(sources_ids)
  expense_length <- length(expenses_ids)
  
  ### ---- 4. Setting Two-tone Colours For Chord Diagram ----
  funding_colors <- rep("green", funding_length)
  expense_colors <- rep("red", expense_length)
  all_colors <- c(funding_colors, expense_colors)

  
  ### ---- 5. Chord Diagram For Allocation Up Until Current Month ----
  circos <- chorddiag(
    data = mat,
    chordedgeColor = "green",
    groupColors = all_colors,
    showTicks = FALSE,
    margin = 80,
    height = 800,
    groupPadding = 5,
    groupThickness = 0.1,
    tooltipNames = sectors,
    tooltipUnit = "$",
    tooltipGroupConnector = " → "
  )
  
  
  ### ---- 6. Activating Highlights For Current Month Allocations and Shadows For Other Allocations ----
  current_allocation_json <- toJSON(current_allocations)
  circos <- onRender(circos, sprintf("
      function(el, x) {
        console.log('running');
        setTimeout(function() {
        
          var currentAllocation = %s;
          
          /* Chord Colors */
          d3.selectAll('.chords path').each(function() {
            var path = d3.select(this);
            var pathId = path.attr('id');
            
            if (pathId && pathId.startsWith('chord-')) {
              var allocatedPart = pathId.replace('chord-', '');
          
              
              var isCurrentMonth = currentAllocation.indexOf(allocatedPart) !== -1;
              
              if (isCurrentMonth) {
                path.style('fill', 'rgba(76, 187, 23, 1)').style('fill-opacity', 1);
              } else {
                path.style('fill', 'rgba(107, 107, 107, 1)').style('fill-opacity', 0.2);
              }
            }
          });
          
          
          /* Group Arc Colors */
          d3.selectAll('.groups path').each(function() {
            var path = d3.select(this);
            var archPathId = path.attr('id');
            
            if (archPathId && archPathId.startsWith('group-')) {
              var allocatedPart = archPathId.replace('group-', '');
              
              var isCurrentMonth = currentAllocation.some(function(pair) {
                return pair.startsWith(allocatedPart + '-') || pair.endsWith('-' + allocatedPart)
              })
              
              
              if (isCurrentMonth) {
                path.style('fill', 'rgba(0, 78, 56, 1)')
                  .style('fill-opacity', 1)
                  .style('stroke', 'rgba(0, 78, 56, 1)');
              } else {
                path.style('fill', 'rgba(211, 211, 211, 1)').style('fill-opacity', 0.2);
              }
            }
          });
        
        }, 10)
        
      }
                     
    ", current_allocation_json))
  
  
  return (circos)
}










