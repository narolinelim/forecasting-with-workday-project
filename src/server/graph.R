create_shortfall_bar <- function(values) {
  #' Create shortfall bar plot
  #'
  #' @param values: reactiveValues containing funding_sources, and expenses
  #'
  #' @return list of total_balance, shortfall plots, and total shortfall

  df_allocations <- values$allocation_result
  funding <- values$funding_sources
  df_expenses_status <- values$expense_status
  expenses <- values$expenses

  allocation_with_funding_df <- df_allocations %>%
    left_join(
      funding %>%
        select(
          source_id,
          valid_from,
          valid_to
        ),
      by = "source_id"
    )

  full_allocation_df <- allocation_with_funding_df %>%
    left_join(
      df_expenses_status %>%
        select(
          expense_id,
          expense_amount = planned_amount,
          expense_date = latest_payment_date
        ),
      by = "expense_id"
    )

  # Prepping dataframe by setting all monthly baseline
  df <- full_allocation_df %>%
    mutate(
      expense_date_month = floor_date(expense_date, "month"),
      valid_from_month = floor_date(valid_from, "month"),
      valid_to_month = floor_date(valid_to, "month"),
      overdue = case_when(
        expense_date >= valid_from & expense_date <= valid_to ~ "In Time",
        TRUE ~ "Overdue"
      )
    )

  ordered_df <- df %>% arrange(expense_date)

  # Dataframe including range of months involved in the allocation and prepping
  # for final shortfall dataframe
  months <- seq(
    from = min(floor_date(expenses$latest_payment_date, "month")),
    to = max(df$valid_to_month),
    by = "1 month"
  )
  months_df <- tibble(Month = months)

  # ---------------------- EXTRACTING DISTINCT EXPENSES ------------------------

  # 1. Extracting unallocated distinct expenses
  unallocated_distinct_expense <- expenses %>%
    anti_join(df, by = "expense_id") %>%
    mutate(
      expense_id = expense_id,
      expense_amount = planned_amount,
      expense_date_month = floor_date(latest_payment_date, "month")
    ) %>%
    select(
      expense_id,
      expense_amount,
      expense_date_month
    )

  # 2. Extracting allocated distinct expenses (and partial allocation)
  distinct_expenses <- df %>%
    distinct(expense_id, expense_amount, expense_date_month)

  all_distinct_expenses <- bind_rows(
    distinct_expenses,
    unallocated_distinct_expense
  )

  # ----------------------------------------------------------------------------

  # Cumulative allocation for each expense for each month
  funding_by_month <- df %>%
    rowwise() %>%
    mutate(Month = list(months[months >= valid_from_month])) %>%
    unnest(Month) %>%
    group_by(expense_id, Month) %>%
    summarise(
      cumulative_allocated = sum(allocated_amount, na.rm = TRUE),
      .groups = "drop"
    )

  # Combining dataframe and recording shortfall timeline after
  # each expense latest payment date
  expense_month_grid <- all_distinct_expenses %>%
    mutate(expense_date_month = floor_date(expense_date_month, "month")) %>%
    crossing(months_df) %>%
    filter(Month >= expense_date_month)

  # Dataframe showing cumulative shortfalls for each expense across all months
  expenses_month_status <- expense_month_grid %>%
    left_join(funding_by_month, by = c("expense_id", "Month")) %>%
    mutate(
      cumulative_allocated = replace_na(cumulative_allocated, 0),
      shortfall = cumulative_allocated - expense_amount,
      is_short = shortfall < 0,
      is_overdue = is_short & (Month > expense_date_month)
    )

  filter_shortfall_df <- expenses_month_status %>%
    filter(shortfall < 0)

  # Final monthly shortfall dataframe
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

  shortfall_num <- expenses_month_status %>%
    filter(is_short == TRUE)

  total_shortfalls <- length(unique(shortfall_num$expense_id))

  total_balance <- sum(funding$amount)

  # -------------------------- SHORTFALL BAR PLOT ------------------------------

  # Number of shortfalls bar graph (by month)
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

  # Total shortfall amount bar graph (by month)
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

  # ----------------------------------------------------------------------------

  p$x$source <- "A"
  p <- event_register(p, "plotly_click")

  return(
    list(
      total_balance = total_balance,
      shortfall_plot = p,
      total_shortfalls = total_shortfalls
    )
  )
}


# CIRCOS PLOT

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

  allocation_with_funding_df <- df_allocations %>%
    left_join(
      funding %>%
        select(
          source_id,
          valid_from,
          valid_to
        ),
      by = "source_id"
    )

  full_allocation_df <- allocation_with_funding_df %>%
    left_join(
      df_expenses_status %>%
        select(
          expense_id,
          expense_amount = planned_amount,
          expense_date = latest_payment_date
        ),
      by = "expense_id"
    )

  ordered_expenses <- expenses[order(expenses$expense_id), ]

  sources_ids <- unique(funding$source_id)
  expenses_ids <- unique(ordered_expenses$expense_id)
  sectors <- c(sources_ids, expenses_ids)

  rows_until_month <- full_allocation_df %>%
    mutate(
      allocation_date = if_else(
        expense_date >= valid_from & expense_date <= valid_to,
        expense_date,
        valid_from
      )
    ) %>%
    filter(allocation_date < month)

  mat <- matrix(0, nrow = length(sectors), ncol = length(sectors))
  rownames(mat) <- sectors
  colnames(mat) <- sectors

  # Allocating to expenses
  for (i in 1:nrow(rows_until_month)) {
    mat[
      rows_until_month$source_id[i],
      rows_until_month$expense_id[i]
    ] <- rows_until_month$allocated_amount[i]
    mat[
      rows_until_month$expense_id[i],
      rows_until_month$source_id[i]
    ] <- rows_until_month$allocated_amount[i]
  }

  # Leftover expenses self-links at the current time (current month)
  cumulative_expense <- rows_until_month %>%
    group_by(expense_id) %>%
    summarise(
      expense_amount = first(expense_amount),
      cumulative_allocation = sum(allocated_amount),
      leftover_expense = expense_amount - cumulative_allocation,
      .groups = "drop"
    )

  for (i in 1:nrow(cumulative_expense)) {
    mat[
      cumulative_expense$expense_id[i],
      cumulative_expense$expense_id[i]
    ] <- cumulative_expense$leftover_expense[i]
  }

  # Funding that hasn't been allocated  or isn't available yet
  unallocated_funding <- funding %>%
    anti_join(rows_until_month, by = "source_id")

  # Leftover funding self-links
  if (nrow(unallocated_funding) > 0) {
    for (i in 1:nrow(unallocated_funding)) {
      mat[
        unallocated_funding$source_id[i],
        unallocated_funding$source_id[i]
      ] <- unallocated_funding$amount[i]
    }
  }

  funding_length <- length(sources_ids)
  expense_length <- length(expenses_ids)

  funding_colors <- rainbow(funding_length)
  expense_colors <- heat.colors(expense_length)
  sector_colors <- c(funding_colors, expense_colors)

  return(
    chorddiag(
      mat,
      groupColors = sector_colors,
      groupNames = sectors,
      groupThickness = 0.1,
      groupPadding = 5,
      groupnamePadding = 40,
      showTicks = TRUE,
      margin = 80,
      tooltipNames = sectors,
      tooltipUnit = "$",
      tooltipGroupConnector = " → ",
      chordedgeColor = "#B3B6B7"
    )
  )
}

