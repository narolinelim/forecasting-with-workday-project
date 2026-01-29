

library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)
library(circlize)
library(chorddiag)



# SHORTFALL PLOT

create_shortfall_bar <- function(values) {
  
  # This works if we ignore overdue payment
  #
  # date_ordered_allocation <- ordered_allocation[order(ordered_allocation$Date),]
  # 
  # date_ordered_allocation$shortfall <- date_ordered_allocation$Allocated - date_ordered_allocation$Amount
  # 
  # expense_shortfall <- date_ordered_allocation %>%
  #   filter(shortfall < 0) %>%
  #   mutate(StartMonth = floor_date(Date, "month"))
  # 
  # months <- seq(
  #   from = floor_date(min(date_ordered_allocation$Date), "month"),
  #   to = floor_date(max(date_ordered_allocation$Date), "month"),
  #   by = "1 month"
  # )
  # 
  # monthly_shortfall <- expense_shortfall %>%
  #   rowwise() %>%
  #   mutate(Month = list(months[months >= StartMonth])) %>%
  #   unnest(Month) %>%
  #   ungroup() %>%
  #   group_by(Month) %>%
  #   summarise(
  #     TotalShortfall = sum(shortfall),
  #     NumberOfShortfalls = n(),
  #     .groups = "drop"
  #   )
  # 
  # total_shortfalls <- tail(monthly_shortfall$NumberOfShortfalls, n = 1)
  # 
  
  df_allocations <- values$allocation_result
  funding <- values$funding_sources
  df_expenses_status <- values$expense_status
  print(df_allocations)
  
  
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
  
  
  # Dataframe including range of months involved in the allocation and prepping
  # for final shortfall dataframe
  months <- seq(
    from = min(df$expense_date_month),
    to = max(df$valid_to_month),
    by = "1 month"
  )
  months_df <- tibble(Month = months)
  

  # Extracting distinct expenses 
  distinct_expenses <- df %>% 
    distinct(expense_id, expense_amount, expense_date_month)

  
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
  expense_month_grid <- distinct_expenses %>%
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
  
  print(expenses_month_status, n = 64)
  

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
  
  print(monthly_shortfall)
  print("expense stats")
  print(expenses_month_status, n = 85)
  
  shortfall_num <- expenses_month_status %>%
    filter(is_short == TRUE)
  
  total_shortfalls <- length(unique(shortfall_num$expense_id))
  
  total_balance <- sum(funding$amount)
  
  
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
          y = -0.07,
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
  
  list(
    total_balance = total_balance,
    shortfall_plot = p,
    total_shortfalls = total_shortfalls
  )
  
}


# CIRCOS PLOT

create_circos_plot <- function(values, month) {
  #allocation should be done within the month of the latest payment date
  # if funding has a big interval.
  
  #print(month)
  
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
  
  ordered_expenses <- expenses[order(expenses$expense_id),]
  #print(funding)
  
  sources_ids <- unique(funding$source_id)
  expenses_ids <- unique(ordered_expenses$expense_id)
  sectors <- c(sources_ids, expenses_ids)
  #print(sectors)
  
  
  rows_until_month <- full_allocation_df %>%
    filter(valid_from < month)
  #print(rows_until_month)
  
  mat <- matrix(0, nrow = length(sectors), ncol = length(sectors))
  rownames(mat) <- sectors
  colnames(mat) <- sectors
  
  # Allocating to expenses
  for (i in 1:nrow(rows_until_month)) {
    mat[rows_until_month$source_id[i], rows_until_month$expense_id[i]] <- rows_until_month$allocated_amount[i]
    mat[rows_until_month$expense_id[i], rows_until_month$source_id[i]] <- rows_until_month$allocated_amount[i]
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
    mat[cumulative_expense$expense_id[i], cumulative_expense$expense_id[i]] <- cumulative_expense$leftover_expense[i]
  }
  
  # Funding that hasn't been allocated  or isn't available yet
  unallocated_funding <- funding %>%
    anti_join(rows_until_month, by = "source_id")
  
  
  # Leftover funding self-links
  if (nrow(unallocated_funding) > 0) {
    for (i in 1:nrow(unallocated_funding)) {
      mat[unallocated_funding$source_id[i], unallocated_funding$source_id[i]] <- unallocated_funding$amount[i]
    }
  }
  #print(mat)
  
  
  funding_length <- length(sources_ids)
  expense_length <- length(expenses_ids)
  
  
  funding_colors <- rainbow(funding_length)
  expense_colors <- heat.colors(expense_length)
  sector_colors <- c(funding_colors, expense_colors)
  
  chorddiag(mat,
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
            chordedgeColor = "#B3B6B7")
  
}

