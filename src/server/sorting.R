# ---- Column-based Sorting ----
col_ordering <- function(expenses_data, ordering_rules) {
  #' This function sorts the expenses data based on column priorities and dynamic category order.
  #'
  #' @param expenses_data: DataFrame containing expenses information
  #' @param ordering_rules: List containing sorting preferences:
  #' - p1_item: 1st priority item ("Categories", "Payment Date", or "None")
  #' - p1_date_dir: Direction for payment date sorting ("earliest_payment_date", "latest_payment_date", or NULL)
  #' - p2_item: 2nd priority item ("Categories", "Payment Date", or "None")
  #' - p2_date_dir: Direction for payment date sorting ("earliest_payment_date", "latest_payment_date", or NULL)
  #' - category_order: Vector defining the user-specified order of expense categories
  #' @return: Sorted expenses DataFrame with updated priority indices

  # A. Reflect the order of category blocks dragged by the user on the page
  expenses_data <- expenses_data %>%
    mutate(
      category_rank = match(expense_category, ordering_rules$category_order)
    )
  # B. Define a mapping function: determine the sorting expression based on the project type
  get_sort_expression <- function(p_item, p_date_dir) {
    if (is.null(p_item) || p_item == "None") {
      return(NULL)
    }

    if (p_item == "Categories") {
      # Sort directly according to the factor order set by mutate
      return(expr(category_rank))
    }

    if (p_item == "Payment Date") {
      # Determine ascending or descending order based on the direction parameter
      if (!is.null(p_date_dir) && p_date_dir == "latest_payment_date") {
        return(expr(desc(latest_payment_date)))
      } else {
        return(expr(latest_payment_date))
      }
    }
    return(NULL)
  }

  # C. Construct a multi-level sorting list
  sort_list <- list()

  # 1st Priority
  p1_expr <- get_sort_expression(
    ordering_rules$p1_item,
    ordering_rules$p1_date_dir
  )
  if (!is.null(p1_expr)) {
    sort_list[[length(sort_list) + 1]] <- p1_expr
  }

  # 2nd Priority
  p2_expr <- get_sort_expression(
    ordering_rules$p2_item,
    ordering_rules$p2_date_dir
  )
  if (!is.null(p2_expr)) {
    sort_list[[length(sort_list) + 1]] <- p2_expr
  }

  # D. Tie-breaker
  # When payment date and categories are the same, this preserves the current relative order when rules are equal
  sort_list[[length(sort_list) + 1]] <- expr(priority)

  # E. Perform the sorting and rewrite the indices
  # Use !!! to unquote the expressions in the list for the arrange function
  expenses_sorted <- expenses_data %>%
    arrange(!!!sort_list) %>%
    mutate(priority = row_number()) %>%
    select(-category_rank)
  return(expenses_sorted)
}


# ---- Manual Row Reordering ----
row_reorder <- function(newOrder, expenses, proxy, id_col) {
  #' Updates the expenses dataframe based on the new order provided by the user.
  #'
  #' @param newOrder: vector of item IDs in the new order
  #' @param expenses: current expenses dataframe
  #' @param proxy: DT proxy object for updating the DataTable
  #' @param id_col: name of the column containing unique item IDs
  #' 
  #' @return: updated expenses dataframe with new priority
  #' 

  if (is.null(newOrder) || length(newOrder) == 0) {
    return (expenses)
  }
  
  new_idx <- match(newOrder, expenses[[id_col]])
  df <- expenses[new_idx, ] |> mutate(priority = seq_len(nrow(expenses)))
  replaceData(proxy, df, resetPaging = FALSE, rownames = FALSE)

  return(df)
}

# JavaScript callback for row reordering
row_reorder_callback <- c(
  "table.on('row-reorder', function(e, details, edit) {",
  "  var ids = table.column(0).data().toArray();", # Get current id order
  "  var newOrder = [...ids];",
  "  for(var entry of details) {",
  "    newOrder[entry.newPosition] = ids[entry.oldPosition];",
  "  }",
  "  Shiny.setInputValue('newOrder', newOrder, {priority: 'event'});",
  "});"
)


