

# Handles the sorting of expenses


library(dplyr)

# Main sorting function
# This function sorts the expenses data based on user selection.
# It supports two modes:
# 1. "manual"   - sorting according to user drag-and-drop order
# 2. "by_rules" - sorting according to user-defined column rules (criteria and category order)
main_sorting_expenses <- function(expenses_data, 
                                  mode = "NULL",           # "manual" or "by_rules"
                                  manual_order = NULL,     # Dataframe representing user's drag-and-drop order (for manual mode)
                                  ordering_rules = NULL) { # List representing user's sorting rules (for by_rules mode)
  
  # ----------------------------
  # 1. Manual sorting (implementation later)
  # ----------------------------
  if (mode == "manual") {
    if (is.null(manual_order)) {
      stop("Manual mode selected but manual_order is NULL")
    }

    expenses_sorted <- expenses_data
    
  } 
  
  # ----------------------------
  # 2. Sort by column (by_rules)
  # ----------------------------
  else if (mode == "by_rules") {
    
    if (is.null(ordering_rules)) {
      stop("By_rules mode selected but ordering_rules is NULL")
    }
    
    # ----------------------------
    # Example of ordering_rules:
    # list(
    #   criteria = c("latest_payment_date", "category"),
    #   category_order = c("salary", "travel", "research")
    # )
    # ----------------------------
    
    # 2.1 Convert category column to factor based on user-defined order
    # This ensures that the 'category' column will be sorted according to the order specified by the user.
    # Example:
    #   Suppose expenses_data$category = c("travel", "salary", "research")
    #   and ordering_rules$category_order = c("salary", "travel", "research")
    #   After conversion:
    #     factor levels are: salary < travel < research
    #   So when we sort, rows with 'salary' will come first, then 'travel', then 'research'.
    
    if ("category" %in% names(expenses_data)) {
      if (!is.null(ordering_rules$category_order)) {
        expenses_data$category <- factor(
          expenses_data$category,
          levels = ordering_rules$category_order
        )
      }
    }
    
    # 2.2 Combine criteria columns + original_index to create a complete sorting order. 
    sort_cols <- c(ordering_rules$criteria, "original_index")
    
    # 2.3 Apply dplyr::arrange to sort according to the defined columns
    expenses_sorted <- expenses_data %>%
      arrange(across(all_of(sort_cols)))
  } 
  
  # ----------------------------
  # 3. Handle invalid mode input
  # ----------------------------
  else {
    stop("mode must be either 'manual' or 'by_rules'")
  }
  
  # ----------------------------
  # 4. Add final_order column
  # This column represents the row position after sorting (1 for first, 2 for second, ...)
  # ----------------------------
  expenses_sorted <- expenses_sorted %>%
    mutate(final_order = row_number())
  
  # ----------------------------
  # 5. Return the sorted dataframe
  # ----------------------------
  return(expenses_sorted)
}

# --- Manual Row Reordering ---
row_reorder <- function(newOrder, expenses, proxy, id_col) {
  new_idx <- match(newOrder, expenses[[id_col]])
  df <- expenses[new_idx, ] |> mutate(priority = seq_len(nrow(expenses)))
  replaceData(proxy, df, resetPaging = FALSE, rownames = FALSE)

  return(df) # Return updated dataframe
}

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


