

# Handles the sorting of expenses
library(dplyr)
library(rlang)

# Main sorting function
# This function sorts the expenses data based on user selection.
# 1. "manual"   - sorting according to user drag-and-drop order (handled by DT/Shiny)
# 2. "by_rules" - sorting according to column priorities and dynamic category order
main_sorting_expenses <- function(expenses_data, 
                                  mode = NULL,           # "manual" or "by_rules"
                                  # manual_order = NULL,     # Dataframe representing user's drag-and-drop order (for manual mode)
                                  ordering_rules = NULL) { # List representing user's sorting rules (for by_rules mode)

  # =========================================================
  # Debug information print
  # =========================================================
  cat("\n--- [Sorting Debug Info] ---\n")
  cat("Current Mode:", ifelse(is.null(mode), "NULL", mode), "\n")
  
  if (!is.null(ordering_rules)) {
    cat("Rule 1 (P1):", ordering_rules$p1_item, "\n")
    cat("Rule 2 (P2):", ordering_rules$p2_item, "\n")
    
    # Print date sorting direction
    if ("Payment Date" %in% c(ordering_rules$p1_item, ordering_rules$p2_item)) {
      date_dir_text <- if(isTruthy(ordering_rules$p1_date_dir) && ordering_rules$p1_date_dir == "latest_payment_date") {
        "Latest -> Earliest (Descending)"
      } else {
        "Earliest -> Latest (Ascending)"
      }
      cat("Date Direction:", date_dir_text, "\n")
    }
    
    # Print category drag-and-drop order
    if (!is.null(ordering_rules$category_order)) {
      cat("Category Drag Order:", paste(ordering_rules$category_order, collapse = " > "), "\n")
    }
  }
  cat("----------------------------\n\n")

  # ---------------------------------------------------------
  # 1. Column Sorting
  # ---------------------------------------------------------
  if (mode == "by_rules") {
    
    if (is.null(ordering_rules)) {
      stop("By_rules mode selected but ordering_rules is NULL")
    }
    
    # A. Reflect the order of category blocks dragged by the user on the page
    # Convert `expense_category` to a factor; the order of `levels` represents the user's desired sequence
    
    if (!is.null(ordering_rules$category_order)) {
      expenses_data <- expenses_data %>%
        mutate(expense_category = factor(
          expense_category, 
          levels = ordering_rules$category_order
        ))
    }
    
    # B. Define a mapping function: determine the sorting expression based on the project type
    # Logic: if it's Categories, use the drag-and-drop order; if it's Date, use the earliest/latest direction
    
    get_sort_expression <- function(p_item, p_date_dir) {
      if (is.null(p_item) || p_item == "None") return(NULL)
      
      if (p_item == "Categories") {
        # Sort directly according to the factor order set by mutate
        return(expr(expense_category))
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
    p1_expr <- get_sort_expression(ordering_rules$p1_item, ordering_rules$p1_date_dir)
    if (!is.null(p1_expr)) sort_list[[length(sort_list) + 1]] <- p1_expr
    
    # 2nd Priority
    p2_expr <- get_sort_expression(ordering_rules$p2_item, ordering_rules$p2_date_dir)
    if (!is.null(p2_expr)) sort_list[[length(sort_list) + 1]] <- p2_expr
    
    # D. Tie-breaker
    # When payment date and categories are the same, this preserves the current relative order when rules are equal
    sort_list[[length(sort_list) + 1]] <- expr(priority)
    
    # E. Perform the sorting and rewrite the indices
    # Use !!! to unquote the expressions in the list for the arrange function
    expenses_sorted <- expenses_data %>%
      arrange(!!!sort_list) %>%
      mutate(priority = row_number())
    
    # return(expenses_sorted)
    
  } 
  
  # ---------------------------------------------------------
  # 2. Manual Sorting
  # ---------------------------------------------------------
  else if (mode == "manual") {
    expenses_sorted <- expenses_data %>%
      mutate(priority = row_number())
    return(expenses_sorted)
  }
  # =========================================================
  # Print a snapshot of the sorted results
  # =========================================================
  cat("\n>>> [Sorting Result Preview] <<<\n")
  if (nrow(expenses_sorted) > 0) {
    # Select key columns to display
    preview_df <- expenses_sorted %>%
      select(priority, expense_id, expense_category, latest_payment_date) %>%
      head(10) # Print the first 10 rows
    
    print(preview_df)
    
    if (nrow(expenses_sorted) > 10) {
      cat("... (and", nrow(expenses_sorted) - 10, "more rows)\n")
    }
  } else {
    cat("Warning: No sorted data to display.\n")
  }
  cat("================================\n\n")
  
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


