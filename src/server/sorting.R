

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
  # 调试信息打印 (Console Logging)
  # =========================================================
  cat("\n--- [Sorting Debug Info] ---\n")
  cat("Current Mode:", ifelse(is.null(mode), "NULL", mode), "\n")
  
  if (!is.null(ordering_rules)) {
    cat("Rule 1 (P1):", ordering_rules$p1_item, "\n")
    cat("Rule 2 (P2):", ordering_rules$p2_item, "\n")
    
    # 打印日期排序方向
    if ("Payment Date" %in% c(ordering_rules$p1_item, ordering_rules$p2_item)) {
      date_dir_text <- if(isTruthy(ordering_rules$p1_date_dir) && ordering_rules$p1_date_dir == "latest_payment_date") {
        "Latest -> Earliest (降序)"
      } else {
        "Earliest -> Latest (升序)"
      }
      cat("Date Direction:", date_dir_text, "\n")
    }
    
    # 打印分类拖拽顺序
    if (!is.null(ordering_rules$category_order)) {
      cat("Category Drag Order:", paste(ordering_rules$category_order, collapse = " > "), "\n")
    }
  }
  cat("----------------------------\n\n")
    
    # Convert category column to factor based on user-defined order
    # This ensures that the 'category' column will be sorted according to the order specified by the user.
    # Example:
    #   Suppose expenses_data$category = c("travel", "salary", "research")
    #   and ordering_rules$category_order = c("salary", "travel", "research")
    #   After conversion:
    #     factor levels are: salary < travel < research
    #   So when we sort, rows with 'salary' will come first, then 'travel', then 'research'.
    
  # ---------------------------------------------------------
  # 1. Column Sorting
  # ---------------------------------------------------------
  if (mode == "by_rules") {
    
    if (is.null(ordering_rules)) {
      stop("By_rules mode selected but ordering_rules is NULL")
    }
    
    # A. 体现用户在页面上拖拽分类方块形成的顺序
    # 将 expense_category 转为 factor，levels 的顺序就是用户期望的先后顺序
    if (!is.null(ordering_rules$category_order)) {
      expenses_data <- expenses_data %>%
        mutate(expense_category = factor(
          expense_category, 
          levels = ordering_rules$category_order
        ))
    }
    
    # B. 定义映射函数：根据项目类型决定排序表达式
    # 逻辑：如果是Categories看拖拽顺序；如果是Date看earliest/latest方向
    get_sort_expression <- function(p_item, p_date_dir) {
      if (is.null(p_item) || p_item == "None") return(NULL)
      
      if (p_item == "Categories") {
        # 直接按上面 mutate 好的 factor 顺序排
        return(expr(expense_category))
      } 
      
      if (p_item == "Payment Date") {
        # 根据方向参数决定升序或降序
        if (!is.null(p_date_dir) && p_date_dir == "latest_payment_date") {
          return(expr(desc(latest_payment_date))) # 大的（晚的）在前
        } else {
          return(expr(latest_payment_date))       # 小的（早的）在前
        }
      }
      return(NULL)
    }
    
    # C. 构建多级排序列表
    sort_list <- list()
    
    # 1st Priority: 第一优先级
    p1_expr <- get_sort_expression(ordering_rules$p1_item, ordering_rules$p1_date_dir)
    if (!is.null(p1_expr)) sort_list[[length(sort_list) + 1]] <- p1_expr
    
    # 2nd Priority: 第二优先级
    p2_expr <- get_sort_expression(ordering_rules$p2_item, ordering_rules$p2_date_dir)
    if (!is.null(p2_expr)) sort_list[[length(sort_list) + 1]] <- p2_expr
    
    # D. 特殊情形处理 (Tie-breaker)
    # 当 payment date 和 categories 相同时，这样当规则相同时，会保持当前的相对先后顺序
    sort_list[[length(sort_list) + 1]] <- expr(priority)
    
    # E. 执行排序并重写序号
    # 使用 !!! 将列表中的表达式解包给 arrange 函数
    expenses_sorted <- expenses_data %>%
      arrange(!!!sort_list) %>%
      mutate(priority = row_number()) # 新序号替换原来的费用序号
    
    # return(expenses_sorted)
    
  } 
  
  # ---------------------------------------------------------
  # 2. 手动排序模式 (Manual Sorting)
  # ---------------------------------------------------------
  else if (mode == "manual") {
    # 手动模式下，行顺序已经在 UI/Server 交互中排好了
    # 这里只负责重写 priority 序号
    expenses_sorted <- expenses_data %>%
      mutate(priority = row_number())
    return(expenses_sorted)
  }
  # =========================================================
  # 打印排序后的结果快照
  # =========================================================
  cat("\n>>> [Sorting Result Preview] <<<\n")
  if (nrow(expenses_sorted) > 0) {
    # 选择关键列进行展示，方便对比
    preview_df <- expenses_sorted %>%
      select(priority, item_id, expense_category, latest_payment_date) %>%
      head(10) # 打印前10行
    
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
row_reorder <- function(input, values, proxy, id_col) {
  observeEvent(input$newOrder, {
    # Match output with indexes
    new_idx <- match(input$newOrder, values$expenses[[id_col]])

    # Reorder the dataframe
    values$expenses <- values$expenses[new_idx, ]

    # Update the new priority
    values$expenses[[id_col]] <- seq_len(nrow(values$expenses))
    
    # Update the DataTable proxy to reflect changes
    replaceData(proxy, values$expenses, resetPaging = FALSE, rownames = FALSE)
  })
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


