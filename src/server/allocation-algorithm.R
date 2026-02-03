# ----------------------------------------------------
#
#  Allocation Algorithm V3: allows for overdue payment
#
# ----------------------------------------------------


date_to_int <- function(date_str, base_date) {
  #' Convert date string "DD/MM/YYYY" to integer days since base_date
  #' 
  #' @param date_str Date string in "DD/MM/YYYY" format
  #' @param base_date Date object representing the base date
  #' 
  #' @return Integer number of days since base_date

  dt <- as.Date(date_str, format = "%d/%m/%Y")
  # Return numeric difference + 1 (so the first day is Day 1, not Day 0)
  return(as.numeric(dt - base_date) + 1)
}

build_compatibility_matrix <- function(sources, expenses) {
  #' Build a compatibility matrix between funding sources and expenses
  #'
  #' @param sources DataFrame of funding sources
  #' @param expenses DataFrame of expenses
  #'
  #' @return A compatibility matrix (data.frame) indicating valid funding sources for each expense

  # Dynamically setting the time
  # 1. Collect all date columns from both dataframes
  all_dates <- c(sources$valid_from, sources$valid_to, expenses$latest_payment_date)
  # 2. Convert to Date objects to find the minimum
  date_objects <- as.Date(unique(all_dates), format = "%d/%m/%Y")
  # 3. Find the earliest date
  global_min_date <- min(date_objects, na.rm = TRUE)
  
  # getting total number of funding source and expenses (NOT total amount)
  n_sources <- nrow(sources)
  n_expenses <- nrow(expenses)
  
  # This is a matrix with size n_sources x n_expenses (row is each funding sources, and column is each expenses sources), such that if the payment date of the expense fall within the valid from and valid to of the funding AND categories of the expense match with the allowed category of the source, then it will be marked as 1, otherwise 0
  # We build a Compatibility Matrix (Valid = 1, Invalid = 0)
  compatibility <- matrix(0, nrow = n_sources, ncol = n_expenses)
  
  for (i in 1:n_sources) {
    for (j in 1:n_expenses) {
      # 1. Category Check
      # Expense category must be in the Source's allowed list
      cat_match <- expenses$expense_category[j] %in% sources$allowed_categories[[i]]
      
      # 2. Time Validity Check
      # Convert dates to integers (Day of Year)
      s_from <- date_to_int(sources$valid_from[i], global_min_date)
      s_to   <- date_to_int(sources$valid_to[i], global_min_date)
      e_date <- date_to_int(expenses$latest_payment_date[j], global_min_date) # Assumes this is the actual payment date
      
      # LOGIC: The payment date must be INSIDE the funding window (Inclusive)
      # ValidFrom <= PaymentDate <= ValidTo
      time_match <- (e_date >= s_from & e_date <= s_to)
      
      # Combine Checks
      if (cat_match && time_match) {
        compatibility[i, j] <- 1
      } else {
        compatibility[i, j] <- 0
      }
    }
  }
  
  return (compatibility)
}

solve_constraint_model <- function(sources, expenses, compatibility) {
  #' Solve the MIP model for funding allocation
  #'
  #' @param sources DataFrame of funding sources
  #' @param expenses DataFrame of expenses
  #' @param compatibility Compatibility matrix between sources and expenses
  #' 
  #' @return The result of the solved MIP model
  
  n_sources <- nrow(sources)
  n_expenses <- nrow(expenses)
  
  # Maximise Sum(Weight_j * y_j)
  # Coefficients for x[i,j] are 0. Coefficients for y[j] are the weights.
  # It works because $$2^k > \sum_{i=0}^{k-1} 2^i$$, so it will always incentivise to use the funding to fulfil the expenses with higher priority
  
  weights <- 2^(n_expenses - 1 - (0:(n_expenses-1))) # Powers of 2 descending
  
  # Define the Model
  model <- MIPModel() %>%
    
    # --- Variables ---
    # x[i,j]: Amount source i pays for expense j (Continuous, Non-negative)
    add_variable(x[i, j], i = 1:n_sources, j = 1:n_expenses, type = "continuous", lb = 0) %>%
    
    # y[j]: Binary indicator if expense j is fully paid (0 or 1)
    add_variable(y[j], j = 1:n_expenses, type = "binary") %>%
    
    # --- Objective Function ---
    # Maximize Sum(Weights * y)
    set_objective(sum_expr(weights[j] * y[j], j = 1:n_expenses), "max") %>%
    
    # --- Constraints ---
    
    # 1. Supply Constraint: Sum of allocations from Source i <= Source Amount
    add_constraint(sum_expr(x[i, j], j = 1:n_expenses) <= sources$amount[i], i = 1:n_sources) %>%
    
    # 2. Demand Linking: Sum of allocations to Expense j == Expense Amount * y[j]
    # If y[j]=1, we must pay full amount. If y[j]=0, we pay 0.
    add_constraint(sum_expr(x[i, j], i = 1:n_sources) == expenses$planned_amount[j] * y[j], j = 1:n_expenses) %>%
    
    # 3. Compatibility Constraint
    # If compatibility[i, j] == 0, then x[i, j] must be 0
    add_constraint(x[i, j] == 0, i = 1:n_sources, j = 1:n_expenses, compatibility[i, j] == 0)
  
  result <- solve_model(model, with_ROI(solver = "highs"))
  
  return (result)
}



apply_greedy_fill <- function(result, sources, expenses, compatibility) {
  #' Apply a greedy fill algorithm to allocate remaining funds to unfunded expenses
  #'
  #' @param result The result of the solved MIP model
  #' @param sources DataFrame of funding sources
  #' @param expenses DataFrame of expenses
  #' @param compatibility Compatibility matrix between sources and expenses
  #' 
  #' @return A matrix representing the final allocation of funds to expenses
  
  n_sources <- nrow(sources)
  n_expenses <- nrow(expenses)
  
  # A. Reconstruct the Optimal Allocation Matrix (Fully Funded Only)
  x_sol_raw <- get_solution(result, x[i, j])
  mat_x <- matrix(0, nrow = n_sources, ncol = n_expenses)
  for (r in 1:nrow(x_sol_raw)) {
    mat_x[x_sol_raw$i[r], x_sol_raw$j[r]] <- x_sol_raw$value[r]
  }
  
  # B. Calculate Remaining Capacity per Source
  current_source_usage <- rowSums(mat_x)
  source_remaining <- sources$amount - current_source_usage
  source_remaining[source_remaining < 1e-6] <- 0 # Fix floating point dust
  
  # C. Identify Unfunded Expenses (Binary y[j] == 0)
  y_sol_raw <- get_solution(result, y[j])
  # Order by j to ensure we respect priority (index 1 is highest priority)
  y_sol_raw <- y_sol_raw[order(y_sol_raw$j), ]
  unfunded_indices <- y_sol_raw$j[y_sol_raw$value < 0.5]
  
  # D. The Greedy Loop
  for (j in unfunded_indices) {
    amount_needed <- expenses$planned_amount[j]
    
    # Try to find money in compatible sources
    for (i in 1:n_sources) {
      if (amount_needed < 1e-6) break 
      
      # Check compatibility AND available funds
      if (source_remaining[i] > 1e-6) {
        take_amount <- min(amount_needed, source_remaining[i])
        
        # Update Matrix & Balances
        mat_x[i, j] <- mat_x[i, j] + take_amount
        source_remaining[i] <- source_remaining[i] - take_amount
        amount_needed <- amount_needed - take_amount
      }
    }
  }
  
  return(mat_x)
}

print_financial_report <- function(mat_x, sources, expenses) {
  
  n_sources <- nrow(sources)
  n_expenses <- nrow(expenses)
  
  cat("\n============================================================\n")
  cat(sprintf("%-60s\n", "                   FINAL SOLUTION REPORT                    "))
  cat("============================================================\n")
  
  # Calculate Status based on matrix totals
  expense_total_alloc <- colSums(mat_x)
  idx_full <- which(expense_total_alloc >= expenses$planned_amount - 1e-5)
  idx_partial <- which(expense_total_alloc > 1e-5 & expense_total_alloc < expenses$planned_amount - 1e-5)
  idx_none <- which(expense_total_alloc <= 1e-5)
  
  # --- 1. FULLY FUNDED ---
  cat("\n--- Fully Funded Expenses ---\n")
  if(length(idx_full) > 0) {
    for (j in idx_full) {
      cat(sprintf("%s: %s ($%s)\n", expenses$expense_id[j], expenses$expense_category[j], format(expenses$planned_amount[j], big.mark=",")))
    }
  } else { cat("None.\n") }
  
  # --- 2. PARTIALLY FUNDED ---
  cat("\n--- Partially Funded Expenses ---\n")
  if(length(idx_partial) > 0) {
    for (j in idx_partial) {
      filled_amt <- expense_total_alloc[j]
      percent <- (filled_amt / expenses$planned_amount[j]) * 100
      cat(sprintf("%s: %s ($%s / $%s) - %.1f%% Covered\n", expenses$expense_id[j], expenses$expense_category[j], format(filled_amt, big.mark=","), format(expenses$planned_amount[j], big.mark=","), percent))
    }
  } else { cat("None.\n") }
  
  # --- 3. UNFUNDED ---
  cat("\n--- Unfunded Expenses ---\n")
  if(length(idx_none) > 0) {
    for (j in idx_none) {
      cat(sprintf("%s: %s ($%s) - Due: %s\n", expenses$expense_id[j], expenses$expense_category[j], format(expenses$planned_amount[j], big.mark=","), expenses$latest_payment_date[j]))
    }
  } else { cat("None.\n") }
  
  cat(sprintf("\nSummary: %d Full / %d Partial / %d Missed\n", length(idx_full), length(idx_partial), length(idx_none)))
  
  # --- 4. ALLOCATION DETAILS ---
  cat("\n--- Allocation Details ---\n")
  cat(sprintf("%-8s %-8s %-15s %s\n", "Source", "Expense", "Exp. Category", "Amount Allocated"))
  cat("------------------------------------------------------------\n")
  for (i in 1:n_sources) {
    for (j in 1:n_expenses) {
      val <- mat_x[i, j]
      if (val > 1e-6) {
        cat(sprintf("%-8s -> %-8s %-15s $%s\n", sources$source_id[i], expenses$expense_id[j], expenses$expense_category[j], format(val, nsmall=2, big.mark=",")))
      }
    }
  }
  
  # --- 5. REMAINING BALANCES ---
  cat("\n--- Remaining Fund Balances ---\n")
  cat(sprintf("%-8s %-10s %-10s %-10s %s\n", "Fund ID", "Initial", "Used", "Remaining", "Allowed Categories"))
  cat("-----------------------------------------------------------------\n")
  total_unused <- 0
  for (i in 1:n_sources) {
    used <- sum(mat_x[i, ])
    remaining <- sources$amount[i] - used
    if (remaining < 1e-6) remaining <- 0
    total_unused <- total_unused + remaining
    cats_str <- paste(unlist(sources$allowed_categories[i]), collapse = ", ")
    cat(sprintf("%-8s %-10s %-10s %-10s %s\n", sources$source_id[i], format(sources$amount[i], big.mark=",", nsmall=0), format(round(used), big.mark=",", nsmall=0), format(round(remaining), big.mark=",", nsmall=0), cats_str))
  }
  cat("-----------------------------------------------------------------\n")
  cat(sprintf("TOTAL UNUSED FUNDS: $%s\n", format(total_unused, big.mark=",")))
}

create_financial_dfs <- function(mat_x, sources, expenses) {
  #' Create DataFrames for allocations, expense status, and funds summary
  #'
  #' @param mat_x Final allocation matrix
  #' @param sources DataFrame of funding sources
  #' @param expenses DataFrame of expenses
  #'
  #' @return A list containing three DataFrames: allocations, expenses status, funds summary, and full allocation data
  #' @details
  #' df_allocations:
  #' SourceID: ID of the funding source (e.g., FS001).
  #' ExpenseID: ID of the expense being paid (e.g., E004).
  #' ExpenseCategory: The category of the expense (e.g., Salary).
  #' AllocatedAmount: The exact dollar amount transferred.
  #'
  #' df_expenses_status:
  #' All original columns (ID, Category, Amount, Date) plus:
  #' IsFilled: A Boolean (TRUE/FALSE) indicating if the optimization solver selected this expense.
  #'
  #' df_funds_summary:
  #' SourceID: ID of the fund.
  #' InitialAmount: The starting budget.
  #' UsedAmount: Total allocated in this solution (sum(x_matrix[i, ])).
  #' RemainingAmount: What is left over (Initial - Used).
  #' 
  #' df_full_allocation:
  #' A combined DataFrame showing detailed allocation per expense including status.

  n_sources <- nrow(sources)
  n_expenses <- nrow(expenses)
  
  # --- 1. Allocations DataFrame ---
  # Find all non-zero entries in the matrix
  # which(..., arr.ind=TRUE) returns a matrix of [row_index, col_index]
  alloc_idx <- which(mat_x > 1e-6, arr.ind = TRUE)
  
  # Construct the dataframe directly from indices
  df_allocations <- data.frame(
    source_id = sources$source_id[alloc_idx[, 1]],
    expense_id = expenses$expense_id[alloc_idx[, 2]],
    expense_category = expenses$expense_category[alloc_idx[, 2]],
    allocated_amount = mat_x[alloc_idx]
  )
  # Optional: Sort by Source then Expense
  df_allocations <- df_allocations[order(df_allocations$source_id, df_allocations$expense_id), ]
  
  
  # --- 2. Expense Status DataFrame ---
  # Calculate how much was allocated to each expense (Column Sums)
  expense_filled_amounts <- colSums(mat_x)
  
  df_expenses_status <- expenses
  df_expenses_status$filled_amount <- expense_filled_amounts
  
  # Determine status: Fully Filled if allocated >= requested (minus tiny error)
  df_expenses_status$is_filled <- expense_filled_amounts >= (expenses$planned_amount - 1e-5)
  
  # Add a readable Status column
  df_expenses_status$status <- ifelse(df_expenses_status$is_filled, "Full",
                                      ifelse(df_expenses_status$filled_amount > 1e-6, "Partial", "Unfunded"))
  
  
  # --- 3. Funds Summary DataFrame ---
  # Calculate how much each source used (Row Sums)
  source_used_amounts <- rowSums(mat_x)
  
  df_funds_summary <- data.frame(
    source_id = sources$source_id,
    initial_amount = sources$amount,
    used_amount = source_used_amounts,
    remaining_amount = sources$amount - source_used_amounts
  )
  # Clean up negative zeros
  df_funds_summary$remaining_amount[df_funds_summary$remaining_amount < 0] <- 0
  
  df_full_allocation <- df_allocations %>%
    left_join(
      df_expenses_status %>%
        select(
          expense_id,
          expense_category,
          planned_amount,
          latest_payment_date,
          status
        ), by = c("expense_id", "expense_category"))
  
  # Return all 3 as a named list
  return(list(
    allocations = df_allocations,
    expenses = df_expenses_status,
    funds = df_funds_summary,
    full_allocation_data = df_full_allocation
  ))
}

activate_allocation_algorithm <- function(sources, expenses) {
  #' Main function to run the allocation algorithm
  #'
  #' @param sources DataFrame of funding sources
  #' @param expenses DataFrame of expenses
  #' 
  #' @return A list containing three DataFrames: allocations, expenses status, and funds summary
  
  compatibility <- build_compatibility_matrix(sources, expenses)
  
  result <- solve_constraint_model(sources, expenses, compatibility)
  
  if (result$status == "optimal" || result$status == "success") {
    
    # Partial fill
    final_matrix <- apply_greedy_fill(result, sources, expenses, compatibility)
    
    # Print report in R for testing
    # print_financial_report(final_matrix, sources, expenses)
    
    # Generate DataFrames
    dfs <- create_financial_dfs(final_matrix, sources, expenses)
    
    
    df_allocations <- dfs$allocations
    df_expenses_status <- dfs$expenses
    df_funds_summary <- dfs$funds
    
  } else {
    cat("No optimal solution found.\n")
    return (NULL)
  }
  
  return (dfs)
}
