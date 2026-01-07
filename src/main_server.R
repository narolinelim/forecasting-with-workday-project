

# Contains all server components from server folder

source("src/server/allocation-algorithm.R")
source("src/server/data-processing.R")
source("src/server/io.R")
source("src/server/sorting.R")
source("src/server/graph.R")


main_server_logic <- function(input, output, session, values) {
  
  
  # ----------------------------
  # SORTING LOGIC
  # This section handles sorting of expenses based on user selection:
  # - Manual sorting (drag-and-drop order from UI)
  # - Sort by column (user-defined criteria and category order)
  # The output 'values$expenses_sorted' includes 'final_order' column
  # and will be used as input for the allocation algorithm
  # ----------------------------
  
  # ----------------------------
  # 1. Watch for changes in sorting mode from UI
  # 'sorting_mode' should come from a dropdown: "manual" or "by_rules"
  # ----------------------------
  observeEvent(input$sorting_mode, {
    
    # Get the selected sorting mode
    mode_selected <- input$sorting_mode
    
    # Get the processed expenses data from previous step
    # Must include 'original_index' for tie-breaking
    expenses_data <- values$expenses_data
    
    # ----------------------------
    # 2. Manual sorting
    # ----------------------------
    if (mode_selected == "manual") {
      
      # TODO: Retrieve user's drag-and-drop order from UI
      # This should be a dataframe 'manual_order' with column 'id'
      # matching expenses_data$id
      manual_order <- NULL  # placeholder
      
      # Call the sorting function
      expenses_sorted <- main_sorting_expenses(
        expenses_data = expenses_data,
        mode = "manual",
        manual_order = manual_order
      )
      
    }
    
    # ----------------------------
    # 3. Sort by column (by_rules)
    # ----------------------------
    else if (mode_selected == "by_rules") {
      
      # TODO: Retrieve user's ordering rules from UI
      # Example format:
      # list(
      #   criteria = c("latest_payment_date", "category"),
      #   category_order = c("salary", "travel", "research")
      # )
      ordering_rules <- NULL  # placeholder
      
      # Call the sorting function
      expenses_sorted <- main_sorting_expenses(
        expenses_data = expenses_data,
        mode = "by_rules",
        ordering_rules = ordering_rules
      )
      
    } else {
      stop("Invalid sorting mode selected")
    }
    
    # ----------------------------
    # 4. Save sorted expenses to reactive values
    # The sorted dataframe includes 'final_order' column and will be
    # used as input for the allocation algorithm
    # ----------------------------
    values$expenses_sorted <- expenses_sorted
    
    # ----------------------------
    # TODOs for future integration:
    # - Connect 'manual_order' to the actual UI drag-and-drop result
    # - Connect 'ordering_rules' to the UI ordering rules drag-and-drop
    # ----------------------------
    
  })
  
  
  
  
}