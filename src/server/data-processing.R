read_excel_data <- function(file_path, sheet_name) {
  #' Read and process the uploaded Excel data
  #'
  #' @param file_path: path to the uploaded excel file
  #' @param sheet_name: name of the sheet to read
  #'
  #' @return: A list containing the funding sources and expense data frames
  #'
  funding_sources_df <- read_excel(file_path, sheet = "Funding") %>%
    process_funding_data()

  expense_df <- read_excel(file_path, sheet = "Expense") %>%
    process_expense_data()

  showNotification("Data uploaded successfully.", type = "message", duration = 5)
  return(list(funding_sources = funding_sources_df, expense = expense_df))
}

process_funding_data <- function(df) {
  #' Read the dataframe, select and rename columns for the funding data
  #' 
  #' @param df: data frame read from the excel sheet
  #' @return: funding_sources_df: processed funding sources data frame
  #' Dataframe structure:
  #' - source_id: Character
  #' - funding_source: Character
  #' - allowed_categories: List of Character
  #' - valid_from: Date
  #' - valid_to: Date
  #' - amount: Numeric
  #' - notes: Character

  funding_sources_df <- df %>%
    select(`Source ID`, `Funding Source`, `Allowed Categories`, `Valid From`, `Valid To`, `Amount`, `Notes`) %>%
    setNames(nm = c("source_id", "funding_source", "allowed_categories", "valid_from", "valid_to", "amount", "notes")) %>%

    # Convert data types
    mutate(
      allowed_categories = strsplit(as.character(allowed_categories), ",\\s*"),
      valid_from = as.Date(valid_from),
      valid_to = as.Date(valid_to),
      amount = as.numeric(amount),
      source_id = as.character(source_id),
      notes = as.character(notes)
    ) %>%

    rowwise() %>%
    ungroup() %>% # Ungroup after rowwise operation
    
    # Remove rows with NA in source_id
    filter(!is.na(source_id))

  return(funding_sources_df)
}

process_expense_data <- function(df) {
  #' Read the dataframe, select and rename columns for the expense data
  #'
  #' @param df: data frame read from the excel sheet
  #' @return: expense_df: processed expense data frame
  #'
  #' Dataframe structure:
  #' - item_id: Character
  #' - expense_category: Character
  #' - planned_amount: Numeric
  #' - latest_payment_date: Date
  
  expense_df <- df %>%
    select(`Priority`,`Item ID`, `Expense Category`, `Planned Amount`, `Latest Payment Date`, `Notes`) %>%
    setNames(nm = c("priority", "item_id", "expense_category", "planned_amount", "latest_payment_date", "notes")) %>%

    # Convert data types
    mutate(
      latest_payment_date = as.Date(latest_payment_date),
      planned_amount = as.numeric(planned_amount),
      item_id = as.character(item_id)
    ) %>%

    # Add index
    mutate(old_index = row_number()) %>%

    # Remove rows with NA in item_id
    filter(!is.na(item_id))
    
  return(expense_df)
}
