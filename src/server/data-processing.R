
read_excel_data <- function(file_path) {
  # Read the excel file uploaded that contains funding and expense data
  #
  # Arguments:
  # file_path: path to the uploaded excel file
  #
  # Return: List of the expense and funding dataframes
  funding_sources_df <- read_excel(file_path, sheet = "Funding") %>%
    process_funding_data()

  expense_df <- read_excel(file_path, sheet = "Expense") %>%
    process_expense_data()

  return(list(funding_sources = funding_sources_df, expense = expense_df))
}

process_funding_data <- function(df) {
  # Read the dataframe, select and rename columns for the funding data
  # Arguments:
  # df: data frame read from the excel sheet
  # Returns:
  # funding_sources_df: processed funding sources data frame
  #
  # Dataframe structure:
  # - source_id: Character
  # - allowed_categories: List of Character
  # - valid_from: Date
  # - valid_to: Date
  # - amount: Numeric

  funding_sources_df <- df %>%
    select(`Source ID`, `Allowed Categories`, `Valid From`, `Valid To`, `Amount`) %>%
    setNames(nm = c("source_id", "allowed_categories", "valid_from", "valid_to", "amount")) %>%

    # Convert data types
    mutate(
      valid_from = as.Date(valid_from),
      valid_to = as.Date(valid_to),
      amount = as.numeric(amount),
      source_id = as.character(source_id)
    ) %>%

    # Unnest categories
    mutate(
      allowed_categories = strsplit(as.character(allowed_categories), ",\\s*")
    ) %>%
    rowwise() %>%
    mutate(allowed_categories = list(allowed_categories)) %>%

    # Add index
    mutate(index = row_number()) %>%
    select(index, everything()) %>%
    ungroup() %>% # Ungroup after rowwise operation
    

    # Remove rows with NA in source_id
    filter(!is.na(source_id))

  return(funding_sources_df)
}

process_expense_data <- function(df) {
  # Read the dataframe, select and rename columns for the expense data
  #
  # Arguments:
  # df: data frame read from the excel sheet
  # Returns:
  # expense_df: processed expense data frame
  # 
  # Dataframe structure:
  # - item_id: Character
  # - expense_category: Character
  # - planned_amount: Numeric
  # - latest_payment_date: Date
  

  expense_df <- df %>%
    select(`Item ID`, `Expense Category`, `Planned Amount`, `Latest Payment Date`) %>%
    setNames(nm = c("item_id", "expense_category", "planned_amount", "latest_payment_date")) %>%
    
    # Convert data types
    mutate(
      latest_payment_date = as.Date(latest_payment_date),
      planned_amount = as.numeric(planned_amount),
      item_id = as.character(item_id)
    )

    # Add index
    mutate(index = row_number()) %>%
    select(index, everything()) %>%

    # Remove rows with NA in item_id
    filter(!is.na(item_id))
    
  return(expense_df)
}
