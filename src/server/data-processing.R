library(readxl)
library(dplyr)
library(tidyr)
library(shiny)

read_excel_data <- function(file_path, sheet_name) {
  # Read the excel file uploaded that contains funding and expense data
  #
  # Arguments:
  # file_path: path to the uploaded excel file
  # sheet_name: name of the sheet to read
  #
  # Return: List of the expense and funding dataframes


  funding_sources_df <- read_excel(file_path, sheet = "Funding") %>%
    #data_validation() %>%
    process_funding_data()

  expense_df <- read_excel(file_path, sheet = "Expense") %>%
    #data_validation() %>%
    process_expense_data()

  showNotification("Data uploaded successfully.", type = "message", duration = 5)
  return(list(funding_sources = funding_sources_df, expense = expense_df))
}

data_validation <- function(df) {
  # Validate the data frame for required columns and data types
  #
  # Arguments:
  # df: data frame to validate
  #
  # Returns:
  # is_valid: Boolean indicating if the data frame is valid

  if ("Funding" %in% df$sheet_name) {
    required_columns <- c("source_id", "allowed_categories", "valid_from", "valid_to", "amount")
  } else if ("Expense" %in% df$sheet_name) {
    required_columns <- c("item_id", "expense_category", "planned_amount", "latest_payment_date")
  } else {
    return(FALSE)
  }

  is_valid <- all(required_columns %in% names(df))
  if (!is_valid) {
    showNotification("Data validation failed: missing required columns.", type = "error", duration = NULL)
    stop("Data validation failed.")
    return(FALSE)
  }
  return(df)
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
    select(`Source ID`, `Allowed Categories`, `Valid From`, `Valid To`, `Amount`, `Notes`) %>%
    setNames(nm = c("source_id", "allowed_categories", "valid_from", "valid_to", "amount", "notes")) %>%

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
