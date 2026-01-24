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
    process_funding_data()

  expense_df <- read_excel(file_path, sheet = "Expense") %>%
    process_expense_data()

  showNotification("Data uploaded successfully.", type = "message", duration = 5)
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
  # Read the dataframe, select and rename columns for the expense data
  #
  # Arguments:
  # df: data frame read from the excel sheet
  # Returns:
  # expense_df: processed expense data frame
  # 
  # Dataframe structure:
  # - expense_id: Character
  # - expense_category: Character
  # - planned_amount: Numeric
  # - latest_payment_date: Date
  

  expense_df <- df %>%
    select(`Priority`, `Expense ID`, `Expense Name`, `Expense Category`, `Planned Amount`, `Latest Payment Date`, `Notes`) %>%
    setNames(nm = c("priority", "expense_id", "expense_name", "expense_category", "planned_amount", "latest_payment_date", "notes")) %>%

    # Convert data types
    mutate(
      latest_payment_date = as.Date(latest_payment_date),
      planned_amount = as.numeric(planned_amount),
      expense_id = as.character(expense_id)
    ) %>%

    # Remove rows with NA in expense_id
    filter(!is.na(expense_id))
    
  return(expense_df)
}

# Data validation functions
data_validation <- function(data) {
  # validate for the values dataframe
  # this will be called at all time to check at any point the data is invalid

  funding_sources <- data$funding_sources
  expenses <- data$expenses

  # For funding sources

  # valid from should be before valid to
  invalid_funding_dates <- funding_sources %>%
    filter(!is.na(valid_from) & !is.na(valid_to) & valid_from > valid_to)
  if (nrow(invalid_funding_dates) > 0) {
    showNotification("Error: Some funding sources have invalid date ranges.", type = "error", duration = NULL)
  }

  # amount should be non-negative
  invalid_funding_amounts <- funding_sources %>%
    filter(!is.na(amount) & amount < 0)
  if (nrow(invalid_funding_amounts) > 0) {
    showNotification("Error: Some funding sources have negative amounts.", type = "error", duration = NULL)
  }

  # For expenses

  # valid categories should be non-empty
  invalid_expense_categories <- expenses %>%
    filter(is.na(expense_category) | expense_category == "")
  if (nrow(invalid_expense_categories) > 0) {
    showNotification("Error: Some expenses have invalid categories.", type = "error", duration = NULL)
  }

  # planned amount should be non-negative
  invalid_expense_amounts <- expenses %>%
    filter(!is.na(planned_amount) & planned_amount < 0)
  if (nrow(invalid_expense_amounts) > 0) {
    showNotification("Error: Some expenses have negative planned amounts.", type = "error", duration = NULL)
  }

  # if no errors, return TRUE
  if (nrow(invalid_funding_dates) == 0 && nrow(invalid_funding_amounts) == 0 &&
      nrow(invalid_expense_categories) == 0 && nrow(invalid_expense_amounts) == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}