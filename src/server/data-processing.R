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
  return(list(funding_sources = funding_sources_df, expenses = expense_df))
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
      priority = as.integer(priority),
      expense_id = as.character(expense_id),
      expense_name = as.character(expense_name),
      expense_category = as.character(expense_category),
      planned_amount = as.numeric(planned_amount),
      latest_payment_date = as.Date(latest_payment_date),
      notes = as.character(notes)
    ) %>%

    # Remove rows with NA in expense_id
    filter(!is.na(expense_id))
    
  return(expense_df)
}

# Data validation functions
data_validation <- function(df, type) {
  
  errors <- c()

  # Validate funding sources
  if (type == "funding") {
    funding_sources <- df
    invalid_funding_dates <- funding_sources %>%
      filter(!is.na(valid_from) & !is.na(valid_to) & valid_from > valid_to)
    if (nrow(invalid_funding_dates) > 0) {
      errors <- c(errors, "Error: Some funding sources have 'valid_from' date later than 'valid_to' date.")
    }

    required_funding_columns <- c("source_id", "funding_source", "allowed_categories", "amount", "valid_from", "valid_to")
    for (column in required_funding_columns) {
      if (any(is.na(funding_sources[[column]]))) {
        errors <- c(errors, paste("Error: Funding column", column, "contains missing values."))
      }
    }

    # Check for duplicate funding sources
    duplicate_funding_ids <- funding_sources %>%
      group_by(source_id) %>%
      filter(n() > 1) %>%
      distinct(source_id)

    if (nrow(duplicate_funding_ids) > 0) {
      errors <- c(errors, "Error: Duplicate funding source IDs found.")
    }

    # Check for negative amounts in funding sources
    if (any(funding_sources$amount < 0, na.rm = TRUE)) {
      errors <- c(errors, "Error: Negative amounts found in funding sources.")
    }
  } 

  # Validate expenses
  else if (type == "expense") {
    expenses <- df

    required_expense_columns <- c("expense_id", "expense_name", "expense_category", "planned_amount", "latest_payment_date", "priority")
    for (column in required_expense_columns) {
      if (any(is.na(expenses[[column]]))) {
        errors <- c(errors, paste("Error: Expense column", column, "contains missing values."))
      }
    }

    # Check for duplicate expense IDs
    duplicate_expense_ids <- expenses %>%
      group_by(expense_id) %>%
      filter(n() > 1) %>%
      distinct(expense_id)
    if (nrow(duplicate_expense_ids) > 0) {
      errors <- c(errors, "Error: Duplicate expense IDs found.")
    }

    # Check for negative amounts in expenses
    if (any(expenses$planned_amount < 0, na.rm = TRUE)) {
      errors <- c(errors, "Error: Negative planned amounts found in expenses.")
    }
  }
  
  return(errors)
}