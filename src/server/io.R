

# Handles input and output
# Uploads, downloads and output schema (excel and reports)


main_output <- function(input, output, session, values) {

  # --- HANDLER: Download Excel Template ---
  output$download_template <- downloadHandler(
    filename = function() "budget_template.xlsx",
    content = function(file) {
      saveWorkbook(create_budget_template_wb(), file, overwrite = TRUE)
    }
  )

  # --- HANDLER: Download the Excel file with current data ---
  output$initial_download <- downloadHandler(
    filename = function() "budget_data.xlsx",
    content = function(file) {
      # Check if data is available
      if (!nrow(values$expenses) == 0 && !nrow(values$funding_sources) == 0) {
        showNotification("Preparing download...", type = "message", duration = 2) 
        saveWorkbook(input_excel_download(values), file, overwrite = TRUE)
      } else {
        showNotification("No data available to download.", type = "error", duration = 5)
        saveWorkbook(create_budget_template_wb(), file, overwrite = TRUE)
      }
    }
  )
}

# Helper functions:

# --- Function: Download current data as Excel file ---
input_excel_download <- function(values) {
  wb <- createWorkbook()

  # Create worksheets
  addWorksheet(wb, "Expenses")
  addWorksheet(wb, "Funding")

  # Temp dataframes for export
  export_expenses <- values$expenses
  export_funding <- values$funding_sources

  # Rename columns and output data
  expense_name_map <- c(
    priority = "Priority",
    item_id = "Item ID",
    expense_category = "Expense Category",
    planned_amount = "Planned Amount",
    latest_payment_date = "Latest Payment Date",
    notes = "Notes",
    old_index = "old_index"
  )
  for (old_name in names(expense_name_map)) {
    if (old_name %in% names(export_expenses)) {
      names(export_expenses)[names(export_expenses) == old_name] <- expense_name_map[[old_name]]
    }
  }
  writeData(wb, "Expenses", export_expenses |> select(-old_index), withFilter = TRUE)

  funding_name_map <- c(
    source_id = "Source ID",
    funding_source = "Funding Source",
    allowed_categories = "Allowed Categories",
    valid_from = "Valid From",
    valid_to = "Valid To",
    amount = "Amount",
    notes = "Notes"
  )
  for (old_name in names(funding_name_map)) {
    if (old_name %in% names(export_funding)) {
      names(export_funding)[names(export_funding) == old_name] <- funding_name_map[[old_name]]
    }
  }
  writeData(wb, "Funding", export_funding, withFilter = TRUE)

  wb
}

# --- Function: Create Excel template workbook ---
create_budget_template_wb <- function() {
  wb <- createWorkbook()
  addWorksheet(wb, "Expenses")
  writeData(
    wb,
    "Expenses",
    data.frame(
      `Priority`= integer(),
      `Item ID` = character(),
      `Expense Category` = character(),
      `Planned Amount` = numeric(),
      `Latest Payment Date` = character(),
      `Notes` = character(),
      check.names = FALSE
    )
  )
  addWorksheet(wb, "Funding")
  writeData(
    wb,
    "Funding",
    data.frame(
      `Source ID` = character(),
      `Funding Source` = character(),
      `Allowed categories` = character(),
      `Valid From` = character(),
      `Valid To` = character(),
      `Amount` = numeric(),
      `Notes` = character(),
      check.names = FALSE
    )
  )
  wb
}