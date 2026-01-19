

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

  # Rename columns and output
  colnames(export_expenses) <- c("Priority", "Item ID", "Expense Category", "Planned Amount", "Latest Payment Date", "Notes", "old_index")
  writeData(wb, "Expenses", export_expenses |> select(-old_index), withFilter = TRUE)

  colnames(export_funding) <- c("Source ID", "Allowed Categories", "Valid From", "Valid To", "Amount", "Notes")
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
      `Name` = character(),
      `Allowed categories` = character(),
      `Valid From` = character(),
      `Valid To` = character(),
      `Amount` = numeric(),
      `Probability` = numeric(),
      `Notes` = character(),
      check.names = FALSE
    )
  )
  wb
}