

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

  # --- HANDLER: Download Allocation Report ---
  output$budget_download <- downloadHandler(
    filename = function() "allocation_report.xlsx",
    content = function(file) {
      # Check if allocation results are available
      if (!nrow(values$allocation_result) == 0 && !nrow(values$funding_summary) == 0) {
        showNotification("Preparing allocation report...", type = "message", duration = 2) 
        saveWorkbook(create_allocation_report_wb(values), file, overwrite = TRUE)
      } else {
        showNotification("No allocation results available to download.", type = "error", duration = 5)
        saveWorkbook(create_allocation_report_wb(values), file, overwrite = TRUE)
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
    expense_id = "Expense ID",
    expense_name = "Expense Name",
    expense_category = "Expense Category",
    planned_amount = "Planned Amount",
    latest_payment_date = "Latest Payment Date",
    notes = "Notes"
  )
  for (old_name in names(expense_name_map)) {
    if (old_name %in% names(export_expenses)) {
      names(export_expenses)[names(export_expenses) == old_name] <- expense_name_map[[old_name]]
    }
  }
  writeData(wb, "Expenses", export_expenses, withFilter = TRUE)
  if (ncol(export_expenses) > 0) {
    setColWidths(wb, "Expenses", cols = seq_len(ncol(export_expenses)), widths = "auto")
  }

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
  if (ncol(export_funding) > 0) {
    setColWidths(wb, "Funding", cols = seq_len(ncol(export_funding)), widths = "auto")
  }

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

write_data_to_excel <- function(wb_object, target_sheet, text, start_row, start_col, end_col = NULL, text_style = NULL, text_style2 = NULL) {
  # Function that writes and styles output for Excel file.
  #
  # Arguments: 
  # wb_object(Workbook) - Workbook object containing a worksheet
  # target_sheet(string) - The name of the worksheet to write to 
  # text(string) - The text to be written into the worksheet
  # start_row(integer) - The row for text to be written into
  # start_col(integer) - The first column for text to be written into
  # end_col(integer) - The last column for text style to be applied to (Optional)
  # text_style(style) - The 1st style of text to be applied (Optional)
  # text_style2(style) - The 2nd style of text to be applied (Optional)
  
  writeData(wb = wb_object, sheet = target_sheet, x = text, startRow = start_row, startCol = start_col)
  
  # Check for end column argument
  if(is.null(end_col)) {
    end_col_idx <- start_col
  } else {
    end_col_idx <- end_col
  }
  
  # Apply text styles if requested
  if(!is.null(text_style)) {
    addStyle(wb = wb_object, sheet = target_sheet, style = text_style, rows = start_row, cols = start_col:end_col_idx)
  }
  if(!is.null(text_style2)) {
    addStyle(wb = wb_object, sheet = target_sheet, style = text_style2, rows = start_row, cols = start_col:end_col_idx)
  }
}

# --- Function: Create Allocation Report Workbook ---
# Column Indices
ITEM_LABEL_COL <- 1
EXPENSE_AMOUNT_COL <- 4
ALLOCATED_AMOUNT_COL <- 5
# Excel Row indices
TITLE_SECTION <- 1
DATE_SECTION <- 3
MAIN_SECTION_HEADER <- 5

create_allocation_report_wb <- function(values) {
  style_header_title <- createStyle(fontSize = 16, textDecoration = "bold")
  style_header_border <- createStyle(border = "bottom", borderColour = "black", borderStyle = "medium", textDecoration = "bold")
  style_bold <- createStyle(textDecoration = "bold")
  style_italic <- createStyle(textDecoration = "italic")
  style_currency <- createStyle(numFmt = "$#,##0.00")
  style_grand_total <- createStyle(fontSize = 12, textDecoration = "bold")

  allocation_result <- values$allocation_result
  funding_summary <- values$funding_summary


  wb <- createWorkbook()
  
  # --- Allocation Result Sheet ---
  allocation_name_map <- c(
    expense_id = "Expense ID",
    source_id = "Source ID",
    expense_category = "Expense Category",
    expense_amount = "Expense Amount",
    allocated_amount = "Allocated Amount",
    latest_payment_date = "Latest Payment Date",
    allocation_status = "Allocation Status"
  )
  
  addWorksheet(wb, "Allocation Result")
  showGridLines(wb, "Allocation Result", showGridLines = FALSE)

  # Title
  write_data_to_excel(wb, "Allocation Result", "Allocation Result Summary", start_row = TITLE_SECTION, start_col = ITEM_LABEL_COL,
                      text_style = style_header_title)
  write_data_to_excel(wb, "Allocation Result", paste("Date: ", Sys.Date()), start_row = DATE_SECTION, start_col = ITEM_LABEL_COL,
                      text_style = style_bold)
  
  # Rename columns and output data
  for (old_name in names(allocation_name_map)) {
    if (old_name %in% names(allocation_result)) {
      names(allocation_result)[names(allocation_result) == old_name] <- allocation_name_map[[old_name]]
    }
  }
  addStyle(wb, "Allocation Result", style = style_currency, 
           rows = (MAIN_SECTION_HEADER + 2):(MAIN_SECTION_HEADER + 1 + nrow(allocation_result)), 
           cols = c(EXPENSE_AMOUNT_COL, ALLOCATED_AMOUNT_COL), gridExpand = TRUE)
  writeData(wb, "Allocation Result", allocation_result, withFilter = TRUE, startRow = MAIN_SECTION_HEADER + 1, startCol = ITEM_LABEL_COL,
            headerStyle = style_header_border)
  
  if (ncol(allocation_result) > 0) {
    last_col <- ITEM_LABEL_COL + ncol(allocation_result) - 1
    setColWidths(wb, "Allocation Result", cols = ITEM_LABEL_COL:last_col, widths = "auto")
  }

  
  # Funding Summary Sheet
  funding_name_map <- c(
    source_id = "Source ID",
    funding_source = "Funding Source",
    initial_amount = "Initial Amount",
    used_amount = "Used Amount",
    remaining_amount = "Remaining Amount"
  )

  addWorksheet(wb, "Funding Summary")
  showGridLines(wb, "Funding Summary", showGridLines = FALSE)

  # Rename columns and output data
  for (old_name in names(funding_name_map)) {
    if (old_name %in% names(funding_summary)) {
      names(funding_summary)[names(funding_summary) == old_name] <- funding_name_map[[old_name]]
    }
  }

  # Title
  write_data_to_excel(wb, "Funding Summary", "Funding Summary", start_row = TITLE_SECTION, start_col = ITEM_LABEL_COL,
                      text_style = style_header_title)
  write_data_to_excel(wb, "Funding Summary", paste("Date: ", Sys.Date()), start_row = DATE_SECTION, start_col = ITEM_LABEL_COL,
                      text_style = style_bold)
  addStyle(wb, "Funding Summary", style = style_currency, 
           rows = (MAIN_SECTION_HEADER + 2):(MAIN_SECTION_HEADER + 1 + nrow(funding_summary)), 
           cols = c(3,4,5), gridExpand = TRUE)

  writeData(wb, "Funding Summary", funding_summary, withFilter = TRUE, startRow = MAIN_SECTION_HEADER + 1, startCol = ITEM_LABEL_COL,
            headerStyle = style_header_border)
  if (ncol(funding_summary) > 0) {
    last_col <- ITEM_LABEL_COL + ncol(funding_summary) - 1
    setColWidths(wb, "Funding Summary", cols = ITEM_LABEL_COL:last_col, widths = "auto")
  }

  wb
}