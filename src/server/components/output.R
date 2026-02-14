# ---- Main output ----
main_output <- function(input, output, session, values) {
  #' Function for downloading excel files

  # ---- HANDLER: Download Excel Template ----
  output$download_template <- downloadHandler(
    filename = function() "budget_template.xlsx",
    content = function(file) {
      saveWorkbook(create_budget_template_wb(), file, overwrite = TRUE)
    }
  )
  
  # ---- HANDLER: Download a filled Excel file for demo ----
  output$download_sample_spreadsheet <- downloadHandler(
    filename = function() "filled_spreadsheet_template.xlsx",
    content = function(file) {
      file_path <- "test/complex_test_case_1.xlsx"
      file.copy(file_path, file)
    }
  )

  # ---- HANDLER: Download the Excel file with current data ----
  output$download_excel <- downloadHandler(
    filename = function() "current_budget_data.xlsx",
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

  # ---- HANDLER: Download Allocation Report ----
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


# ---- Helper functions: ----

input_excel_download <- function(values) {
  #' Download current data as Excel file
  #'
  #' @param values: reactiveValues containing current data
  #' 
  #' @return: wb: Excel workbook object 

  wb <- createWorkbook()

  # Create worksheets
  addWorksheet(wb, "Funding")
  addWorksheet(wb, "Expense")

  # Temp dataframes for export
  export_expenses <- values$expenses %>%
    select(priority, expense_name, expense_category, planned_amount, latest_payment_date, notes)
  export_funding <- values$funding_sources %>%
    select(funding_source, allowed_categories, valid_from, valid_to, amount, notes)

  # Rename columns and output data
  expense_name_map <- c(
    priority = "Priority",
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
  writeData(wb, "Expense", export_expenses, withFilter = TRUE)
  if (ncol(export_expenses) > 0) {
    setColWidths(wb, "Expense", cols = seq_len(ncol(export_expenses)), widths = "auto")
  }

  funding_name_map <- c(
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

  return(wb)
}

create_budget_template_wb <- function() {
  #' Create Excel template workbook
  #' 
  #' @return: wb: Excel workbook object with template structure
  
  wb <- createWorkbook()
  addWorksheet(wb, "Funding")
  writeData(
    wb,
    "Funding",
    data.frame(
      `Funding Source` = character(),
      `Allowed Categories` = character(),
      `Valid From` = character(),
      `Valid To` = character(),
      `Amount` = numeric(),
      `Notes` = character(),
      check.names = FALSE
    )
  )
  addWorksheet(wb, "Expense")
  writeData(
    wb,
    "Expense",
    data.frame(
      `Priority`= integer(),
      `Expense Name` = character(),
      `Expense Category` = character(),
      `Planned Amount` = numeric(),
      `Latest Payment Date` = character(),
      `Notes` = character(),
      check.names = FALSE
    )
  )
  return(wb)
}

write_data_to_excel <- function(wb_object, target_sheet, text, start_row, start_col, end_col = NULL, text_style = NULL, text_style2 = NULL) {
  #' Function that writes and styles output for Excel file.
  #' 
  #' @param wb_object(Workbook) - Workbook object containing a worksheet
  #' @param target_sheet(string) - The name of the worksheet to write to 
  #' @param text(string) - The text to be written into the worksheet
  #' @param start_row(integer) - The row for text to be written into
  #' @param start_col(integer) - The first column for text to be written into
  #' @param end_col(integer) - The last column for text style to be applied to (Optional)
  #' @param text_style(style) - The 1st style of text to be applied (Optional)
  #' @param text_style2(style) - The 2nd style of text to be applied (Optional)

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

# Column Indices
ITEM_LABEL_COL <- 1
EXPENSE_AMOUNT_COL <- 4
ALLOCATED_AMOUNT_COL <- 5
# Excel Row indices
TITLE_SECTION <- 1
DATE_SECTION <- 3
MAIN_SECTION_HEADER <- 5

create_allocation_report_wb <- function(values) {
  #' Create Excel workbook for allocation report
  #' 
  #' @param values: reactiveValues containing allocation results and funding summary
  #' 
  #' @return: wb: Excel workbook object with allocation report structure

  # ---- Quick Styles ----
  style_header_title <- createStyle(fontSize = 16, textDecoration = "bold")
  style_header_border <- createStyle(border = "bottom", borderColour = "black", borderStyle = "medium", textDecoration = "bold")
  style_bold <- createStyle(textDecoration = "bold")
  style_italic <- createStyle(textDecoration = "italic")
  style_currency <- createStyle(numFmt = "$#,##0.00")
  style_grand_total <- createStyle(fontSize = 12, textDecoration = "bold")
  
  # ---- Data from values ----
  allocation_result <- values$full_budget_allocation_df
  allocation_result <- allocation_result %>%
    mutate(
      latest_payment_date = format(as.Date(latest_payment_date), "%d-%m-%Y")
    ) %>%
    select(
      source_id,
      expense_id,
      expense_category,
      allocated_amount,
      planned_amount,
      latest_payment_date,
      status
    )
  funding_summary <- values$funding_summary
  funding_sources <- values$funding_sources
  expenses <- values$expenses

  wb <- createWorkbook()

  # ---- Allocation Result Sheet ----
  allocation_name_map <- c(
    source_id = "Source ID",
    expense_id = "Expense ID",
    expense_category = "Expense Category",
    allocated_amount = "Allocated Amount",
    planned_amount = "Expense Amount",
    latest_payment_date = "Latest Payment Date",
    status = "Allocation Status"
  )
  
  addWorksheet(wb, "Allocation Result")
  showGridLines(wb, "Allocation Result", showGridLines = FALSE)

  ## ---- Title ----
  write_data_to_excel(wb, "Allocation Result", "Allocation Result Summary", start_row = TITLE_SECTION, start_col = ITEM_LABEL_COL,
                      text_style = style_header_title)
  write_data_to_excel(wb, "Allocation Result", paste("Date: ", Sys.Date()), start_row = DATE_SECTION, start_col = ITEM_LABEL_COL,
                      text_style = style_bold)

  ## ---- Rename columns and output data ----
  for (old_name in names(allocation_name_map)) {
    if (old_name %in% names(allocation_result)) {
      names(allocation_result)[names(allocation_result) == old_name] <- allocation_name_map[[old_name]]
    }
  }

  ## ---- Data Output ----
  addStyle(wb, "Allocation Result", style = style_currency, 
           rows = (MAIN_SECTION_HEADER + 2):(MAIN_SECTION_HEADER + 1 + nrow(allocation_result)), 
           cols = c(EXPENSE_AMOUNT_COL, ALLOCATED_AMOUNT_COL), gridExpand = TRUE)
  writeData(wb, "Allocation Result", allocation_result, withFilter = TRUE, startRow = MAIN_SECTION_HEADER + 1, startCol = ITEM_LABEL_COL,
            headerStyle = style_header_border)
  
  if (ncol(allocation_result) > 0) {
    last_col <- ITEM_LABEL_COL + ncol(allocation_result) - 1
    setColWidths(wb, "Allocation Result", cols = ITEM_LABEL_COL + 1:last_col, widths = "auto")
  }


  # ---- Funding Summary Sheet ----
  funding_name_map <- c(
    source_id = "Source ID",
    funding_source = "Funding Source",
    initial_amount = "Initial Amount",
    used_amount = "Used Amount",
    remaining_amount = "Remaining Amount"
  )

  addWorksheet(wb, "Funding Summary")
  showGridLines(wb, "Funding Summary", showGridLines = FALSE)

  ## ---- Rename columns and output data ----
  for (old_name in names(funding_name_map)) {
    if (old_name %in% names(funding_summary)) {
      names(funding_summary)[names(funding_summary) == old_name] <- funding_name_map[[old_name]]
    }
  }

  ## ---- Title ----
  write_data_to_excel(wb, "Funding Summary", "Funding Summary", start_row = TITLE_SECTION, start_col = ITEM_LABEL_COL,
                      text_style = style_header_title)
  write_data_to_excel(wb, "Funding Summary", paste("Date: ", Sys.Date()), start_row = DATE_SECTION, start_col = ITEM_LABEL_COL,
                      text_style = style_bold)
  
  ## ---- Data Output ----
  addStyle(wb, "Funding Summary", style = style_currency, 
           rows = (MAIN_SECTION_HEADER + 2):(MAIN_SECTION_HEADER + 1 + nrow(funding_summary)), 
           cols = c(3,4,5), gridExpand = TRUE)

  writeData(wb, "Funding Summary", funding_summary, withFilter = TRUE, startRow = MAIN_SECTION_HEADER + 1, startCol = ITEM_LABEL_COL,
            headerStyle = style_header_border)
  if (ncol(funding_summary) > 0) {
    last_col <- ITEM_LABEL_COL + ncol(funding_summary) - 1
    setColWidths(wb, "Funding Summary", cols = ITEM_LABEL_COL:last_col, widths = "auto")
  }

  # ---- Funding Input Sheet ----
  display_funding_names <- c(
    source_id = "Source ID",
    funding_source = "Funding Source",
    allowed_categories = "Allowed Categories",
    valid_from = "Valid From",
    valid_to = "Valid To",
    amount = "Amount",
    notes = "Notes"
  )

  addWorksheet(wb, "Funding")
  showGridLines(wb, "Funding", showGridLines = FALSE)

  ## ---- Title ----
  write_data_to_excel(wb, "Funding", "Funding Sources", start_row = TITLE_SECTION, start_col = ITEM_LABEL_COL,
                      text_style = style_header_title)
  
  ## ---- Rename columns and output data ----
  for (old_name in names(display_funding_names)) {
    if (old_name %in% names(funding_sources)) {
      names(funding_sources)[names(funding_sources) == old_name] <- display_funding_names[[old_name]]
    }
  }
  
  ## ---- Data Output ----
  addStyle(wb, "Funding", style = style_currency, 
           rows = (MAIN_SECTION_HEADER + 2):(MAIN_SECTION_HEADER + 1 + nrow(funding_sources)), 
           cols = c(5), gridExpand = TRUE)
  writeData(wb, "Funding", funding_sources, withFilter = TRUE, startRow = MAIN_SECTION_HEADER + 1, startCol = ITEM_LABEL_COL,
            headerStyle = style_header_border)
  if (ncol(funding_sources) > 0) {
    last_col <- ITEM_LABEL_COL + ncol(funding_sources) - 1
    setColWidths(wb, "Funding", cols = ITEM_LABEL_COL:last_col, widths = "auto")
  }

  # ---- Expenses Sheet ----
  display_expense_names <- c(
    priority = "Priority",
    expense_id = "Expense ID",
    expense_name = "Expense Name",
    expense_category = "Expense Category",
    planned_amount = "Planned Amount",
    latest_payment_date = "Latest Payment Date",
    notes = "Notes"
  )

  addWorksheet(wb, "Expenses")
  showGridLines(wb, "Expenses", showGridLines = FALSE)

  ## ---- Title ----
  write_data_to_excel(wb, "Expenses", "Expenses", start_row = TITLE_SECTION, start_col = ITEM_LABEL_COL,
                      text_style = style_header_title)


  ## ---- Rename columns and output data ----
  for (old_name in names(display_expense_names)) {
    if (old_name %in% names(expenses)) {
      names(expenses)[names(expenses) == old_name] <- display_expense_names[[old_name]]
    }
  }

  ## ---- Data Output ----
  addStyle(wb, "Expenses", style = style_currency,
           rows = (MAIN_SECTION_HEADER + 2):(MAIN_SECTION_HEADER + 1 + nrow(expenses)),
           cols = c(5), gridExpand = TRUE)
  writeData(wb, "Expenses", expenses, withFilter = TRUE, startRow = MAIN_SECTION_HEADER + 1, startCol = ITEM_LABEL_COL,
            headerStyle = style_header_border)
  if (ncol(expenses) > 0) {
    last_col <- ITEM_LABEL_COL + ncol(expenses) - 1
    setColWidths(wb, "Expenses", cols = ITEM_LABEL_COL:last_col, widths = "auto")
  }

  return(wb)
}