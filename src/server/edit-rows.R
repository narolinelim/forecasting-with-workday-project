add_funding_button <- function(input, values) {
  #' Add a new funding source row to the funding_sources dataframe
  #' 
  #' @param input: Shiny input object
  #' @param values: reactiveValues containing funding_sources dataframe

  new_row <- data.frame(
    source_id = NA,  # Placeholder, will be assigned after adding the row
    funding_source = if (is.null(input$source_name_input)) NA else input$source_name_input,
    allowed_categories = if (is.null(input$add_allowed_categories)) NA else I(list(tolower(as.character(input$add_allowed_categories)))),
    valid_from = if (is.null(input$valid_from_date)) NA else as.Date(input$valid_from_date),
    valid_to = if (is.null(input$valid_to_date)) NA else as.Date(input$valid_to_date),
    amount = if (is.null(input$funding_amount)) NA else as.numeric(input$funding_amount),
    notes = if (is.null(input$funding_note)) NA else input$funding_note,
    stringsAsFactors = FALSE
  )

  must_have <- c("funding_source", "allowed_categories", "valid_from", "valid_to", "amount")
  for (col in must_have) {
    val <- new_row[[col]]
    if (any(is.na(val))) {
      showNotification("Missing required field", type = "error", duration = 5)
      return(NULL)
    }
  }

  values$funding_sources <- rbind(values$funding_sources, new_row)
  values$funding_sources$source_id <- paste0("FS-", seq_len(nrow(values$funding_sources)))
}

add_expense_button <- function(input, values) {
  #' Add a new expense row to the expenses dataframe
  #' 
  #' @param input: Shiny input object
  #' @param values: reactiveValues containing expenses dataframe

  new_row <- data.frame(
    priority = nrow(values$expenses) + 1,
    expense_id = NA,  # Placeholder, will be assigned after adding the row
    expense_name = if (is.null(input$expense_name_input)) NA else input$expense_name_input,
    expense_category = if (is.null(input$expense_type)) NA else tolower(input$expense_type),
    planned_amount = if (is.null(input$expense_amount)) NA else as.numeric(input$expense_amount),
    latest_payment_date = if (is.null(input$latest_payment_date)) NA else as.Date(input$latest_payment_date),
    notes = if (is.null(input$expense_note)) NA else input$expense_note
  )

  must_have <- c("expense_name", "expense_category", "planned_amount", "latest_payment_date")
  for (col in must_have) {
    val <- new_row[[col]]
    if (any(is.na(val))) {
      showNotification("Missing required field", type = "error", duration = 5)
      return(NULL)
    }
  }

  values$expenses <- rbind(values$expenses, new_row)
  values$expenses$expense_id <- paste0("E-", seq_len(nrow(values$expenses)))
}

delete_row <- function(df, selected_rows) {
  #' Delete selected rows from the dataframe
  #'
  #' @param df: data frame from which rows will be deleted
  #' @param selected_rows: vector of row indices to be deleted
  #' 
  #' @return: updated data frame after deletion

  if (length(selected_rows) > 0 && all(selected_rows %in% seq_len(nrow(df)))) {
    df <- df[-selected_rows, ]
    
    if ("priority" %in% colnames(df)) {
      # Reassign priority values after deletion
      df$priority <- seq_len(nrow(df))
    }

    showNotification(paste("Deleted", length(selected_rows), "row(s)."), type = "message")
  } else {
    showNotification("No rows selected or invalid selection.", type = "error")
  }
  return(df)
}