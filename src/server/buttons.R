add_funding_button <- function(input, output, session, values) {
  new_row <- data.frame(
    source_id = paste0("FS-", nrow(values$funding_sources) + 1),
    allowed_categories = input$add_allowed_categories,
    valid_from = as.Date(input$valid_from_date),
    valid_to = as.Date(input$valid_to_date),
    amount = as.numeric(input$funding_amount),
    notes = input$funding_note
  )

  values$funding_sources <- rbind(values$funding_sources, new_row)
}

add_expense_button <- function(input, output, session, values) {
  new_row <- data.frame(
    priority = nrow(values$expenses) + 1,
    item_id = paste0("E-", nrow(values$expenses) + 1),
    expense_category = input$expense_type,
    planned_amount = as.numeric(input$expense_amount),
    latest_payment_date = as.Date(input$latest_payment_date),
    notes = input$expense_note,
    old_index = nrow(values$expenses) + 1
  )

  values$expenses <- rbind(values$expenses, new_row)
}