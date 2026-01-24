# injection to values to test for data validation
values <- reactiveValues(
  funding_sources = data.frame(
    source_id = c("FS-1", "FS-2"),
    funding_source = c("Source A", "Source B"),
    allowed_categories = c("Category 1", "Category 2"),
    valid_from = as.Date(c("2024-01-01", "2024-06-01")),
    valid_to = as.Date(c("2024-12-31", "2024-05-31")),  # Invalid date range for FS-2
    amount = c(10000, -5000),  # Negative amount for FS-2
    notes = c("Note A", "Note B")
  ),
  expenses = data.frame(
    priority = c(1, 2),
    expense_id = c("E-1", "E-2"),
    expense_name = c("Expense A", "Expense B"),
    expense_category = c("Category 1", ""),  # Invalid category for E-2
    planned_amount = c(2000, -1500),  # Negative planned amount for E-2
    latest_payment_date = as.Date(c("2024-03-15", "2024-04-20")),
    notes = c("Note A", "Note B")
  )
)