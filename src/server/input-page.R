source("src/ui/input-page.R")
source("src/server/components/data-processing.R")

input_server <- function(input, output, session, values, current_view) {
  ## ---- EVENT: Upload Expenses and Funding Data ----
  observeEvent(input$spreadsheet_upload, {
    req(input$spreadsheet_upload)
    path <- input$spreadsheet_upload$datapath

    tryCatch(
      {
        data_list <- read_excel_data(path)
        funding_sources_df <- data_list$funding_sources
        expense_df <- data_list$expenses

        values$funding_sources <- funding_sources_df
        values$expenses <- expense_df

        showNotification(
          "Data saved successfully",
          type = "message",
          duration = 3
        )
      },
      error = function(e) {
        showNotification(
          paste("Upload failed:", e$message),
          type = "error",
          duration = 3,
          ignoreInit = FALSE
        )
      }
    )
  })
}