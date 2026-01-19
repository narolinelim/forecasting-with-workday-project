library(shiny)
library(openxlsx)
library(DT)

TEMPLATE_PATH <- file.path("templates", "budget_template.xlsx")

create_budget_template_wb <- function() {
  wb <- createWorkbook()
  addWorksheet(wb, "Expenses")
  writeData(wb, "Expenses", data.frame(
    `Item ID`=character(), `Allowed categories`=character(), `Amount`=numeric(),
    `Latest Payment Date`=character(), `Notes`=character(), check.names = FALSE
  ))
  addWorksheet(wb, "Funding")
  writeData(wb, "Funding", data.frame(
    `Source ID`=character(), `Name`=character(), `Allowed categories`=character(),
    `Valid From`=character(), `Valid To`=character(), `Amount`=numeric(),
    `Probability`=numeric(), `Notes`=character(), check.names = FALSE
  ))
  wb
}

server <- function(input, output, session) {
  
  expenses_data <- reactiveVal(NULL)
  funding_data  <- reactiveVal(NULL)
  allocation_data <- reactiveVal(NULL)
  
  output$download_template <- downloadHandler(
    filename = function() "budget_template.xlsx",
    content = function(file) {
      if (file.exists(TEMPLATE_PATH)) file.copy(TEMPLATE_PATH, file)
      else saveWorkbook(create_budget_template_wb(), file, overwrite = TRUE)
    }
  )
  
  observeEvent(input$spreadsheet_upload, {
    req(input$spreadsheet_upload)
    path <- input$spreadsheet_upload$datapath
    sheets <- getSheetNames(path)
    
    if (!all(c("Expenses","Funding") %in% sheets)) {
      showNotification("Upload failed: use the template (needs Expenses + Funding sheets).", type="error", duration=NULL)
      expenses_data(NULL); funding_data(NULL); allocation_data(NULL)
      return(NULL)
    }
    
    exp_df <- read.xlsx(path, sheet="Expenses")
    fund_df <- read.xlsx(path, sheet="Funding")
    
    exp_needed  <- c("Item ID","Allowed categories","Amount","Latest Payment Date","Notes")
    fund_needed <- c("Source ID","Name","Allowed categories","Valid From","Valid To","Amount","Probability","Notes")
    
    if (!all(exp_needed %in% names(exp_df)) || !all(fund_needed %in% names(fund_df))) {
      showNotification("Upload failed: columns don’t match template.", type="error", duration=NULL)
      expenses_data(NULL); funding_data(NULL); allocation_data(NULL)
      return(NULL)
    }
    
    expenses_data(exp_df); funding_data(fund_df); allocation_data(NULL)
    showNotification("Upload successful", type="message", duration=3)
  })
  
  observeEvent(input$generate_forecast, {
    req(expenses_data(), funding_data())
    exp <- expenses_data()
    planned <- suppressWarnings(as.numeric(exp[["Amount"]]))
    planned[is.na(planned)] <- 0
    
    allocation_data(data.frame(
      `Item ID`=exp[["Item ID"]],
      `Allowed categories`=exp[["Allowed categories"]],
      `Planned Amount`=planned,
      `Covered Amount`=0,
      `Source ID`=NA,
      `Shortfall Amount`=planned,
      `Shortfall %`=ifelse(planned==0,0,1),
      `Latest Payment Date`=exp[["Latest Payment Date"]],
      check.names = FALSE
    ))
    showNotification("Forecast generated", type="message", duration=3)
  })
  
  output$initial_download <- downloadHandler(
    filename = function() "expenses.xlsx",
    content = function(file) {
      wb <- createWorkbook(); addWorksheet(wb, "Expenses")
      writeData(wb, "Expenses", if (is.null(expenses_data())) data.frame() else expenses_data())
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$budget_download <- downloadHandler(
    filename = function() "budget_allocation.xlsx",
    content = function(file) {
      req(allocation_data())
      wb <- createWorkbook(); addWorksheet(wb, "Budget Allocation")
      writeData(wb, "Budget Allocation", allocation_data())
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$sample_expense_table <- DT::renderDT({ expenses_data() %||% data.frame() }, rownames=FALSE)
  output$sample_funding_table <- DT::renderDT({ funding_data() %||% data.frame() }, rownames=FALSE)
}

`%||%` <- function(a, b) if (is.null(a)) b else a


