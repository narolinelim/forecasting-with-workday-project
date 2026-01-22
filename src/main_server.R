

# Contains all server components from server folder

source("src/server/allocation-algorithm.R")
source("src/server/data-processing.R")
source("src/server/io.R")
source("src/server/sorting.R")
source("src/server/graph.R")

main_server_logic <- function(input, output, session, values) {
  # Current page
  current_view <- reactiveVal("forecast")
  

  # --- EVENTS: Navigation between tabs ---
  observeEvent(input$dashboard_tab, current_view("dashboard"))
  observeEvent(input$forecast_tab, current_view("forecast"))
  observeEvent(input$funding_tab, current_view("funding"))
  observeEvent(input$expense_tab, current_view("expense"))

  # --- OUTPUT: Switching between tabs ---
  output$tab_content <- renderUI({
    switch(
      current_view(),
      "dashboard" = dashboard_ui(),
      "forecast" = forecast_ui(),
      "funding" = funding_ui(),
      "expense" = expense_ui()
    )
  })

  # --- OUTPUT: Generating Forecast Notification ---
  observe({
    showNotification(
      "Allocation Finished. Go to Dashboard for result.",
      duration = 3
    )
  }) |>
    bindEvent(input$generate_forecast)

  # --- EVENTS: Exit Session Popup ---
  observeEvent(input$exit_session, {
    showModal(
      tagAppendAttributes(
        modalDialog(
          title = "Exiting Session",
          "All data from this session will be deleted",
          easyClose = TRUE,
          footer = tagList(
            actionButton("return_to_session", "Return", class = "session-btn"),
            actionButton("end_session", "Exit Session", class = "session-btn")
          )
        ),
        class = "exit-session-popup"
      )
    )
  })

  # --- EVENT: Return to session ---
  observeEvent(input$return_to_session, {
    removeModal()
    current_view("dashboard")
  })

  # Render priority mode
  output$priority_card <- renderUI({
    if (isTruthy(input$select_priority) && input$select_priority == "Manual Priority") {
      manual_priority_ui()
    } else {
      column_priority_ui()
    }
  })

  # Render first priority
  output$first_priority <- renderUI({
    if (input$select_first_priority_item == "Payment Date") {
      payment_date_view()
    } else {
      categories_view()
    }
  })

  # Render second priority
  output$second_priority <- renderUI({
    if (input$select_second_priority_item == "Payment Date") {
      payment_date_view()
    } else if (input$select_second_priority_item == "Categories") {
      categories_view()
    } else if (input$select_second_priority_item == "None") {
      div("No second priority.", class = "no-second-priority")
    }
  })

  # Dragging feature for categories priority
  observeEvent(input$drag_categories, {
    input$drag_categories
  })

  # --- MANUAL ROW REORDERING LOGIC ---
  # Create proxy for table updates
  proxy <- dataTableProxy("sample_manual_table")

  # Observe row reordering events
  row_reorder(input, values, proxy, id_col = "priority")

  
  # --- EVENT: Upload Expenses and Funding Data ---
  observeEvent(input$spreadsheet_upload, {
    req(input$spreadsheet_upload)
    path <- input$spreadsheet_upload$datapath
    
    tryCatch({
      data_list <- read_excel_data(path)
      funding_sources_df <- data_list$funding_sources
      expense_df <- data_list$expense

      values$funding_sources <- funding_sources_df
      values$expenses <- expense_df
      showNotification("Data saved successfully", type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("Upload failed:", e$message), type = "error", duration = NULL)
    })
  })
  
  

  # ----------------------------
  # SORTING LOGIC
  # This section handles sorting of expenses based on user selection:
  # - Manual sorting (drag-and-drop order from UI)
  # - Sort by column (user-defined criteria and category order)
  # The output 'values$expenses_sorted' includes 'final_order' column
  # and will be used as input for the allocation algorithm
  # ----------------------------


  # ----------------------------
  # New logic for column sorting 0120
  # ----------------------------
  # 1. 动态构造排序规则列表
  current_ordering_rules <- reactive({
    list(
      p1_item        = input$select_first_priority_item,
      p1_date_dir    = input$`payment-date-options`,      # 目前 UI 只有一个全局日期方向
      p2_item        = input$select_second_priority_item,
      p2_date_dir    = input$`payment-date-options`,      # 同上
      category_order = input$drag_categories            # 获取拖拽后的类别顺序向量
    )
  })
  
  # 2. 监听任何排序条件的变化并触发排序
  # 监听 UI 变化并实时排序
  # observe({
  #   # 检查基础条件：数据已上传 且 UI 模式已选择
  #   req(values$expenses, input$select_priority)
  #   
  #   # 确定模式
  #   is_column_mode <- input$select_priority == "Column Priority"
  #   
  #   if (is_column_mode) {
  #     # 调用排序函数
  #     # 注意：直接传入 values$expenses，它里面自带了当前的 priority
  #     sorted_df <- main_sorting_expenses(
  #       expenses_data  = values$expenses,
  #       mode           = "by_rules",
  #       ordering_rules = current_ordering_rules()
  #     )
  #     
  #     # 重要：只有当新老顺序不同时才赋值，防止死循环
  #     # 或者直接赋值（values是reactiveValues，DT会自动重绘）
  #     values$expenses <- sorted_df
  #   }
  # }) %>% bindEvent(
  #   # 任何一个按钮动了，都会触发这里的代码
  #   input$select_priority,
  #   input$select_first_priority_item,
  #   input$select_second_priority_item,
  #   input$`payment-date-options`,
  #   input$drag_categories
  # )
  observe({
    # --- 调试：第一行先打印，确认 observer 活过来了 ---
    cat("\n[Server Signal] Observer Triggered! Mode:", input$select_priority, "\n")
    
    # 获取当前规则
    rules <- current_ordering_rules()
    
    # 构造或获取数据
    data_to_sort <- if(!is.null(values$expenses) && nrow(values$expenses) > 0) {
      values$expenses
    } else {
      # 假数据：确保这里的列名 item_id, expense_category 等和你的 sorting.R 对应
      data.frame(
        priority = 1:5,
        item_id = c("EXP001", "EXP002", "EXP003", "EXP004", "EXP005"),
        expense_category = c("Salary", "Travel", "Salary", "Equipment", "Cheese"),
        planned_amount = c(5000, 200, 4500, 1200, 50),
        latest_payment_date = as.Date(c("2024-03-01", "2024-01-15", "2024-02-10", "2024-01-15", "2024-03-15")),
        notes = c("", "Conf. in Sydney", "Monthly", "Laptop", "Kitchen"),
        stringsAsFactors = FALSE
      )
    }
    
    # 确定 mode
    # 增加 req 判断，如果 input 没拿到，默认给 manual 以防报错
    target_mode <- if(isTruthy(input$select_priority) && input$select_priority == "Column Priority") "by_rules" else "manual"
    
    # 执行排序
    sorted_result <- main_sorting_expenses(
      expenses_data = data_to_sort,
      mode = target_mode,
      ordering_rules = rules
    )
    
    # 只有在有真实数据时才写回 values，防止假数据污染
    if (!is.null(values$expenses) && nrow(values$expenses) > 0) {
      values$expenses <- sorted_result
    }
    
  }) %>% bindEvent(
    input$select_priority,
    input$select_first_priority_item,
    input$select_second_priority_item,
    input$`payment-date-options`, 
    input$drag_categories,
    # ignoreInit = FALSE 确保启动时就运行一次
    # ignoreNULL = FALSE 确保即便有些输入是空的也会运行
    ignoreInit = FALSE,
    ignoreNULL = FALSE
  )
  # Adding new funding form
  observeEvent(input$add_funding, {
    showModal(upload_funding_modal())
  })

  # Adding new expense form
  observeEvent(input$add_expense, {
    showModal(upload_expense_modal())
  })

  # Sample table outputs (for viewings only)
  # output$sample_budget_table <- renderDT({
  #   datatable(penguins)
  # })

  # output$sample_leftover_table <- renderDT({
  #   datatable(penguins)
  # })

  output$sample_funding_table <- renderDT({
    
    datatable(
      as.data.frame(penguins[1:5,]),
      extensions = "Buttons",
      options = list(
        dom = "<'row align-items-center'
                <'col-sm-6'l>
                <'col-sm-6 text-end'B>
              >
              <'row'<'col-sm-12'f>>
              t
              <'row'
                <'col-sm-5'i>
                <'col-sm-7'p>
              >",
        buttons = list(
          list(
            extend = "collection",
            className = "delete-row",
            text = "Delete row",
            action = DT::JS(
              "function (e,dt,node,config) {
                  dt.rows('.selected').remove().draw();
              }"
            )
          )
        )
      )
    )
  }, server = FALSE)
  

  output$sample_expense_table <- renderDT({
    datatable(
      values$expenses |> select(-old_index),
      extensions = "Buttons",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "<'row align-items-center'
                <'col-sm-6'l>
                <'col-sm-6 text-end'B>
              >
              <'row'<'col-sm-12'f>>
              t
              <'row'
                <'col-sm-5'i>
                <'col-sm-7'p>
              >",
        buttons = list(
          list(
            extend = "collection",
            className = "delete-row",
            text = "Delete row",
            action = DT::JS(
              "function (e,dt,node,config) {
                  dt.rows('.selected').remove().draw();
              }"
            )
          )
        )
      ),
      rownames = FALSE
    )
  }, server = FALSE)

  output$sample_manual_table <- renderDT({
    datatable(
      values$expenses |> select(-old_index),
      extensions = 'RowReorder',
      selection = 'none',
      callback = JS(row_reorder_callback),
      options = list(
        rowReorder = TRUE,
        pageLength = 100
      ),
      rownames = FALSE
    )
  })
  
  output$sample_priority_table <- renderDT({
    datatable(
      penguins
    )
  })

  # output$sample_table <- renderDT({
  #   datatable(
  #     values$expenses(),
  #     options = list(
  #       pageLength = 10,
  #       scrollY = "300px",
  #       scrollX = TRUE,
  #       dom = '<"row"<"col-sm-12"l>><"row"<"col-sm-12"f>>rtip'
  #     )
  #   )
  # })
}





