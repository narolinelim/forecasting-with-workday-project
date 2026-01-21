

library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)
library(circlize)


# Funding Source
sources <- data.frame(
  ID = c("FS001", "FS002", "FS003", "FS004", "FS005", "FS006", "FS007", "FS008", "FS009", "FS010"),
  Categories = I(list(
    c("Salary"), c("Equipment"), c("Travel"), c("Salary", "Travel"), 
    c("Equipment", "Travel"), c("Salary"), c("Equipment"), c("Travel"), 
    c("Salary", "Equipment"), c("Salary", "Equipment", "Travel")
  )),
  ValidFrom = c("01/02/2025", "01/02/2025", "01/03/2025", "01/02/2025", "01/04/2025", 
                "01/06/2025", "01/01/2025", "01/05/2025", "01/07/2025", "01/02/2025"),
  ValidTo = c("30/06/2025", "31/08/2025", "30/09/2025", "31/12/2025", "31/10/2025", 
              "31/12/2025", "31/12/2025", "30/11/2025", "31/12/2025", "31/12/2025"),
  Amount = c(15000, 12000, 8000, 20000, 10000, 18000, 36000, 5000, 14000, 10000)
)

# Expense Data Frame
expenses <- data.frame(
  ID = c("E009", "E014", "E015", "E013", "E001", "E002", "E003", "E004", "E005", 
         "E006", "E007", "E008", "E010", "E011", "E012"),
  Category = c("Travel", "Equipment", "Travel", "Salary", "Salary", "Equipment", 
               "Travel", "Salary", "Equipment", "Travel", "Salary", "Equipment", 
               "Salary", "Equipment", "Travel"),
  Amount = c(6000, 20000, 15000, 10000, 5000, 8000, 3000, 12000, 15000, 4000, 
             8000, 10000, 15000, 12000, 5000),
  Date = c("10/08/2025", "20/12/2025", "25/12/2025", "01/01/2025", "15/02/2025", 
           "20/02/2025", "10/03/2025", "15/04/2025", "20/05/2025", "10/06/2025", 
           "15/07/2025", "20/07/2025", "15/09/2025", "20/10/2025", "10/11/2025")
)

total_balance <- sum(sources$Amount)

ordered_allocation <- data.frame(
  ExpenseID = c("E013", "E003", "E006", "E009",
                "E012", "E001", "E004", "E007",
                "E010", "E002", "E005", "E008",
                "E011", "E014", "E015"),
  Category = c("Salary", "Travel", "Travel", "Travel",
               "Travel", "Salary", "Salary", "Salary",
               "Salary", "Equipment", "Equipment", "Equipment",
               "Equipment", "Equipment", "Travel"),
  Amount = c(10000, 3000, 4000, 6000, 
             5000, 5000, 12000, 8000, 
             15000, 8000, 15000, 10000,
             12000, 20000, 15000),
  Allocated = c(0, 3000, 4000, 6000, 
                5000, 5000, 12000, 8000,
                10000, 8000, 15000, 10000,
                12000, 20000, 15000),
  Date = as.Date(c("2025-01-01","2025-03-10","2025-06-10","2025-08-10",
           "2025-10-11","2025-02-15","2025-04-15","2025-07-15",
           "2025-09-15","2025-02-20","2025-05-20","2025-07-20",
           "2025-10-20","2025-12-20","2025-12-25")),
  isFilled = c("FALSE", "TRUE", "TRUE", "TRUE",
               "TRUE", "TRUE", "TRUE", "TRUE",
               "FALSE", "TRUE", "TRUE", "TRUE",
               "TRUE", "TRUE", "TRUE")
)


# SHORTFALL PLOT

# Three potential plots:
# - bar chart
# - step line graph
# - stacked bar chart


# Mock dataframe

"
Y axis: total shortfall amount
X axis: timeline in weeks or months

"

create_shortfall_bar <- function() {
  
  date_ordered_allocation <- ordered_allocation[order(ordered_allocation$Date),]
  
  date_ordered_allocation$shortfall <- date_ordered_allocation$Allocated - date_ordered_allocation$Amount
  
  expense_shortfall <- date_ordered_allocation %>%
    filter(shortfall < 0) %>%
    mutate(StartMonth = floor_date(Date, "month"))
  
  months <- seq(
    from = floor_date(min(date_ordered_allocation$Date), "month"),
    to = floor_date(max(date_ordered_allocation$Date), "month"),
    by = "1 month"
  )
  
  monthly_shortfall <- expense_shortfall %>%
    rowwise() %>%
    mutate(Month = list(months[months >= StartMonth])) %>%
    unnest(Month) %>%
    ungroup() %>%
    group_by(Month) %>%
    summarise(
      TotalShortfall = sum(shortfall),
      NumberOfShortfalls = n(),
      .groups = "drop"
    )
  
  total_shortfalls <- tail(monthly_shortfall$NumberOfShortfalls, n = 1)
  
  
  shortfall_number_bar <- plot_ly(
    data = monthly_shortfall,
    x = ~Month,
    y = ~NumberOfShortfalls,
    type = "bar",
    name = "Number of Shortfalls",
    hovertemplate = paste(
      "Month: %{x|%b %Y}<br>",
      "Number of Shortfalls: %{y}<extra></extra>"
    )
  ) %>%
    layout(
      yaxis = list(title = "Number of Shortfalls"),
      xaxis = list(showticklabels = FALSE)
    )
  
  
  shortfall_amount_bar <- plot_ly(
    data = monthly_shortfall,
    x = ~Month,
    y = ~TotalShortfall,
    type = "bar",
    name = "Total Shortfall Amount",
    hovertemplate = paste(
      "Month: %{x|%b %Y}<br>",
      "Total Shortfall Amount: %{y}<extra></extra>"
    )
  ) %>%
    layout(
      yaxis = list(title = "Total Shortfall Amount"),
      xaxis = list(tickformat = "%b %Y", title = "Month")
    )
  
  
  p <- subplot(
    shortfall_number_bar,
    shortfall_amount_bar,
    nrows = 2,
    shareX = TRUE,
    titleX = TRUE
  ) %>%
    layout(
      xaxis = list(
        tickformat = "%b %Y",
        tickmode = "linear",
        dtick = "M1",
        showticklabels = TRUE
      ),
      margin = list(t = 80, b = 40),
      grid = list(rows = 2, columns = 1, pattern = "independent"),
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.2,
        yanchor = "top"
      )
    ) 
  
  p$x$source <- "A"
  p <- event_register(p, "plotly_click")
  
  list(
    shortfall_plot = p,
    total_shortfalls = total_shortfalls
  )

}


# CIRCOS PLOT

"
  Two sides: funding and expenses,
  
  feature activation: when user clicks on a bar in the bar graph,
  app should show the circos plot of allocation at that point in time

"

# Mock dataframe

"

"

create_circos_plot <- function() {
  
  
  circos.initialize()
}












