

library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)

# shortfall plot

# Three potential plots:
# - bar chart
# - step line graph
# - stacked bar chart


# Mock dataframe

"
Y axis: total shortfall amount
X axis: timeline in weeks or months

"

ordered_allocation <- data.frame(
  ExpenseID = c("E013", "E003", "E006", "E009",
                "E012", "E001", "E004", "E007",
                "E010", "E002", "E005", "E008",
                "E011", "E014", "E015"),
  Category = c("Salary", "Travel", "Travel", "Travel",
               "Travel", "Salary", "Salary", "Salary",
               "Salary", "Equipment", "Equipment", "Equipment",
               "Equipment", "Equipment", "Travel"),
  Amount = c(10000, 3000, 4000, 6000, 5000, 5000,
             12000, 8000, 15000, 8000, 15000, 10000,
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

shortfall_bar <- plot_ly(
  monthly_shortfall, 
  x = ~Month, 
  y = ~TotalShortfall,
  type = "bar", 
  hovertemplate = paste(
    "Month: %{x|%b %Y}<br>",
    "Total Shortfall: %{y}<extra></extra>"
  ),
  color = "red"
  ) %>%
  layout(
    xaxis = list(
      side = "top",
      title = "Month",
      tickformat = "%b %Y"
    )
  )
  

shortfall_bar

# shortfall_data <- data.frame(
#   Weeks = c("Week 1", "Week 2", "Week 3", "Week 4"),
#   Shortfall = c(0, 250, 1500, 0)
# )
# 
# ggplot(shortfall_data, aes(x=Weeks, y=Shortfall)) + 
#   geom_bar(stat = "identity")
# 
# 
# bar_fig <- plot_ly(
#   x = c("Week 1", "Week 2", "Week 3", "Week 4"),
#   y = c(0, 250, 1500, 0),
#   name = "shortfall",
#   type = "bar"
# )
# 
# 
# step_fig <- plot_ly(
#   shortfall_data, x = ~Weeks, y = ~Shortfall, type = 'scatter', mode = 'lines',
#   line = list(shape = 'hv', color = 'blue', width = 2)
# )
# step_fig


# circos plot