

library(ggplot2)
library(plotly)

# shortfall plot

# Three potential plots:
# - bar chart
# - step line graph
# - stacked bar chart


# Mock dataframe

"
Y axis: total shortfall amount
X axis: timeline in weeks

"

# EXPENSE DATAFRAME

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

# ALLOCATION DATAFRAME 

allocation_data <- data.frame(
  ExpenseID = c(
    "E005","E005","E005","E006","E007","E007","E008","E010",
    "E010","E011","E012","E009","E014","E014","E015","E001",
    "E001","E002","E003","E003","E004"
  ),
  
  ExpenseCategory = c(
    "Equipment","Equipment","Equipment","Travel","Salary","Salary",
    "Equipment","Salary","Salary","Equipment","Travel","Travel",
    "Equipment","Equipment","Travel","Salary","Salary","Equipment",
    "Travel","Travel","Salary"
  ),
  
  ExpenseAmount = c(
    15000,15000,15000,4000,8000,8000,10000,15000,
    15000,12000,5000,6000,20000,20000,15000,5000,
    5000,8000,3000,3000,12000
  ),
  
  AllocatedAmount = c(
    2000,10000,3000,4000,3000,5000,10000,13000,
    2000,12000,5000,6000,13000,7000,15000,3000,
    2000,8000,2000,1000,12000
  ),
  
  LatestPaymentDate = as.Date(c(
    "2025-05-20","2025-05-20","2025-05-20","2025-06-10","2025-07-15",
    "2025-07-15","2025-07-20","2025-09-15","2025-09-15","2025-10-20",
    "2025-10-11","2025-08-10","2025-12-20","2025-12-20","2025-12-25",
    "2025-02-15","2025-02-15","2025-02-20","2025-03-10","2025-03-10",
    "2025-04-15"
  )),
  
  SourceID = c(
    "FS002","FS005","FS007","FS010","FS004","FS006","FS002","FS006",
    "FS009","FS007","FS010","FS003","FS007","FS009","FS004","FS001",
    "FS004","FS007","FS003","FS010","FS001"
  ),
  
  SourceValidFrom = as.Date(c(
    "2025-02-01","2025-04-01","2025-01-01","2025-02-01","2025-02-01",
    "2025-06-01","2025-02-01","2025-06-01","2025-07-01","2025-01-01",
    "2025-02-01","2025-03-01","2025-01-01","2025-07-01","2025-02-01",
    "2025-02-01","2025-02-01","2025-01-01","2025-03-01","2025-02-01",
    "2025-02-01"
  )),
  
  SourceValidTo = as.Date(c(
    "2025-08-31","2025-10-31","2025-12-31","2025-12-31","2025-12-31",
    "2025-12-31","2025-08-31","2025-12-31","2025-12-31","2025-12-31",
    "2025-12-31","2025-09-30","2025-12-31","2025-12-31","2025-12-31",
    "2025-06-30","2025-12-31","2025-12-31","2025-09-30","2025-12-31",
    "2025-06-30"
  )),
  
  stringsAsFactors = FALSE
)


ordered_funding_allocation <- allocation_data[order(allocation_data$SourceValidFrom),]


total_expense <- sum(expenses$Amount)

total_allocated <- sum(allocation_data$AllocatedAmount)


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