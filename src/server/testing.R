
# testing complex cases

library(ggplot2)
library(dplyr)

# Funding Source
fundings <- data.frame(
  ID = c("FS001", "FS002", "FS003", "FS004", "FS005", "FS006", "FS007"),
  Category = c(
    "Salary",
    "Equipment",
    "Travel",
    "Salary",
    "Equipment",
    "Travel",
    "Equipment"
  ),
  ValidFrom = as.Date(c(
    "2025-01-01",
    "2025-02-01",
    "2025-04-01",
    "2025-05-01",
    "2025-07-01",
    "2025-01-01",
    "2025-10-01"
  )),
  ValidTo = as.Date(c(
    "2025-03-31",
    "2025-06-30",
    "2025-09-30",
    "2025-12-31",
    "2025-12-31",
    "2025-12-31",
    "2025-12-31"
  )),
  Amount = c(
    12000,
    18000,
    10000,
    30000,
    15000,
    8000,
    9000
  )
)


# Expense Data Frame
expenses <- data.frame(
  ID = c(
    "E_01","E_02","E_03","E_04","E_05",
    "E_06","E_07","E_08","E_09","E_10"
  ),
  Category = c(
    "Salary",
    "Equipment",
    "Salary",
    "Travel",
    "Equipment",
    "Salary",
    "Travel",
    "Equipment",
    "Salary",
    "Travel"
  ),
  Date = as.Date(c(
    "2025-01-15",
    "2025-02-20",
    "2025-03-10",
    "2025-04-05",
    "2025-05-15",
    "2025-06-10",
    "2025-07-20",
    "2025-08-30",
    "2025-10-01",
    "2025-11-15"
  )),
  Amount = c(
    9000,
    40000,
    8000,
    6000,
    15000,
    20000,
    7000,
    9000,
    12000,
    5000
  )
)

# Funding order (top to bottom)
funding_plot <- fundings %>%
  arrange(ValidFrom, desc(Amount)) %>%
  mutate(ID = factor(ID, levels = rev(unique(ID))))

# Expense order (so dots show on their own rows)
expenses_plot <- expenses %>%
  arrange(Date, desc(Amount)) %>%
  mutate(ID = factor(ID, levels = rev(unique(ID))))

ggplot() +
  # Funding windows (segments)
  geom_segment(
    data = funding_plot,
    aes(x = ValidFrom, xend = ValidTo, y = ID, yend = ID, linewidth = Amount, color = Category),
    alpha = 0.7
  ) +
  # Expense points (on a separate panel-like band below)
  geom_point(
    data = expenses_plot,
    aes(x = Date, y = ID, size = Amount, shape = Category),
    alpha = 0.9
  ) +
  scale_size_continuous(range = c(2, 8)) +
  labs(
    title = "Funding validity windows + expense due dates",
    x = "Date",
    y = "ID",
    linewidth = "Funding amount",
    size = "Expense amount",
    color = "Funding category",
    shape = "Expense category"
  ) +
  theme_minimal(base_size = 12)


ideal_allocation <- data.frame(
  ExpenseID = c("E001","E002","E002","E002","E003","E003","E004","E005","E006","E007","E007","E009","E010"),
  ExpenseDate = c("15/01/2025","20/02/2025","20/02/2025","20/02/2025","10/03/2025","10/03/2025",
                  "05/04/2025","15/05/2025","10/06/2025","20/07/2025","20/07/2025","01/10/2025","15/11/2025"),
  Category = c("Salary","Equipment","Equipment","Equipment","Salary","Salary","Travel","Equipment","Salary","Travel","Travel","Salary","Travel"),
  ExpenseAmount = c(9000,40000,40000,40000,8000,8000,6000,15000,20000,7000,7000,12000,5000),
  
  SourceID = c("FS001","FS002","FS005","FS007","FS001","FS004","FS003","FS007","FS004","FS003","FS006","FS004","FS006"),
  AllocatedAmount = c(9000,18000,15000,7000,3000,5000,6000,2000,20000,4000,3000,5000,5000),
  
  # when the money is actually available to pay (same-day if active; otherwise ValidFrom if overdue)
  AllocationDate = c("15/01/2025","20/02/2025","01/07/2025","01/10/2025","10/03/2025","01/05/2025",
                     "05/04/2025","01/10/2025","10/06/2025","20/07/2025","20/07/2025","01/10/2025","15/11/2025"),
  
  IsOverdueAllocation = c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
)

