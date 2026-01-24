
# testing complex cases

# Funding Source
funding <- data.frame(
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

# Allocation Data frame
df_allocations <- data.frame(
  SourceID = c(
    "FS001","FS004","FS007",
    "FS002","FS008",
    "FS003","FS006",
    "FS009",
    "FS010","FS011",
    "FS005","FS012",
    "FS013","FS014",
    "FS015",
    "FS016","FS017",
    "FS018","FS019",
    "FS020","FS021","FS022"
  ),
  ExpenseID = c(
    "E001","E001","E001",
    "E002","E002",
    "E003","E003",
    "E004",
    "E005","E005",
    "E006","E006",
    "E007","E007",
    "E008",
    "E009","E009",
    "E010","E010",
    "E011","E011","E012"
  ),
  ExpenseCategory = c(
    "Salary","Salary","Salary",
    "Equipment","Equipment",
    "Travel","Travel",
    "Salary",
    "Equipment","Equipment",
    "Travel","Travel",
    "Salary","Salary",
    "Equipment",
    "Travel","Travel",
    "Salary","Salary",
    "Equipment","Equipment","Travel"
  ),
  AllocatedAmount = c(
    12000, 5000, 3000,
    10000, 4000,
    6000, 2000,
    8000,
    15000, 5000,
    3000, 1000,
    10000, 6000,
    12000,
    4000, 1000,
    9000, 2000,
    5000, 3000, 2500
  )
)


# Expenses 
df_expenses_status <- data.frame(
  ID = paste0("E", sprintf("%03d", 1:12)),
  Category = c(
    "Salary","Equipment","Travel","Salary",
    "Equipment","Travel","Salary","Equipment",
    "Travel","Salary","Equipment","Travel"
  ),
  Amount = c(
    22000, 18000, 8000, 10000,
    25000, 6000, 18000, 15000,
    7000, 13000, 12000, 5000
  ),
  Date = c(
    "15/01/2025","20/02/2025","10/03/2025","01/05/2025",
    "20/06/2025","15/08/2025","01/10/2025","10/12/2025",
    "15/02/2026","01/04/2026","20/07/2026","10/11/2026"
  ),
  FilledAmount = c(
    20000, 14000, 8000, 8000,
    20000, 4000, 16000, 12000,
    5000, 11000, 8000, 2500
  ),
  IsFilled = c(
    FALSE, FALSE, TRUE, FALSE,
    FALSE, FALSE, FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE
  ),
  Status = c(
    "Partial","Partial","Full","Partial",
    "Partial","Partial","Partial","Partial",
    "Partial","Partial","Partial","Partial"
  )
)


df_funds_summary <- data.frame(
  SourceID = paste0("FS", sprintf("%03d", 1:22)),
  InitialAmount = c(
    20000,15000,12000,10000,20000,
    8000,12000,6000,10000,11000,
    9000,5000,16000,14000,12000,
    10000,9000,11000,8000,7000,
    6000,5000
  ),
  UsedAmount = c(
    12000,10000,8000,5000,15000,
    3000,12000,4000,10000,11000,
    5000,1000,16000,6000,12000,
    4000,3000,9000,2000,5000,
    3000,2500
  ),
  RemainingAmount = c(
    8000,5000,4000,5000,5000,
    5000,0,2000,0,0,
    4000,4000,0,8000,0,
    6000,6000,2000,6000,2000,
    3000,2500
  )
)
