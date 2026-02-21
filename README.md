# WEHI Forecasting with Workdays Project

---

## Table of Contents

[1. Quick Links](#1-quick-links)

[2. Getting Started](#2-getting-started)

[3. Usage](#3-usage)

[4. Project Structure](#4-project-structure)

[5. Project Evolution](#5-project-evolution)

[6. Resources & Documentation](#6-resources--documentation)

---

## 1. Quick Links

- [Live App: Forecasting Budget App](https://forecasting-budget.shinyapps.io/forecasting-budget-demo/)
- [Live Demo](https://wehieduau.sharepoint.com/:f:/r/sites/StudentInternGroupatWEHI/Shared%20Documents/Forecasting%20Budget/Summer%202025-2026/PRESENTATION%20AND%20FORMAL%20REPORTS/Demo%20Video?csf=1&web=1&e=khZeuK)
- [Fast Track report](https://wehieduau.sharepoint.com/:w:/r/sites/StudentInternGroupatWEHI/Shared%20Documents/Forecasting%20Budget/2025%20Summer/Presentation%20and%20Formal%20Reports/Fast-track%20Report.docx?d=we00883216ea24fa7aa48c4a65b73be05&csf=1&web=1&e=cKiZhd)
- [Final Report powerpoint](https://wehieduau.sharepoint.com/:p:/r/sites/StudentInternGroupatWEHI/Shared%20Documents/Forecasting%20Budget/Summer%202025-2026/PRESENTATION%20AND%20FORMAL%20REPORTS/Forecasting%20Budget%20Summer%2025-26%20Final%20Presentation1.pptx?d=w78eb122ed4574714ad6ee22068fe082e&csf=1&web=1&e=6hV9sp)


---

## 2. Getting Started

### 2.1 Prerequisites

You will need **R** (**version 4.4.1 (2024-06-14) -- "Race for Your Life"** as WEHI's internal Linux server runs on this version) and **RStudio** installed on your machine.

**R Installation Links**
-   [**Download R**](https://cran.r-project.org/)
-   [**Download RStudio**](https://posit.co/download/rstudio-desktop/)

### 2.2 Installation

1.  **Clone the repository:**

    ``` bash
    git clone https://github.com/narolinelim/forecasting-with-workday-project.git
    ```
    * If you're working on the forked version from Rowland, replace the URL with your forked repository URL.

2.  **Run the app**

    Open the `app.R` file in RStudio. There might be updates and changes to the packages used in the code, but there is no need for the user to manually install or update any packages as the code will automatically check for package requirements and install any missing packages when the app is launched.

    Click **Run App** or run:

    ``` bash
    shiny::runApp("path/to/your/app.R")
    ```

---

## 3. Usage

1.  **Upload the Master Spreadsheet** - The Master Spreadsheet contains all the necessary data for forecasting, including funding sources and expenses. In this version, the user will upload the Master Spreadsheet into the app to initiate the forecasting process.

2.  **Select the priority level** - Select the priority level for the forecast. The priority level determines how funds are allocated to expenses based on their importance.

3.  **Generate the forecast** - Generate the forecast based on the uploaded Master Spreadsheet and selected priority level. The app will process the data and provide a forecast of funding allocation and expenses.

4.  **Download the foretasted results in Dashboard** - After generating the forecast, the user can download the results in the Dashboard section of the app. The results will be available in an Excel format.

---

## 4. Project Structure

### 4.1 Overview

This repository contains the codebase for the WEHI Forecasting with Workdays Shiny application. This is a proof of concept that streamlines the process of updates and finalisaiton for budget forecasting, ensuring that the forecast is consistent and systematic.
The app accepts a Master Spreadsheet as input, processes the data, and generates a forecast based on user-defined priority levels. 

### 4.2 Modules

#### 4.2.1 Directory Structure
```
├── README.md                             # Overview, setup and usage
├── app.R                                 # App launcher
├── requirements/
│   └── packages.R                        # Dependency checks / install helper
├── www/                                  # Static assets: images, css, js
│   └── style.css                         # Custom styling
└── src/
    ├── main_ui.R                         # Main UI layout and page wiring
    ├── main_server.R                     # Main server wiring (main_server_logic + module sourcing)
    ├── server/                           # Server-side modules (business logic)
    │   ├── dashboard_server.R            # Dashboard page server logic
    │   ├── forecast_page_server.R        # Forecast page server logic
    │   ├── input_review_server.R         # Input review page server logic
    │   └── components/                   # Reusable business logic components
    │       ├── allocation_algorithm.R    # Allocation engine (optimizer + helpers)
    │       ├── data_processing.R         # Read/clean Excel and validation routines
    │       ├── edit_rows.R               # CRUD helpers for funding & expense rows
    │       ├── sorting.R                 # Sorting and reordering logic
    │       ├── graph.R                   # Plot creation (shortfall, circos/chord)
    │       └── output.R                  # Download handlers and Excel builders
    └── ui/                               # UI component files
        ├── dashboard_ui.R                # Dashboard page UI and components
        ├── forecast_page_ui.R            # Forecast page UI and components
        └── input_review_ui.R             # Input review page UI and components
```

#### 4.2.2 Module Structure Diagram

The diagram below illustrates the structure and interaction of the various modules within the application. The modules sturcture as `module A ---> module B` indicates that `module A` calls function(s) in `module B`.

![Module Structure Diagram](www/module_structure_diagram_v2.png)

* Link to `function_calls` documentation: [link](https://drive.google.com/file/d/1Te92fI6pUqYcI2P1Gx5USteNNj-qVOYJ/view?usp=sharing)

Diagram is updated as of 16/02/2025

#### 4.2.3 Values Callouts

How to call variables across modules:
- `input$variable_name` - Access input values from UI components.
- `output$variable_name` - Define output values to be rendered in the UI.
- `values (reactiveValues)` - Store and manage reactive values that can be shared across modules.
    - `values$variable_name` - Access or modify reactive values.

Each component should have a ID defined in the UI and server modules that will be used as `variable_name` in the above calls.

### 4.3 Code Guidelines

#### 4.3.1 Naming Conventions

- **Use descriptive names for variables and functions**
Variables and functions should have clear and descriptive names that indicate their purpose. Generic names like `df` or `temp` should be avoided. For special cases where generic names are used, comments should be added to clarify their purpose and how it can be used.

- **Consistent naming style**
Use a consistent naming style throughout the codebase. In this project, we use `snake_case` for variable and function names (e.g., `calculate_forecast`, `user_input_data`).

#### 4.3.2 Functions

The function should have a clear purpose and a clear description of what it does. Each function should have a docstring that describes its purpose, input parameters, and return values.

Example:
```r
function_name <- function(param1, param2) {
    #' Function description and explaination
    #' 
    #' @param param1 Description of param1
    #' @param param2 Description of param2
    #' 
    #' @return Description of the return value

    # Function logic here
    return(result) # Explixit return statement
}
```

The function should explicity return a value using the `return()` statement, rather than relying on implicit returns in R for better readability and maintainability. 

---

## 5. Project Evolution

### Current Workflow

1.  **Data Input** - The user inputs funding sources and expenses.
    - User can upload a Master Spreadsheet containing all necessary data for forecasting.
    - User can also manually add/delete funding sources and expenses through the app interface.
2.  **Data Processing** - The app processes the uploaded data, cleaning and validating it for forecasting.
3.  **Priority Selection** - The user selects the priority level for the forecast.
    - Column priorities: Sort the expenses based on user-defined columns.
    - Manual priorities: User can manually reorder expenses to set their priorities.
4.  **Forecast Generation** - The app generates the forecast based on the processed data and selected priority level.
5.  **Results Output** - The user can download the forecasted results in Excel format.

---

## 6. Resources & Documentation

### 6.1 Handbooks

- [**User Manual (Web App Workflow)**](https://wehieduau.sharepoint.com/:w:/r/sites/StudentInternGroupatWEHI/Shared%20Documents/Forecasting%20Budget/Summer%202025-2026/PRESENTATION%20AND%20FORMAL%20REPORTS/User%20Manual.docx?d=w7c8aa2a0ff304e09bc794a126d1b848f&csf=1&web=1&e=wTcszP)

- [**Admin Manual (Local Setup)**](https://wehieduau.sharepoint.com/:w:/r/sites/StudentInternGroupatWEHI/_layouts/15/Doc2.aspx?action=editNew&sourcedoc=%7B518bb466-6083-474b-8832-eb06cbc1d9f4%7D&wdOrigin=TEAMS-MAGLEV.teamsSdk_ns.rwc&wdExp=TEAMS-TREATMENT&wdhostclicktime=1770083024171&web=1)

### 6.2 Project Background

- [**Project Folder**](https://wehieduau.sharepoint.com/:f:/r/sites/StudentInternGroupatWEHI/Shared%20Documents/Forecasting%20Budget/Summer%202025-2026?csf=1&web=1&e=1UrsrQ)

- [**Wireframe**](https://www.figma.com/design/3MiVeEfBgM1ZnFrgMnojyG/Draft_UI-UX?node-id=47-752)

- [**Technical Diaries**](https://wehieduau.sharepoint.com/:f:/r/sites/StudentInternGroupatWEHI/Shared%20Documents/Forecasting%20Budget/Summer%202025-2026/TECHNICAL%20DIARY?csf=1&web=1&e=lY055g)

- [**Individual Learning Plans**](https://wehieduau.sharepoint.com/:f:/r/sites/StudentInternGroupatWEHI/Shared%20Documents/Forecasting%20Budget/Summer%202025-2026/ILP?csf=1&web=1&e=bSyhLj)

### 6.3 Future Enhancements

- [**Possible Future Improvements**](https://wehieduau.sharepoint.com/:w:/r/sites/StudentInternGroupatWEHI/_layouts/15/Doc2.aspx?action=editNew&sourcedoc=%7B8ea9208e-5a0e-44ea-83bb-65b2af95cee1%7D&wdOrigin=TEAMS-MAGLEV.teamsSdk_ns.rwc&wdExp=TEAMS-TREATMENT&wdhostclicktime=1770084328500&web=1)

  
---
