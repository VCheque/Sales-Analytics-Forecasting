U.S. Retail Sales Analytics & Forecasting (R + Shiny)

**Overview**

This project analyzes U.S. retail store sales data to uncover key business insights and predict future sales trends.
The work includes data cleaning, exploratory analysis, forecasting models, and an interactive Shiny dashboard.
It is designed as a portfolio case study for roles in Data Analysis, Business Intelligence, and Data Engineering.

**Business Problem**

Retailers need to:

    Understand sales patterns across products, stores, and time.
    
    Identify seasonality and holiday effects on demand.
    
    Forecast future sales to optimize inventory and staffing.


**Repository Structure**

sales-analytics-forecasting/
├── data/
│   └── raw/               # raw datasets (from Kaggle)
├── scripts/
│   ├── 01_data_cleaning.R
│   ├── 02_eda_visuals.R
│   ├── 03_forecasting.R
│   └── 04_shiny_app.R
├── app/
│   └── app.R              # Shiny app code
├── report/
│   └── sales_analysis_report.Rmd
├── README.md              # project documentation
└── renv.lock              # reproducible R environment


**Tech Stack**

    R (data cleaning, visualization, modeling)
    
    ggplot2 (EDA & plots)
    
    forecast / prophet (time series forecasting)
    
    Shiny (interactive dashboard)
    
    renv (reproducible environment)

**Dataset**

    Source: Kaggle – Store Sales Time Series Forecasting (https://www.kaggle.com/competitions/walmart-recruiting-store-sales-forecasting/data)
    
    Description: Daily sales for multiple U.S. stores and product families.
    
    Features:
    
    date (daily)
    
    store_nbr (store ID)
    
    family (product category)
    
    sales (units sold)
    
    onpromotion (promo indicator)


**Methodology**

*Data Cleaning*

    Handle missing values, duplicates, and date formatting.
    
    Create new features (month, year, weekday, holiday flag).

*Exploratory Data Analysis (EDA)*

    Sales trends by store and product family.
    
    Seasonal patterns and holiday spikes.
    
    Correlation between promotions and sales.

*Forecasting Models*

    ARIMA (forecast package).
    
    Prophet (Meta’s time-series library).
    
    Compare models with RMSE/MAE.

*Dashboard (Shiny)*

    Overview tab (KPIs).
    
    Interactive filters (store, product).
    
    Forecast tab (next 3–6 months prediction).
