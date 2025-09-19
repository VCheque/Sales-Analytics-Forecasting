# Data Dictionary — Walmart Store Sales (Weekly)

Source: Kaggle “Walmart Recruiting – Store Sales Forecasting”. Files: `train.csv`, `features.csv`, `stores.csv`, `sampleSubmission.csv`.  
Reference: competition data page.  

## train.csv
- Store: integer, store ID
- Dept: integer, department ID
- Date: week start date (YYYY-MM-DD)
- Weekly_Sales: numeric, weekly sales for that department at that store (target)
- IsHoliday: boolean (TRUE/FALSE) indicating a holiday week (Super Bowl, Labor Day, Thanksgiving, Christmas per competition)

## features.csv
- Store: integer, store ID
- Date: week start date
- Temperature: numeric, average temp in the region
- Fuel_Price: numeric, cost of fuel in the region
- MarkDown1..MarkDown5: numeric, promotional markdowns (many missing values)
- CPI: numeric, consumer price index
- Unemployment: numeric, unemployment rate
- IsHoliday: boolean, holiday indicator aligned to `train`

## stores.csv
- Store: integer, store ID
- Type: categorical, store format (e.g., A/B/C)
- Size: integer, store size (square feet)

## sampleSubmission.csv
- Id: string key "Store_Dept_Date"
- Weekly_Sales: numeric (to be predicted)
