U.S. Retail Sales Analytics & Forecasting
================

**Overview**

This repository contains an end-to-end workflow for analyzing U.S.
retail weekly sales and producing short-term forecasts (ARIMA baseline;
Prophet optional). For the full narrative, see the HTML report in
report/ or the GitHub Pages link.

*Live report (GitHub Pages)*:
<https://vcheque.github.io/Sales-Analytics-Forecasting/> *Raw HTML in
repo*: report/sales_analysis_report.html

**Quick KPIs**

| min_date   | max_date   | n_stores | n_depts | total_sales |
|:-----------|:-----------|---------:|--------:|------------:|
| 2010-02-05 | 2012-10-26 |       45 |      81 |  6737218987 |

**Highlights**

*Overall Weekly Sales Trend*

![](outputs/figures/01_overall_weekly_sales_trend.png)<!-- -->

*Top 10 Stores by Total Sales*

![](outputs/figures/02_top10_stores_total_sales.png)<!-- -->

*Seasonality Heatmap*

![](outputs/figures/03_seasonality_heatmap_month_year.png)<!-- -->

*Backtesting (tsCV) snapshot*

| n_stores |     RMSE |      MAE |
|---------:|---------:|---------:|
|       10 | 301699.1 | 165866.9 |

Rolling-origin CV — average metrics across evaluated stores

<img src="README_files/figure-gfm/unnamed-chunk-5-1.png" width="672" />

**How to Reproduce**

    scripts/01_data_cleaning.R → creates data/processed/*

    scripts/02_eda_visuals.R → saves figures/tables to outputs/*

    scripts/03_forecasting.R → saves forecast plots + metrics

    Shiny: shiny::runApp("app")

    Report: knit report/sales_analysis_report.Rmd

**Environment**

Reproducibility managed by renv. Run renv::restore() to install
packages.
