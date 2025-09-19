# scripts/03_forecasting.R
# Purpose: Forecast store-level weekly sales and compare ARIMA vs Prophet on a 12-week holdout.
# Author's notes:
# - I’m keeping the forecasting unit at the store-week level because it’s the most
#   decision-relevant granularity for staffing/inventory. Department-level is possible
#   but noisier and multiplies the number of series.
# - I evaluate out-of-sample performance on the last 12 weeks to simulate "future"
#   forecasting. This avoids leaking information from the tail of the series.

# ---------------------------
# Packages
# ---------------------------
# Why these:
# - readr/dplyr/tibble/purrr: data IO + wrangling + iteration over stores
# - lubridate: date handling (weekly frequency, splits)
# - ggplot2: evaluation plots per store
# - forecast: ARIMA baselines (auto.arima is a strong classical baseline)
# - tidyr: pivot_longer for nice plotting
need <- c("readr","dplyr","tibble","purrr","lubridate","ggplot2","forecast","tidyr")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
invisible(lapply(need, library, character.only = TRUE))

# Prophet is optional. If it's not installed, I still run ARIMA so the script never blocks.
has_prophet <- requireNamespace("prophet", quietly = TRUE)

# ---------------------------
# Paths
# ---------------------------
# I save figures/tables under outputs so the report/README can link to stable artifacts.
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables",  recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# Load processed data
# ---------------------------
# I prefer CSV for portability (viewable on GitHub). If missing, I fall back to RDS.
path_csv <- "data/processed/store_weekly_sales.csv"
path_rds <- "data/processed/store_weekly_sales.rds"

if (file.exists(path_csv)) {
  store_weekly <- readr::read_csv(path_csv, show_col_types = FALSE)
} else if (file.exists(path_rds)) {
  store_weekly <- readr::read_rds(path_rds)
} else {
  stop("No processed dataset found. Run scripts/01_data_cleaning.R first.")
}

# Ensure the 'date' column is Date; I harden this to prevent surprises.
store_weekly <- store_weekly %>% dplyr::mutate(date = as.Date(date))

# ---------------------------
# Metrics
# ---------------------------
# I report RMSE/MAE for magnitude and MAPE for relative error, skipping zeros to avoid
# division-by-zero artifacts. These are common yardsticks for demand forecasting.
rmse <- function(actual, pred) sqrt(mean((actual - pred)^2, na.rm = TRUE))
mae  <- function(actual, pred) mean(abs(actual - pred), na.rm = TRUE)
mape <- function(actual, pred) {
  idx <- actual != 0 & is.finite(actual) & is.finite(pred)
  mean(abs((actual[idx] - pred[idx]) / actual[idx])) * 100
}

# ---------------------------
# Holdout definition
# ---------------------------
# I hold out the last 12 weeks (approx. a quarter) to emulate a realistic short-term forecast.
h <- 12L

# I don’t evaluate all stores to keep runtime/plots manageable in the repo.
# Instead, I take the top-10 by total sales so we’re focusing on the most impactful series.
stores_to_eval <- store_weekly %>%
  dplyr::group_by(store) %>%
  dplyr::summarize(total = sum(total_weekly_sales, na.rm = TRUE), .groups = "drop") %>%
  dplyr::slice_max(order_by = total, n = 10) %>%
  dplyr::pull(store)

message("Evaluating stores: ", paste(stores_to_eval, collapse = ", "))

# ---------------------------
# Core evaluation for a single store
# ---------------------------
# Design choices:
# - I sort by date to ensure proper temporal order.
# - I build a weekly time series with frequency = 52 for ARIMA seasonality detection.
# - For Prophet (if available), I use weekly + yearly seasonality; I keep defaults light
#   to avoid overfitting and to keep the tutorial reproducible across environments.
eval_store <- function(store_id) {
  df <- store_weekly %>%
    dplyr::filter(store == store_id) %>%
    dplyr::arrange(date)
  
  all_weeks <- sort(unique(df$date))
  if (length(all_weeks) <= h + 10) {
    # If there isn't enough history for a meaningful train/test split, I skip this store.
    return(NULL)
  }
  
  test_weeks <- tail(all_weeks, h)
  train_df   <- df %>% dplyr::filter(date < min(test_weeks))
  test_df    <- df %>% dplyr::filter(date %in% test_weeks)
  
  # Numeric training target
  y_train <- train_df$total_weekly_sales
  
  # ---------------------------
  # ARIMA baseline
  # ---------------------------
  # I use frequency = 52 (weekly seasonality). auto.arima with stepwise heuristic is a
  # strong baseline and relatively fast. This sets a credible benchmark for Prophet/ML later.
  y_ts <- stats::ts(y_train, frequency = 52)
  fit_arima   <- forecast::auto.arima(y_ts, stepwise = TRUE, approximation = FALSE)
  fc_arima    <- forecast::forecast(fit_arima, h = h)
  pred_arima  <- as.numeric(fc_arima$mean)
  
  # ---------------------------
  # Prophet (optional)
  # ---------------------------
  # I keep Prophet optional to avoid blocking users who can't compile/install it locally.
  pred_prophet <- rep(NA_real_, h)
  if (has_prophet) {
    # Prophet expects a data frame with columns ds (date) and y (target).
    df_p <- tibble::tibble(ds = train_df$date, y = train_df$total_weekly_sales)
    
    # I enable weekly + yearly seasonality. changepoint.prior.scale is modest to avoid
    # chasing noise. Tuning is possible, but I keep it simple for clarity/comparability.
    m <- prophet::prophet(
      df_p,
      weekly.seasonality = TRUE,
      yearly.seasonality = TRUE,
      changepoint.prior.scale = 0.1
    )
    future <- prophet::make_future_dataframe(m, periods = h, freq = "week")
    fc_p   <- prophet::predict(m, future)
    pred_prophet <- tail(fc_p$yhat, h)
  }
  
  # ---------------------------
  # Metrics on the holdout
  # ---------------------------
  # I evaluate strictly on the withheld last 12 weeks so we’re not cherry-picking in-sample fit.
  actual <- test_df$total_weekly_sales
  met <- tibble::tibble(
    store = store_id,
    model = c("ARIMA", if (has_prophet) "Prophet" else NULL),
    RMSE  = c(rmse(actual, pred_arima),
              if (has_prophet) rmse(actual, pred_prophet) else NULL),
    MAE   = c(mae(actual, pred_arima),
              if (has_prophet) mae(actual, pred_prophet) else NULL),
    MAPE  = c(mape(actual, pred_arima),
              if (has_prophet) mape(actual, pred_prophet) else NULL)
  )
  
  # ---------------------------
  # Visualization: Actual vs predictions on holdout
  # ---------------------------
  # For auditability, I save one plot per store showing how the forecasts track held-out actuals.
  comp <- tibble::tibble(
    date   = test_df$date,
    Actual = actual,
    ARIMA  = pred_arima
  )
  if (has_prophet) comp$Prophet <- pred_prophet
  
  comp_long <- tidyr::pivot_longer(comp, -date, names_to = "series", values_to = "value")
  
  p <- ggplot2::ggplot(comp_long, ggplot2::aes(date, value, color = series)) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::labs(
      title = paste("Store", store_id, "- Holdout Forecast (", h, " weeks)", sep = " "),
      x = "Week", y = "Weekly Sales"
    ) +
    ggplot2::theme_minimal(base_size = 12)
  
  ggplot2::ggsave(
    filename = file.path("outputs/figures", sprintf("forecast_store_%s.png", store_id)),
    plot = p, width = 9, height = 5, dpi = 180
  )
  
  # Return a lightweight object so I can bind all metrics outside the function
  list(metrics = met)
}

# ---------------------------
# Run evaluation across selected stores
# ---------------------------
# I parallelize conceptually via purrr::map (simple and dependency-light). If I needed
# speed, I could switch to furrr with a plan(multisession) — not necessary here.
results <- purrr::map(stores_to_eval, eval_store)
results <- results[!purrr::map_lgl(results, is.null)]  # drop skipped stores

if (length(results) == 0) {
  stop("No stores evaluated (insufficient data).")
}

# ---------------------------
# Aggregate metrics
# ---------------------------
# I persist both per-store metrics (for drill-down) and average metrics (for quick summary).
metrics_tbl <- dplyr::bind_rows(purrr::map(results, "metrics"))

avg_metrics <- metrics_tbl %>%
  dplyr::group_by(model) %>%
  dplyr::summarize(
    n_stores = dplyr::n(),
    RMSE = mean(RMSE, na.rm = TRUE),
    MAE  = mean(MAE,  na.rm = TRUE),
    MAPE = mean(MAPE, na.rm = TRUE),
    .groups = "drop"
  )

# ---------------------------
# Save outputs
# ---------------------------
# I write tidy CSVs so the report/README (and future BI tools) can ingest them easily.
readr::write_csv(metrics_tbl, "outputs/tables/model_metrics_by_store.csv")
readr::write_csv(avg_metrics,  "outputs/tables/model_metrics_avg.csv")

message("Forecasting complete. Plots saved to outputs/figures/, tables to outputs/tables/.")
if (!has_prophet) {
  message("Note: Prophet not installed; only ARIMA results were generated.\n",
          "To enable Prophet: install.packages('prophet') or remotes::install_github('facebook/prophet')")
}
