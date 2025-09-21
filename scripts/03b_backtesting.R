# scripts/03b_backtesting.R
# Purpose: Rolling-origin cross-validation for store-level weekly sales using ARIMA.
# Notes (my reasoning):
# - I use forecast::tsCV so I can get multi-step-ahead errors cleanly.
# - I summarize errors across horizons 1..h to a single score per store (and also keep per-horizon).
# - This complements the simple last-12-weeks holdout in 03_forecasting.R.

# ---- Packages ----
need <- c("readr","dplyr","tibble","purrr","forecast")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
invisible(lapply(need, library, character.only = TRUE))

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# ---- Params ----
h <- 12L  # evaluate 1..12 weeks ahead
min_obs <- 2 * 52  # require ~2 years of history to participate

# ---- Load data ----
path_csv <- "data/processed/store_weekly_sales.csv"
path_rds <- "data/processed/store_weekly_sales.rds"
if (file.exists(path_csv)) {
  store_weekly <- readr::read_csv(path_csv, show_col_types = FALSE)
} else if (file.exists(path_rds)) {
  store_weekly <- readr::read_rds(path_rds)
} else stop("No processed dataset found. Run scripts/01_data_cleaning.R first.")

store_weekly <- store_weekly %>% dplyr::arrange(store, date)

# ---- Choose evaluation set: top 10 by total sales (and enough history) ----
stores_to_eval <- store_weekly %>%
  group_by(store) %>%
  summarize(n = dplyr::n(), total = sum(total_weekly_sales, na.rm = TRUE), .groups = "drop") %>%
  filter(n >= min_obs) %>%
  slice_max(order_by = total, n = 10) %>%
  pull(store)

message("Backtesting stores: ", paste(stores_to_eval, collapse = ", "))

# ---- Helper: compute tsCV errors for ARIMA for a single series ----
fcfun <- function(y, h) {
  fit <- forecast::auto.arima(y, stepwise = TRUE, approximation = FALSE)
  forecast::forecast(fit, h = h)
}

eval_store_tscv <- function(store_id) {
  y <- store_weekly %>%
    filter(store == store_id) %>%
    arrange(date) %>%
    pull(total_weekly_sales)
  
  if (length(y) < min_obs) return(NULL)
  
  y_ts <- ts(y, frequency = 52)
  # e is a matrix: rows = time indices, cols = horizon 1..h
  e <- forecast::tsCV(y_ts, forecastfunction = function(x, h) fcfun(x, h), h = h)
  
  # Metrics per horizon
  horizon_tbl <- tibble::tibble(
    store  = store_id,
    horizon = 1:h,
    RMSE = apply(e, 2, function(col) sqrt(mean(col^2, na.rm = TRUE))),
    MAE  = apply(e, 2, function(col) mean(abs(col), na.rm = TRUE))
  )
  
  # Overall summary across horizons (robust one-number score per store)
  overall_tbl <- horizon_tbl %>%
    summarize(
      store = store_id,
      RMSE = mean(RMSE, na.rm = TRUE),
      MAE  = mean(MAE,  na.rm = TRUE),
      .groups = "drop"
    )
  
  list(horizon = horizon_tbl, overall = overall_tbl)
}

# ---- Run across stores ----
res <- purrr::map(stores_to_eval, eval_store_tscv)
res <- res[!purrr::map_lgl(res, is.null)]

if (!length(res)) stop("No series available for backtesting with the current constraints.")

by_horizon <- dplyr::bind_rows(purrr::map(res, "horizon"))
by_store   <- dplyr::bind_rows(purrr::map(res, "overall")) %>% mutate(model = "ARIMA", h = h)

# ---- Save outputs ----
readr::write_csv(by_store,   "outputs/tables/backtest_metrics_by_store.csv")
readr::write_csv(by_horizon, "outputs/tables/backtest_metrics_by_horizon.csv")

# ---- Quick average summary for README/report ----
avg <- by_store %>%
  summarize(n_stores = dplyr::n(), RMSE = mean(RMSE, na.rm = TRUE), MAE = mean(MAE, na.rm = TRUE), .groups = "drop")
readr::write_csv(avg, "outputs/tables/backtest_metrics_avg.csv")

message("Backtesting complete. Tables written to outputs/tables/:",
        "\n - backtest_metrics_by_store.csv",
        "\n - backtest_metrics_by_horizon.csv",
        "\n - backtest_metrics_avg.csv")
