# scripts/01_data_cleaning.R
# Purpose: read raw Walmart weekly sales data, clean, join features, engineer variables,
#          and export tidy datasets for EDA and forecasting.

# ---- Packages ----
need <- c("readr","dplyr","tidyr","lubridate","janitor")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
invisible(lapply(need, library, character.only = TRUE))

# ---- Paths ----
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# ---- Read raw ----
train    <- readr::read_csv("data/raw/train.csv", show_col_types = FALSE)    %>% janitor::clean_names()
features <- readr::read_csv("data/raw/features.csv", show_col_types = FALSE) %>% janitor::clean_names()
stores   <- readr::read_csv("data/raw/stores.csv", show_col_types = FALSE)   %>% janitor::clean_names()

# Expect columns (after clean_names):
# train:    store, dept, date, weekly_sales, is_holiday
# features: store, date, temperature, fuel_price, markdown1..markdown5, cpi, unemployment, is_holiday
# stores:   store, type, size

# ---- Types & basic cleaning ----
train <- train %>%
  mutate(
    date         = as.Date(date),
    store        = as.integer(store),
    dept         = as.integer(dept),
    weekly_sales = as.numeric(weekly_sales),
    is_holiday   = as.logical(is_holiday)
  )

features <- features %>%
  mutate(
    date       = as.Date(date),
    store      = as.integer(store),
    # Replace NA markdowns with 0 (no promotion)
    across(starts_with("markdown"), ~tidyr::replace_na(., 0)),
    # CPI / Unemployment can be imputed later if missing
    is_holiday = as.logical(is_holiday)
  )

stores <- stores %>%
  mutate(
    store = as.integer(store),
    type  = as.factor(type),
    size  = as.integer(size)
  )

# ---- Duplicate checks ----
dup_train <- train %>% count(store, dept, date) %>% filter(n > 1)
if (nrow(dup_train) > 0) {
  message("Warning: duplicate (store, dept, date) rows found in train: ", nrow(dup_train))
}

# ---- Join features & stores ----
# Keep the holiday flag from train; drop the one from features after a sanity check
hol_check <- train %>%
  inner_join(features %>% select(store, date, is_holiday), by = c("store","date"), suffix = c("_train","_feat")) %>%
  summarize(agree = mean(is_holiday_train == is_holiday_feat, na.rm = TRUE)) %>%
  pull(agree)

if (!is.na(hol_check) && hol_check < 0.99) {
  message("Note: holiday flags differ between train and features by more than 1%. Using train$is_holiday.")
}

features <- features %>% select(-is_holiday)

df <- train %>%
  left_join(features, by = c("store","date")) %>%
  left_join(stores,   by = "store")

# ---- Imputation for CPI / Unemployment (median by store, then global median) ----
fill_by_store <- function(x, grp) {
  x2 <- x
  med_by_store <- tapply(x, grp, function(v) median(v, na.rm = TRUE))
  idx <- is.na(x2)
  x2[idx] <- med_by_store[as.character(grp[idx])]
  # any still NA -> global median
  x2[is.na(x2)] <- median(x2, na.rm = TRUE)
  x2
}

if ("cpi" %in% names(df)) {
  df$cpi <- fill_by_store(df$cpi, df$store)
}
if ("unemployment" %in% names(df)) {
  df$unemployment <- fill_by_store(df$unemployment, df$store)
}

# ---- Feature engineering (calendar) ----
df <- df %>%
  mutate(
    year      = lubridate::year(date),
    month     = lubridate::month(date),
    week      = lubridate::isoweek(date),
    quarter   = lubridate::quarter(date),
    month_fac = factor(month, levels = 1:12, labels = month.abb)
  )

# ---- Export 1: department-level weekly (joined tidy) ----
readr::write_rds(df, "data/processed/department_weekly_joined.rds")
readr::write_csv(df, "data/processed/department_weekly_joined.csv")

# ---- Export 2: store-level weekly totals (for simpler forecasting) ----
store_weekly <- df %>%
  group_by(store, date, year, month, week, quarter, month_fac, is_holiday, type, size) %>%
  summarize(total_weekly_sales = sum(weekly_sales, na.rm = TRUE),
            temperature = mean(temperature, na.rm = TRUE),
            fuel_price  = mean(fuel_price,  na.rm = TRUE),
            cpi         = mean(cpi,         na.rm = TRUE),
            unemployment = mean(unemployment, na.rm = TRUE),
            across(starts_with("markdown"), ~mean(.x, na.rm = TRUE)),
            .groups = "drop")

readr::write_rds(store_weekly, "data/processed/store_weekly_sales.rds")
readr::write_csv(store_weekly, "data/processed/store_weekly_sales.csv")

message("Cleaning complete. Files written to data/processed/:",
        "\n - department_weekly_joined.[rds/csv]",
        "\n - store_weekly_sales.[rds/csv]")
