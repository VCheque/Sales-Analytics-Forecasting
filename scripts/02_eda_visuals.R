# scripts/02_eda_visuals.R
# Purpose: Load processed data and produce EDA tables/plots for the report and Shiny.

# ---- Packages ----
need <- c("readr","dplyr","tidyr","lubridate","ggplot2","scales","forcats","tibble","stringr")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
invisible(lapply(need, library, character.only = TRUE))

# ---- Paths ----
fig_dir <- "outputs/figures"
tab_dir <- "outputs/tables"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Read processed data (created in 01_data_cleaning.R) ----
store_weekly <- readr::read_csv("data/processed/store_weekly_sales.csv", show_col_types = FALSE)
dept_weekly  <- readr::read_csv("data/processed/department_weekly_joined.csv", show_col_types = FALSE)

# Ensure date types
store_weekly <- store_weekly %>% mutate(date = as.Date(date))
dept_weekly  <- dept_weekly  %>% mutate(date = as.Date(date))

# ---- Quick KPIs (saved as a small CSV for README/report) ----
kpis <- tibble(
  min_date = min(store_weekly$date, na.rm = TRUE),
  max_date = max(store_weekly$date, na.rm = TRUE),
  n_stores = n_distinct(store_weekly$store),
  n_depts  = n_distinct(dept_weekly$dept),
  total_sales = sum(store_weekly$total_weekly_sales, na.rm = TRUE)
)
readr::write_csv(kpis, file.path(tab_dir, "kpis.csv"))

# ---- Missingness summary (numeric columns) ----
miss_tbl <- dept_weekly %>%
  summarize(across(everything(), ~ sum(is.na(.x)))) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "column", values_to = "n_missing") %>%
  arrange(desc(n_missing))

# ---- Overall weekly sales trend ----
overall <- store_weekly %>%
  group_by(date) %>%
  summarize(total_sales = sum(total_weekly_sales, na.rm = TRUE), .groups = "drop")

p_trend <- ggplot(overall, aes(date, total_sales)) +
  geom_line(linewidth = 0.6) +
  scale_y_continuous(labels = label_dollar()) +
  labs(title = "Overall Weekly Sales Trend",
       x = "Week",
       y = "Total Weekly Sales (USD)") +
  theme_minimal(base_size = 12)
# p_trend - remove # to view the plot/image
ggsave(file.path(fig_dir, "01_overall_weekly_sales_trend.png"), p_trend, width = 10, height = 5, dpi = 180)

# ----  Top 10 stores by total sales ----
top_stores <- store_weekly %>%
  group_by(store) %>%
  summarize(total = sum(total_weekly_sales, na.rm = TRUE), .groups = "drop") %>%
  slice_max(order_by = total, n = 10) %>%
  mutate(store = fct_reorder(as.factor(store), total))

p_topstores <- ggplot(top_stores, aes(store, total)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = label_dollar()) +
  labs(title = "Top 10 Stores by Total Sales",
       x = "Store",
       y = "Total Sales (USD)") +
  theme_minimal(base_size = 12)
ggsave(file.path(fig_dir, "02_top10_stores_total_sales.png"), p_topstores, width = 7, height = 5, dpi = 180)
# p_topstores - remove # to view the plot/image

# ----  Seasonality heatmap (Monthly totals by year) ----
monthly <- store_weekly %>%
  mutate(year = year(date), month = month(date, label = TRUE, abbr = TRUE)) %>%
  group_by(year, month) %>%
  summarize(monthly_sales = sum(total_weekly_sales, na.rm = TRUE), .groups = "drop")

p_season <- ggplot(monthly, aes(x = month, y = factor(year), fill = monthly_sales)) +
  geom_tile() +
  scale_fill_continuous(labels = label_dollar()) +
  labs(title = "Seasonality Heatmap: Monthly Sales by Year",
       x = "Month", y = "Year", fill = "Sales") +
  theme_minimal(base_size = 12)
ggsave(file.path(fig_dir, "03_seasonality_heatmap_month_year.png"), p_season, width = 9, height = 5, dpi = 180)
p_season


# ---- Promotions vs sales (markdown intensity vs sales) ----
promo <- dept_weekly %>%
  mutate(markdown_total = rowSums(select(., starts_with("markdown")), na.rm = TRUE)) %>%
  group_by(store, date) %>%
  summarize(total_weekly_sales = sum(weekly_sales, na.rm = TRUE),
            markdown_total = sum(markdown_total, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(promo_bin = ntile(markdown_total, 10)) %>%
  group_by(promo_bin) %>%
  summarize(avg_sales = mean(total_weekly_sales, na.rm = TRUE),
            avg_markdown = mean(markdown_total, na.rm = TRUE),
            .groups = "drop")

p_promo <- ggplot(promo, aes(avg_markdown, avg_sales)) +
  geom_point() + geom_smooth(method = "loess", se = FALSE) +
  scale_y_continuous(labels = label_dollar()) +
  labs(title = "Promotion Intensity vs Average Weekly Sales",
       x = "Average Markdown (proxy for promotion)",
       y = "Average Weekly Sales (USD)") +
  theme_minimal(base_size = 12)
ggsave(file.path(fig_dir, "04_promotions_vs_sales.png"), p_promo, width = 8, height = 5, dpi = 180)
p_promo

# ----  Holiday effect (distribution) ----
p_holiday <- store_weekly %>%
  mutate(is_holiday = ifelse(is_holiday, "Holiday Week", "Non-Holiday Week")) %>%
  ggplot(aes(is_holiday, total_weekly_sales)) +
  geom_boxplot(outlier.alpha = 0.2) +
  scale_y_continuous(labels = label_dollar()) +
  labs(title = "Holiday vs Non-Holiday Weekly Sales",
       x = "", y = "Total Weekly Sales (USD)") +
  theme_minimal(base_size = 12)
ggsave(file.path(fig_dir, "05_holiday_effect_boxplot.png"), p_holiday, width = 6, height = 5, dpi = 180)
p_holiday

# ---- Correlation heatmap (numeric drivers) ----
num_cols <- c("total_weekly_sales","temperature","fuel_price","cpi","unemployment",
              grep("^markdown", names(dept_weekly), value = TRUE, ignore.case = TRUE))

# Aggregate dept -> store-week so we can align with total_weekly_sales
dept_agg <- dept_weekly %>%
  group_by(store, date) %>%
  summarize(across(all_of(grep("^markdown", names(dept_weekly), value = TRUE)), ~mean(.x, na.rm = TRUE)),
            .groups = "drop")

drivers <- store_weekly %>%
  select(store, date, total_weekly_sales, temperature, fuel_price, cpi, unemployment) %>%
  left_join(dept_agg, by = c("store","date")) %>%
  select(all_of(intersect(num_cols, names(.))))

cmat <- cor(drivers, use = "pairwise.complete.obs")
corr_long <- as.data.frame(cmat) |>
  tibble::rownames_to_column("var1") |>
  pivot_longer(-var1, names_to = "var2", values_to = "corr")

p_corr <- ggplot(corr_long, aes(var1, var2, fill = corr)) +
  geom_tile() +
  scale_fill_gradient2(limits = c(-1, 1), oob = scales::squish) +
  labs(title = "Correlation Heatmap: Sales vs Drivers", x = "", y = "", fill = "r") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(fig_dir, "06_correlation_heatmap.png"), p_corr, width = 8.5, height = 7, dpi = 180)
p_corr

# message("EDA complete. Figures in outputs/figures and tables in outputs/tables.")
