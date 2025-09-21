# app/app.R
# Purpose: Public-facing Shiny app to summarize the project.
# Design notes (my choices):
# - I avoid heavy UI frameworks to keep dependencies light; a simple navbarPage is enough.
# - I compute a quick ARIMA forecast on-demand per store (fast enough for weekly series).
# - I rely on processed CSVs produced by earlier scripts so the app is decoupled from raw data.
# - I keep plots minimal and business-oriented; the README/report explains methods in detail.

# ---------------------------
# ---------------------------
# Packages (no installs at runtime)
# ---------------------------
# Optional: set a CRAN mirror anyway (harmless)
options(repos = c(CRAN = "https://cloud.r-project.org"))

suppressPackageStartupMessages({
  library(shiny)
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(scales)
  library(DT)
  library(forecast)
  library(tidyr)
  library(bslib)
  # no library(qs) needed since we call qs::qread() directly when present
})

# ---------------------------
# Data loading
# ---------------------------
# I expect these processed files from scripts/01_data_cleaning.R

read_any <- function(paths) {
  paths <- paths[file.exists(paths)]
  if (!length(paths)) stop("Processed dataset not found. Run scripts/01_data_cleaning.R first.")
  p <- paths[1]
  ext <- tools::file_ext(p)
  if (ext %in% c("csv","gz")) {
    out <- readr::read_csv(p, show_col_types = FALSE)
  } else if (ext == "rds") {
    out <- readr::read_rds(p)
  } else if (ext == "qs") {
    out <- qs::qread(p)
  } else stop("Unsupported file extension: ", ext)
  out
}

store_candidates <- c(
  file.path("data","processed","store_weekly_sales.csv"),
  file.path("data","processed","store_weekly_sales.csv.gz"),
  file.path("data","processed","store_weekly_sales.rds"),
  file.path("data","processed","store_weekly_sales.qs"),
  file.path("..","data","processed","store_weekly_sales.csv"),
  file.path("..","data","processed","store_weekly_sales.csv.gz"),
  file.path("..","data","processed","store_weekly_sales.rds"),
  file.path("..","data","processed","store_weekly_sales.qs")
)

dept_candidates <- c(
  file.path("data","processed","department_weekly_joined.csv"),
  file.path("data","processed","department_weekly_joined.csv.gz"),
  file.path("data","processed","department_weekly_joined.rds"),
  file.path("data","processed","department_weekly_joined.qs"),
  file.path("..","data","processed","department_weekly_joined.csv"),
  file.path("..","data","processed","department_weekly_joined.csv.gz"),
  file.path("..","data","processed","department_weekly_joined.rds"),
  file.path("..","data","processed","department_weekly_joined.qs")
)

store_weekly <- read_any(store_candidates) %>% dplyr::mutate(date = as.Date(date))
dept_weekly  <- read_any(dept_candidates)  %>% dplyr::mutate(date = as.Date(date))


# KPIs that don’t change during a session; I compute these once
kpi <- list(
  min_date  = min(store_weekly$date, na.rm = TRUE),
  max_date  = max(store_weekly$date, na.rm = TRUE),
  n_stores  = dplyr::n_distinct(store_weekly$store),
  n_weeks   = dplyr::n_distinct(store_weekly$date),
  total_usd = sum(store_weekly$total_weekly_sales, na.rm = TRUE)
)


# ---------------------------
# QA data loaders (prefer in-app copies; fall back to ../outputs/tables)
# ---------------------------
read_qa_csv <- function(fname) {
  cands <- c(file.path("data","qa",fname), file.path("..","outputs","tables",fname))
  hits  <- cands[file.exists(cands)]
  if (!length(hits)) return(NULL)
  readr::read_csv(hits[1], show_col_types = FALSE)
}

qa_model_by_store <- read_qa_csv("model_metrics_by_store.csv")      # holdout (ARIMA [+ Prophet if present])
qa_model_avg      <- read_qa_csv("model_metrics_avg.csv")
qa_bt_by_store    <- read_qa_csv("backtest_metrics_by_store.csv")   # tsCV (ARIMA)
qa_bt_by_hz       <- read_qa_csv("backtest_metrics_by_horizon.csv")


# ---------------------------
# UI
# ---------------------------
ui <- shiny::navbarPage(
  title = "Retail Sales Analytics & Forecasting",
  id = "nav",
  collapsible = TRUE,
  theme = bslib::bs_theme(bootswatch = "flatly"),
  header = tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"),
    # Only: background image presence check for gradient fallback
    tags$script(HTML("
      (function(){
        var img = new Image();
        img.onload = function(){ document.body.classList.remove('no-bg'); };
        img.onerror = function(){ document.body.classList.add('no-bg'); };
        img.src = 'bg-retail.jpg';
      })();
    "))
  ),
  
  # ---- Overview ----
  tabPanel("Overview",
           fluidPage(
             br(),
             div(class = "section-card",
                 fluidRow(
                   column(3, div(h4("Date range"),  p(paste(kpi$min_date, "to", kpi$max_date)))),
                   column(3, div(h4("Stores"),      p(kpi$n_stores))),
                   column(3, div(h4("Weeks"),       p(kpi$n_weeks))),
                   column(3, div(h4("Total Sales"), p(scales::dollar(kpi$total_usd))))
                 )
             ),
             div(class = "section-card",
                 fluidRow(
                   column(8, h4("Overall Weekly Sales Trend"), plotOutput("p_trend", height = 360)),
                   column(4, h4("Top 10 Stores by Total Sales"), plotOutput("p_topstores", height = 360))
                 )
             )
           )
  ),
  
  # ---- Explore ----
  tabPanel("Explore",
           fluidPage(
             br(),
             div(class = "section-card",
                 fluidRow(
                   column(3, selectInput("store_sel", "Store(s)",
                                         choices = sort(unique(store_weekly$store)),
                                         selected = sort(unique(store_weekly$store))[1],
                                         multiple = TRUE)),
                   column(3, uiOutput("dept_ui")),
                   column(4, dateRangeInput("date_rng", "Date range",
                                            start = kpi$min_date, end = kpi$max_date,
                                            min = kpi$min_date,  max = kpi$max_date, weekstart = 1))
                 )
             ),
             div(class = "section-card",
                 fluidRow(
                   column(12, h4("Filtered Sales by Department"),
                          plotOutput("p_dept_ts", height = 380))
                 )
             )
           )
  ),
  
  # ---- Forecast ----
  tabPanel("Forecast",
           fluidPage(
             br(),
             div(class = "section-card",
                 fluidRow(
                   column(3, selectInput("store_fc", "Store",
                                         choices = sort(unique(store_weekly$store)))),
                   column(3, sliderInput("h_fc", "Forecast horizon (weeks)",
                                         min = 4, max = 26, value = 12, step = 1)),
                   column(4, helpText("Fits an ARIMA model on the store history and forecasts the next h weeks."))
                 )
             ),
             div(class = "section-card",
                 fluidRow(
                   column(12, h4("Actual (recent) vs Forecast (future)"),
                          plotOutput("p_forecast", height = 420))
                 )
             )
           )
  ),
  
  # ---- Model QA ----
  tabPanel("Model QA",
           fluidPage(
             br(),
             div(class = "section-card",
                 fluidRow(
                   column(6, h4("Holdout metrics (last 12 weeks)"),
                          tableOutput("qa_holdout_avg")),
                   column(6, h4("Rolling-origin CV (1..12 weeks)"),
                          tableOutput("qa_backtest_avg"))
                 )
             ),
             div(class = "section-card",
                 fluidRow(
                   column(7, h4("RMSE by forecast horizon"),
                          plotOutput("qa_horizon_plot", height = 360)),
                   column(5, h4("Per-store holdout metrics"),
                          div(style = "font-size: 12px;", DT::DTOutput("qa_holdout_table")))
                 )
             ),
             div(class = "section-card",
                 fluidRow(
                   column(3,
                          h4("Audit a store"),
                          selectInput("qa_store", "Store",
                                      choices = sort(unique(store_weekly$store)),
                                      selected = sort(unique(store_weekly$store))[1]),
                          helpText("Recomputes a 12-week ARIMA holdout for the selected store.")
                   ),
                   column(9,
                          h4("Holdout: actual vs ARIMA prediction (last 12 weeks)"),
                          plotOutput("qa_holdout_plot", height = 380))
                 )
             )
           )
  ),
  
  # ---- Data ----
  tabPanel("Data",
           fluidPage(
             br(),
             div(class = "section-card",
                 h4("Store-week data (processed)"),
                 DTOutput("tbl_store"),
                 br(),
                 downloadButton("dl_store_csv", "Download filtered CSV")
             )
           )
  )
)




# ---------------------------
# Server
# ---------------------------
server <- function(input, output, session) {
  
  # ---- Overview plots ----
  output$p_trend <- renderPlot({
    overall <- store_weekly %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(total_sales = sum(total_weekly_sales, na.rm = TRUE), .groups = "drop")
    ggplot(overall, aes(date, total_sales)) +
      geom_line(linewidth = 0.6) +
      scale_y_continuous(labels = scales::label_dollar()) +
      labs(x = "Week", y = "Total Weekly Sales (USD)") +
      theme_minimal(base_size = 12)
  })
  
  output$p_topstores <- renderPlot({
    top_stores <- store_weekly %>%
      dplyr::group_by(store) %>%
      dplyr::summarise(total = sum(total_weekly_sales, na.rm = TRUE), .groups = "drop") %>%
      dplyr::slice_max(order_by = total, n = 10) %>%
      dplyr::mutate(store = reorder(factor(store), total))
    ggplot(top_stores, aes(store, total)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = scales::label_dollar()) +
      labs(x = "Store", y = "Total Sales (USD)") +
      theme_minimal(base_size = 12)
  })
  
  # ---- Explore: dependent department choices ----
  output$dept_ui <- renderUI({
    # I derive departments available within the selected stores for a clean UX.
    depts <- dept_weekly %>% dplyr::filter(store %in% input$store_sel) %>% dplyr::pull(dept) %>% unique() %>% sort()
    selectInput("dept_sel", "Department(s)", choices = depts, selected = head(depts, 3), multiple = TRUE)
  })
  
  # Explore: time series by department (aggregated over selected stores)
  output$p_dept_ts <- renderPlot({
    req(input$store_sel, input$dept_sel, input$date_rng)
    df <- dept_weekly %>%
      dplyr::filter(store %in% input$store_sel,
                    dept  %in% input$dept_sel,
                    date  >= input$date_rng[1],
                    date  <= input$date_rng[2]) %>%
      dplyr::group_by(date, dept) %>%
      dplyr::summarise(weekly_sales = sum(weekly_sales, na.rm = TRUE), .groups = "drop")
    ggplot(df, aes(date, weekly_sales, color = factor(dept))) +
      geom_line() +
      scale_y_continuous(labels = scales::label_dollar()) +
      labs(x = "Week", y = "Weekly Sales (USD)", color = "Dept") +
      theme_minimal(base_size = 12)
  })
  
  # ---- Forecast: on-demand ARIMA per store ----
  output$p_forecast <- renderPlot({
    req(input$store_fc, input$h_fc)
    h <- as.integer(input$h_fc)
    
    # I use store-week totals; this keeps the signal strong and the UI snappy.
    df <- store_weekly %>%
      dplyr::filter(store == input$store_fc) %>%
      dplyr::arrange(date)
    
    validate(need(nrow(df) >= h + 10, "Not enough history to forecast."))
    
    # Train/test split only for the plot: I show last 26 weeks of actuals for context
    recent_n <- min(26, nrow(df))
    recent_actual <- tail(df, recent_n)
    
    y_ts <- stats::ts(df$total_weekly_sales, frequency = 52)
    fit  <- forecast::auto.arima(y_ts, stepwise = TRUE, approximation = FALSE)
    fc   <- forecast::forecast(fit, h = h)
    
    # Build a tidy frame for ggplot: recent actuals + future predictions
    future_dates <- seq(max(df$date) + 7, by = 7, length.out = h)
    plot_df <- dplyr::bind_rows(
      tibble::tibble(date = recent_actual$date, series = "Actual", value = recent_actual$total_weekly_sales),
      tibble::tibble(date = future_dates,     series = "Forecast", value = as.numeric(fc$mean))
    )
    
    ggplot(plot_df, aes(date, value, color = series)) +
      geom_line(linewidth = 0.8) +
      scale_y_continuous(labels = scales::label_dollar()) +
      labs(x = "Week", y = "Weekly Sales (USD)") +
      theme_minimal(base_size = 12)
  })
  
  # ---- Model QA: summary tables ----
  output$qa_holdout_avg <- renderTable({
    validate(need(!is.null(qa_model_avg), "No holdout summary found."))
    qa_model_avg
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  output$qa_backtest_avg <- renderTable({
    validate(need(!is.null(qa_bt_by_store), "No backtest summary found."))
    # If you saved backtest_metrics_avg.csv, prefer that; otherwise compute a quick overall:
    if (!is.null(qa_bt_by_store)) {
      data.frame(
        n_stores = nrow(qa_bt_by_store),
        RMSE = mean(qa_bt_by_store$RMSE, na.rm = TRUE),
        MAE  = mean(qa_bt_by_store$MAE,  na.rm = TRUE)
      )
    }
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  # ---- Model QA: horizon plot (tsCV) ----
  output$qa_horizon_plot <- renderPlot({
    validate(need(!is.null(qa_bt_by_hz), "No horizon file found. Run scripts/03b_backtesting.R."))
    df <- qa_bt_by_hz
    ggplot(df, aes(horizon, RMSE, group = store)) +
      geom_line(alpha = 0.25) +
      stat_summary(fun = mean, geom = "line", linewidth = 1) +
      labs(x = "Horizon (weeks ahead)", y = "RMSE") +
      theme_minimal(base_size = 12)
  })
  
  # ---- Model QA: per-store holdout table ----
  output$qa_holdout_table <- DT::renderDT({
    validate(need(!is.null(qa_model_by_store), "No per-store holdout table found."))
    DT::datatable(
      qa_model_by_store,
      options = list(pageLength = 8, scrollX = TRUE),
      filter = "top",
      rownames = FALSE
    )
  })
  
  # ---- Model QA: recompute a 12-week holdout for the chosen store ----
  output$qa_holdout_plot <- renderPlot({
    req(input$qa_store)
    h <- 12L
    df <- store_weekly %>%
      dplyr::filter(store == input$qa_store) %>%
      dplyr::arrange(date)
    
    validate(need(nrow(df) >= h + 10, "Not enough history to compute a holdout."))
    
    # train/test split
    all_weeks  <- sort(unique(df$date))
    test_weeks <- tail(all_weeks, h)
    train_df   <- df %>% dplyr::filter(date < min(test_weeks))
    test_df    <- df %>% dplyr::filter(date %in% test_weeks)
    
    y_ts <- stats::ts(train_df$total_weekly_sales, frequency = 52)
    fit  <- forecast::auto.arima(y_ts, stepwise = TRUE, approximation = FALSE)
    fc   <- forecast::forecast(fit, h = h)
    
    comp <- tibble::tibble(
      date   = test_df$date,
      Actual = test_df$total_weekly_sales,
      ARIMA  = as.numeric(fc$mean)
    ) %>% tidyr::pivot_longer(-date, names_to = "series", values_to = "value")
    
    ggplot(comp, aes(date, value, color = series)) +
      geom_line(linewidth = 0.8) +
      scale_y_continuous(labels = scales::label_dollar()) +
      labs(x = "Week", y = "Weekly Sales (USD)") +
      theme_minimal(base_size = 12)
  })
  
  # ---- Data tab: interactive table + download filtered CSV ----
  output$tbl_store <- renderDT({
    datatable(
      store_weekly,
      options = list(pageLength = 10, scrollX = TRUE),
      filter = "top",
      rownames = FALSE
    )
  })
  
  output$dl_store_csv <- downloadHandler(
    filename = function() sprintf("store_weekly_sales_%s.csv", Sys.Date()),
    content  = function(file) readr::write_csv(store_weekly, file)
  )
  
}

# ---------------------------
# Launch
# ---------------------------
shinyApp(ui, server)
