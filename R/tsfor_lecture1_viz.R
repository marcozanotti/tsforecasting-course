# Time Series Forecasting: Machine Learning and Deep Learning with R & Python ----

# Lecture 1: Manipulation, Transformation & Visualization -----------------
# Marco Zanotti

# Goals:
# - Learn timetk data wrangling functionality
# - Commonly used time series transformations
# - Commonly used time series visualizations



# Packages ----------------------------------------------------------------

source("R/utils.R")
source("R/packages.R")



# Data --------------------------------------------------------------------

analytics_tbl <- read_rds("data/analytics_hourly.rds")
subscribers_tbl <- read_rds("data/subscribers.rds")
events_tbl <- read_rds("data/events.rds")



# Manipulation ------------------------------------------------------------

# * Summarize by Time -----------------------------------------------------

# - Apply commonly used aggregations
# - High-to-Low Frequency

# to daily
subscribers_daily_tbl <- subscribers_tbl |>
  summarise_by_time(optin_time, .by = "day", optins = n())

subscribers_tbl |>
  group_by(member_rating) |>
  summarise_by_time(optin_time, .by = "day", optins = n()) |>
  pivot_wider(names_from = member_rating, values_from = optins)

analytics_daily_tbl <- analytics_tbl |>
  mutate(date = ymd_h(dateHour), .before = everything()) |>
  select(-dateHour) |>
  summarise_by_time(date, .by = "day", across(pageViews:sessions, .fns = sum))

# to weekly
subscribers_tbl |>
  summarize_by_time(optin_time, .by = "week", optins = n())

# to monthly
subscribers_tbl |>
  summarize_by_time(optin_time, .by = "month", optins = n())


# * Pad by Time -----------------------------------------------------------

# - Filling in time series gaps
# - Low-to-High Frequency (un-aggregating)

# fill daily gaps
subscribers_daily_tbl |>
  pad_by_time(.date_var = optin_time, .by = "day", .pad_value = 0, .start_date = "2018-06-01")

# weekly to daily
subscribers_daily_tbl |>
  pad_by_time(optin_time, .by = "day", .start_date = "2018-06") |>
  mutate_by_time(.by = "week", optins = sum(optins, na.rm = TRUE) / 7)


# * Filter by Time --------------------------------------------------------

# - Pare data down before modeling

subscribers_daily_tbl |>
  filter_by_time(.start_date = "2018-11-20")

subscribers_daily_tbl |>
  filter_by_time(.start_date = "2019-12", .end_date = "2019")

subscribers_daily_tbl |>
  filter_by_time(.start_date = "2019-12", .end_date = "2019-12-01" %+time% "4 weeks")


# * Mutate by Time --------------------------------------------------------

# - Get change from beginning/end of period

# first, last, mean, median by period
subscribers_daily_tbl |>
  mutate_by_time(
    .by = "1 week",
    optins_mean = mean(optins),
    optins_median = median(optins),
    optins_max = max(optins),
    optins_min = min(optins)
  )


# * Join by Time ----------------------------------------------------------

# - Investigating Relationships
# - Identify External Regressors

subscribers_google_joined_tbl <- subscribers_daily_tbl |>
  pad_by_time(.date_var = optin_time, .pad_value = 0, .start_date = "2018-06") |>
  left_join(analytics_daily_tbl, by = c("optin_time" = "date"))

# inspect join
subscribers_google_joined_tbl |> plot_missing()
subscribers_google_joined_tbl |> tk_summary_diagnostics()

# plot relationships
subscribers_google_joined_tbl |>
  pivot_longer(-optin_time) |>
  plot_time_series(optin_time, value, .color_var = name, .smooth = FALSE)

subscribers_google_joined_tbl |>
  drop_na() |>
  mutate(across(optins:sessions, .fns = log1p)) |>
  mutate(across(optins:sessions, .fns = standardize_vec)) |>
  pivot_longer(optins:sessions) |>
  plot_time_series(optin_time, value, name, .smooth = FALSE)


# * Future Frame ----------------------------------------------------------

# - Forecasting helper

subscribers_day_tbl <- subscribers_tbl |>
  summarise_by_time(.date_var = optin_time, .by = "day", optins = n()) |>
  pad_by_time(.date_var = optin_time, .by = "day", .pad_value = 0)

subscribers_daily_tbl |>
  future_frame(.length_out = 10)

# modelling example on date features
model_fit_lm <- lm(
  optins ~ as.numeric(optin_time) + wday(optin_time, label = TRUE),
  data = subscribers_daily_tbl
)

future_tbl <- subscribers_daily_tbl |>
  future_frame(.length_out = "2 months")

predictions_vec <- predict(model_fit_lm, newdata = future_tbl) |> as.vector()

subscribers_daily_tbl |>
  select(optin_time, optins) |>
  add_column(type = "actual") |>
  bind_rows(
    future_tbl |>
      mutate(optins = predictions_vec, type = "prediction")
  ) |>
  plot_time_series(optin_time, optins, type, .smooth = FALSE)



# Transformation ----------------------------------------------------------

subscribers_daily_tbl <- subscribers_tbl |>
  summarise_by_time(.date_var = optin_time, .by = "day", optins = n()) |>
  pad_by_time(.date_var = optin_time, .by = "day", .pad_value = 0)

# * Variance Reduction ----------------------------------------------------

# Log
subscribers_daily_tbl |>
  mutate(optins = log(optins))

# Log + 1
subscribers_daily_tbl |>
  mutate(optins = log1p(optins)) |>
  plot_time_series(optin_time, optins)

# - inversion with exp() and expm1()

# Box-Cox
subscribers_daily_tbl |>
  mutate(optins = box_cox_vec(optins + 1, lambda = "auto")) |>
  plot_time_series(optin_time, optins)

# - inversion with box_cox_inv_vec()


# * Range Reduction -------------------------------------------------------

# - Used in visualization to overlay series
# - Used in ML for models that are affected by feature magnitude (e.g. linear regression)

# Normalization Range (0,1)
analytics_daily_tbl |>
  pivot_longer(-date) |>
  group_by(name) |>
  mutate(value = normalize_vec(value)) |>
  plot_time_series(date, value)

# Standardization
analytics_daily_tbl |>
  pivot_longer(-date) |>
  group_by(name) |>
  mutate(value = standardize_vec(value)) |>
  plot_time_series(date, value)


# * Smoothing -------------------------------------------------------------

# - Identify trends and cycles
# - Clean seasonality

subscribers_daily_tbl |>
  mutate(
    optins = log1p(optins),
    optins_smooth = smooth_vec(optins, period = 24 * 7, degree = 0)
  ) |>
  pivot_longer(-optin_time) |>
  plot_time_series(optin_time, value, .color_var = name, .smooth = FALSE)

analytics_daily_tbl |>
  pivot_longer(-date) |>
  group_by(name) |>
  mutate(value_smooth = smooth_vec(value, period = 24 * 7, degree = 0)) |>
  pivot_longer(contains("value"), names_repair = "unique") |>
  rename(name = `name...2`, type = `name...3`) |>
  group_by(name) |>
  plot_time_series(date, value, .color_var = type, .smooth = FALSE)


# * Rolling Averages ------------------------------------------------------

# - Common time series operations to visualize trend
# - A simple transformation that can help create improve features
# - Can help with outlier-effect reduction & trend detection
# - Note: Businesses often use a rolling average as a forecasting technique
# A rolling average forecast is usually sub-optimal (good opportunity for you!).

analytics_daily_tbl |>
  pivot_longer(-date) |>
  group_by(name) |>
  mutate(
    value_roll = slidify_vec(
      value,
      .f = mean,
      .period = 24 * 7, # 6 months = 7 days * 24 weeks
      .align = "center",
      .partial = TRUE
    )
  ) |>
  pivot_longer(contains("value"), names_repair = "unique") |>
  rename(name = `name...2`, type = `name...3`) |>
  group_by(name) |>
  plot_time_series(date, value, .color_var = type, .smooth = FALSE)


# * Missing Values Imputation ---------------------------------------------

# - Imputation helps with filling gaps (if needed)

subscribers_daily_tbl |>
  mutate(optins_na = ifelse(optins == 0, NA, optins)) |>
  mutate(optins_imputed = ts_impute_vec(optins_na, period = 7)) |>
  pivot_longer(-optin_time) |>
  plot_time_series(optin_time, log1p(value), .color_var = name, .smooth = FALSE)


# * Anomaly Cleaning ------------------------------------------------------

# - Outlier removal helps linear regression detect trend and reduces high leverage points
# WARNING: Make sure you check outliers against events
# - usually there is a reason for large values

# Anomaly detection
subscribers_daily_tbl |>
  plot_anomaly_diagnostics(optin_time, optins)

subscribers_daily_tbl |>
  plot_anomaly_diagnostics(optin_time, log1p(optins))

subscribers_cleaned_daily_tbl <- subscribers_daily_tbl |>
  mutate(
    optins_log = log1p(optins),
    optins_cleaned = ts_clean_vec(optins, period = 7),
    optins_log_cleaned = ts_clean_vec(optins_log, period = 7)
  )

subscribers_cleaned_daily_tbl |>
  pivot_longer(-optin_time) |>
  mutate(
    cleaned = ifelse(str_detect(name, "cleaned"), "cleaned", "level"),
    type = ifelse(str_detect(name, "log"), "log", "level")
  ) |>
  plot_time_series(optin_time, value, cleaned, .facet_vars = type, .smooth = FALSE)

# without log
# outlier effect - before cleaning
subscribers_cleaned_daily_tbl |>
  plot_time_series_regression(
    optin_time,
    .formula = optins ~ as.numeric(optin_time) +
      lubridate::wday(optin_time, label = TRUE) +
      lubridate::month(optin_time, label = TRUE),
    .show_summary = TRUE
  )

# outlier effect - after cleaning
subscribers_cleaned_daily_tbl |>
  plot_time_series_regression(
    optin_time,
    .formula = optins_cleaned ~ as.numeric(optin_time) +
      lubridate::wday(optin_time, label = TRUE) +
      lubridate::month(optin_time, label = TRUE),
    .show_summary = TRUE
  )

# with log
# outlier effect - before cleaning
subscribers_cleaned_daily_tbl |>
  plot_time_series_regression(
    optin_time,
    .formula = optins_log ~ as.numeric(optin_time) +
      lubridate::wday(optin_time, label = TRUE) +
      lubridate::month(optin_time, label = TRUE),
    .show_summary = TRUE
  )

# outlier effect - after cleaning
subscribers_cleaned_daily_tbl |>
  plot_time_series_regression(
    optin_time,
    .formula = optins_log_cleaned ~ as.numeric(optin_time) +
      lubridate::wday(optin_time, label = TRUE) +
      lubridate::month(optin_time, label = TRUE),
    .show_summary = TRUE
  )


# * Lags & Differencing ---------------------------------------------------

# - Lags: Often used for feature engineering
# - Lags: Autocorrelation
# - MOST IMPORTANT: Can possibly use lagged variables in a model, if lags are correlated
# - Difference: Used to go from growth to change
# - Difference: Makes a series "stationary" (potentially)

# lags
subscribers_daily_tbl |>
  mutate(optins_lag_1 = lag_vec(optins, lag = 1))

subscribers_daily_tbl |>
  tk_augment_lags(.value = optins, .lags = c(1, 2, 7, 14))

# differencing
analytics_daily_tbl |>
  mutate(across(pageViews:sessions, .fns = diff_vec)) |>
  pivot_longer(-date) |>
  plot_time_series(date, value, name, .smooth = FALSE)


# * Fourier Transform ------------------------------------------------------

# - Useful for incorporating seasonality & autocorrelation
# - BENEFIT: Don't need a lag, just need a frequency (based on your time index)

# single fourier series
subscribers_daily_tbl |>
  mutate(sin14_k1 = fourier_vec(optin_time, period = 14, K = 1, type = "sin")) |>
  mutate(cos14_k1 = fourier_vec(optin_time, period = 14, K = 1, type = "cos")) |>
  select(-optins) |>
  pivot_longer(matches("(cos)|(sin)")) |>
  plot_time_series(optin_time, value, name, .smooth = FALSE)

# multiple fourier series
subscribers_daily_tbl |>
  tk_augment_fourier(optin_time, .periods = c(14, 30, 90, 365), .K = 2) |>
  plot_time_series_regression(
    optin_time,
    .formula = log1p(optins) ~ as.numeric(optin_time) + . - optin_time,
    .show_summary = TRUE
  )


# * Confined Interval -----------------------------------------------------

# - Transformation used to confine forecasts to a max/min interval

subscribers_daily_tbl |>
  plot_time_series(
    optin_time,
    log_interval_vec(optins, limit_lower = 0, offset = 1)
  )



# Visualization -----------------------------------------------------------

subscribers_day_tbl <- subscribers_tbl |>
  summarise_by_time(.date_var = optin_time, .by = "day", optins = n()) |>
  pad_by_time(.by = "day", .pad_value = 0)

analytics_long_hour_tbl <- analytics_tbl |>
  mutate(date = ymd_h(dateHour), .before = everything()) |>
  select(-dateHour) |>
  pivot_longer(cols = pageViews:sessions)


# * Time Series Plot ------------------------------------------------------

subscribers_day_tbl |>
  plot_time_series(optin_time, optins, .smooth = FALSE)

analytics_long_hour_tbl |>
  plot_time_series(date, value, .color_var = name, .smooth = FALSE)

analytics_long_hour_tbl |>
  plot_time_series(date, value, .color_var = name, .facet_vars = name, .smooth = FALSE)

# Log Transforms
subscribers_day_tbl |>
  plot_time_series(optin_time, log(optins + 1))

analytics_long_hour_tbl |>
  plot_time_series(date, log(value + 1), .color_var = name, .facet_vars = name)


# * Autocorrelation Function (ACF) Plot -----------------------------------

subscribers_day_tbl |>
  plot_acf_diagnostics(optin_time, log(optins + 1), .lags = 10, .show_white_noise_bars = TRUE)

subscribers_day_tbl |>
  plot_acf_diagnostics(optin_time, log(optins + 1), .lags = 500, .show_white_noise_bars = TRUE)


# * Cross-Correlation Function (CCF) Plot ---------------------------------

subscribers_ga_day_tbl <- subscribers_day_tbl |>
  left_join(
    analytics_long_hour_tbl |>
      pivot_wider(names_from = name, values_from = value) |>
      summarise_by_time(
        .date_var = date,
        .by = "day",
        across(pageViews:sessions, .fns = sum)
      ),
    by = c("optin_time" = "date")
  )

subscribers_ga_day_tbl |>
  drop_na() |>
  plot_acf_diagnostics(
    optin_time,
    optins,
    .ccf_vars = pageViews:sessions,
    .lags = 100,
    .show_white_noise_bars = TRUE,
    .show_ccf_vars_only = TRUE,
    .facet_ncol = 3,
  )


# * Smoothing Plot --------------------------------------------------------

subscribers_day_tbl |>
  plot_time_series(
    optin_time,
    log(optins + 1),
    .smooth_period = "90 days",
    .smooth_degree = 1
  )


# * Boxplots --------------------------------------------------------------

subscribers_day_tbl |>
  plot_time_series_boxplot(
    optin_time,
    log(optins + 1),
    .period = "1 month"
  )

subscribers_day_tbl |>
  plot_time_series_boxplot(
    optin_time,
    log(optins + 1),
    .period = "7 days",
    .smooth = TRUE,
    .smooth_func = median, # change smoother
    .color_var = lubridate::year(optin_time)
  )


# * Seasonality Plot ------------------------------------------------------

subscribers_day_tbl  |>
  plot_seasonal_diagnostics(optin_time, log(optins + 1))


# * Decomposition Plot ----------------------------------------------------

subscribers_day_tbl |>
  plot_stl_diagnostics(optin_time, log(optins + 1))


# * Anomaly Detection Plot ------------------------------------------------

subscribers_day_tbl |>
  tk_anomaly_diagnostics(optin_time, optins, .alpha = .01, .max_anomalies = .01)

subscribers_day_tbl |>
  plot_anomaly_diagnostics(optin_time, optins, .alpha = .01, .max_anomalies = .01)


# * Time Series Regression Plot -------------------------------------------

subscribers_day_tbl |>
  plot_time_series_regression(
    optin_time,
    log(optins + 1) ~
      as.numeric(optin_time) + # linear trend
      lubridate::wday(optin_time, label = TRUE) + # week day calendar features
      lubridate::month(optin_time, label = TRUE), # month calendar features
    .show_summary = TRUE
  )

