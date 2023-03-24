# Time Series Forecasting: Machine Learning and Deep Learning with R & Python ----

# Lecture 2: Features Engineering & Recipes -------------------------------
# Marco Zanotti

# Goals:
# - Learn advanced features engineering workflows & techniques
# - Learn how to use recipes

# Challenges:
# - Challenge 1 - Feature Engineering



# Packages ----------------------------------------------------------------

source("R/utils.R")
source("R/packages.R")



# Data --------------------------------------------------------------------

subscribers_tbl <- read_rds("data/subscribers.rds")
analytics_tbl <- read_rds("data/analytics_hourly.rds")
events_tbl <- read_rds("data/events.rds")



# Features Engineering ----------------------------------------------------

# Pre-processing Data

# subscribers data
subscribers_daily_tbl <- subscribers_tbl %>%
  summarise_by_time(optin_time, .by = "day", optins = n()) %>%
  pad_by_time(.pad_value = 0)

subscribers_daily_tbl %>%
  plot_time_series(optin_time, log1p(optins), .smooth = FALSE)

data_prep_tbl <- subscribers_daily_tbl %>%
  # pre-processing
  mutate(optins_trans = log_interval_vec(optins, limit_lower = 0, offset = 1)) %>%
  mutate(optins_trans = standardize_vec(optins_trans)) %>%
  # fix missing values at beginning of series
  filter_by_time(.start_date = "2018-07-03") %>%
  # Cleaning
  mutate(optins_trans_cleaned = ts_clean_vec(optins_trans, period = 7)) %>%
  mutate(optins_trans = ifelse(
    optin_time %>% between_time("2018-11-18", "2018-11-20"),
    optins_trans_cleaned,
    optins_trans)
  ) %>%
  select(-optins, -optins_trans_cleaned)

data_prep_tbl %>%
  plot_time_series(optin_time, optins_trans)

# google analytics
analytics_prep_tbl <- analytics_tbl %>%
  mutate(date = ymd_h(dateHour), .before = everything()) %>%
  summarise_by_time(date, .by = "day", across(pageViews:sessions, .fns = sum)) %>%
  mutate(across(pageViews:sessions, .fns = log1p)) %>%
  mutate(across(pageViews:sessions, .fns = standardize_vec))

analytics_prep_tbl %>%
  pivot_longer(-date) %>%
  plot_time_series(date, .value = value, .facet_nrow = 3, .facet_vars = name)

# events data
events_daily_tbl <- events_tbl %>%
  mutate(event_date = ymd_hms(event_date)) %>%
  summarise_by_time(event_date, .by = "day", event = n())

events_daily_tbl %>%
  pad_by_time(.pad_value = 0) %>%
  plot_time_series(event_date, event, .smooth = FALSE)


# * Time-Based Features ---------------------------------------------------

# - tk_augment_timeseries_signature()

data_prep_signature_tbl <- data_prep_tbl %>%
  tk_augment_timeseries_signature() %>%
  select(
    -diff, -ends_with("iso"), -ends_with(".xts"), -contains("hour"),
    -contains("minute"), -contains("second"), -contains("am.pm")
  )
data_prep_signature_tbl %>% glimpse()


# * Trend-Based Features --------------------------------------------------

# linear trend
data_prep_signature_tbl %>%
  plot_time_series_regression(optin_time, .formula = optins_trans ~ index.num)

# nonlinear trend - basis splines
data_prep_signature_tbl %>%
  plot_time_series_regression(
    optin_time,
    optins_trans ~ splines::bs(index.num, df = 3),
    .show_summary = TRUE
  )

# nonlinear trend - natural splines
data_prep_signature_tbl %>%
  plot_time_series_regression(
    optin_time,
    optins_trans ~ splines::ns(index.num, knots = quantile(index.num, probs = c(0.25, 0.5))),
    .show_summary = TRUE
  )


# * Seasonal Features -----------------------------------------------------

# weekly seasonality
data_prep_signature_tbl %>%
  plot_time_series_regression(optin_time, optins_trans ~ wday.lbl, .show_summary = TRUE)

# monthly seasonality
data_prep_signature_tbl %>%
  plot_time_series_regression(optin_time, optins_trans ~ month.lbl, .show_summary = TRUE)


# * Interaction Features --------------------------------------------------

data_prep_signature_tbl %>%
  plot_time_series_regression(
    optin_time,
    optins_trans ~ (as.factor(week2) * wday.lbl),
    .show_summary = TRUE
  )


# * Rolling Average Features ----------------------------------------------

# - tk_augment_slidify

data_prep_rolls_tbl <- data_prep_tbl %>%
  tk_augment_slidify(
    optins_trans, mean,
    .period = c(7, 14, 30, 90),
    .align = "center",
    .partial = TRUE
  )
data_prep_rolls_tbl %>% glimpse()

data_prep_rolls_tbl %>%
  plot_time_series_regression(optin_time, optins_trans ~ ., .show_summary = TRUE)


# * Lag Features ----------------------------------------------------------

# tk_augment_lags

data_prep_tbl %>%
  plot_acf_diagnostics(optin_time, optins_trans, .lags = 100)

data_prep_lags_tbl <- data_prep_tbl %>%
  tk_augment_lags(optins_trans, .lags = c(1, 7, 14, 30, 90, 365)) %>%
  drop_na()
data_prep_lags_tbl %>% glimpse()

data_prep_lags_tbl %>%
  plot_time_series_regression(optin_time, optins_trans ~ ., .show_summary = TRUE)


# * Fourier Series Features -----------------------------------------------

# - tk_augment_fourier

data_prep_tbl %>%
  plot_acf_diagnostics(optin_time, optins_trans, .lags = 100)

data_prep_fourier_tbl <- data_prep_tbl %>%
  tk_augment_fourier(optin_time, .periods = c(1, 7, 14, 30, 90, 365), .K = 2)
data_prep_fourier_tbl %>% glimpse()

data_prep_fourier_tbl %>%
  plot_time_series_regression(optin_time, optins_trans ~ ., .show_summary = TRUE)


# * Event Data Features ---------------------------------------------------

events_daily_tbl %>% glimpse()

data_prep_events_tbl <- data_prep_tbl %>%
  left_join(events_daily_tbl, by = c("optin_time" = "event_date")) %>%
  mutate(event = ifelse(is.na(event), 0, event))
data_prep_events_tbl %>% glimpse()

data_prep_events_tbl %>%
  plot_time_series(optin_time, optins_trans, .smooth = FALSE, .interactive = FALSE) +
  geom_point(color = "red", data = . %>% filter(event == 1))

data_prep_events_tbl %>%
  plot_time_series_regression(optin_time, optins_trans ~ ., .show_summary = TRUE)


# * External Regressor Features -------------------------------------------

analytics_prep_tbl %>% glimpse()

data_prep_google_tbl <- data_prep_tbl %>%
  left_join(analytics_prep_tbl, by = c("optin_time" = "date")) %>%
  drop_na()
data_prep_google_tbl %>% glimpse()

data_prep_google_tbl %>%
  plot_acf_diagnostics(
    optin_time, optins_trans,
    .lags = 100,
    .ccf_vars = pageViews:sessions,
    .show_ccf_vars_only = TRUE
  )

data_prep_google_tbl <- data_prep_google_tbl %>%
  tk_augment_lags(pageViews:sessions, .lags = c(7, 42)) %>%
  drop_na()
data_prep_google_tbl %>% glimpse()

data_prep_google_tbl %>%
  plot_time_series_regression(optin_time, optins_trans ~ ., .show_summary = TRUE)



# Recipes -----------------------------------------------------------------

# Description of the steps to be applied to a data set to prepare it for analysis.
# based on steps
# may contain pre-processing steps
# may contain feature engineering steps

# Splitting Data
subscribers_daily_tbl %>% tk_summary_diagnostics()

splits <- time_series_split(subscribers_daily_tbl, assess = 7 * 8, cumulative = TRUE)
splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(optin_time, optins)

# Creating Recipe
recipe_spec_full <- recipe(optins ~ ., data = training(splits)) %>%

  # pre-processing steps
  step_log_interval(optins, limit_lower = 0, offset = 1) %>%
  step_normalize(optins) %>%
  step_filter(optin_time >= "2018-07-03") %>%
  step_ts_clean(optins, period = 7) %>%

  # features engineering steps
  # time-based, trend and seasonal features
  step_timeseries_signature(optin_time) %>%
  step_rm(matches("(iso)|(xts)|(hour)|(minute)|(second)|(am.pm)")) %>%
  step_normalize(matches("(index.num)|(year)|(yday)")) %>%
  step_dummy(all_nominal(), one_hot = TRUE) %>%
  step_ns(ends_with("index.num"), deg_free = 2) %>%
  # interaction features
  step_interact(~ matches("week2") * matches("wday.lbl")) %>%
  # rolling features
  step_slidify_augment(
    optins, .f = mean, period = c(7, 14, 30, 90), align = "center", partial = TRUE
  ) %>%
  # lag features
  # step_lag(optins, lag = 56) %>% # should remove NA's
  # fourier series features
  step_fourier(optin_time, period = c(7, 14, 30, 90), K = 2) %>%
  step_rm(optin_time)

# Note: cannot add event data or external regressors via recipe

recipe_spec_full
recipe_spec_full %>% prep() %>% juice() %>% glimpse()

# Fitting on Recipe & Calibrating
model_spec_lm <- linear_reg() %>%
  set_engine("lm")

model_spec_rf <- rand_forest() %>%
  set_engine("ranger")

workflow_fit_lm <- workflow() %>%
  add_model(model_spec_lm) %>%
  add_recipe(recipe_spec_full) %>%
  fit(training(splits))

workflow_fit_rf <- workflow() %>%
  add_model(model_spec_rf) %>%
  add_recipe(recipe_spec_full) %>%
  fit(training(splits))

calibration_tbl <- modeltime_table(
  workflow_fit_lm,
  workflow_fit_rf
) %>%
  modeltime_calibrate(new_data = testing(splits), quiet = FALSE)

calibration_tbl %>%
  modeltime_forecast(new_data = testing(splits), actual_data = subscribers_daily_tbl) %>%
  plot_modeltime_forecast()

calibration_tbl %>% modeltime_accuracy()



# Features Engineering + Recipes Workflow -----------------------------------

# * Pre-processing Data ---------------------------------------------------

subscribers_daily_tbl %>% glimpse()
events_daily_tbl %>% glimpse()

# pre-processing target variable
subscribers_prep_tbl <- subscribers_daily_tbl %>%
  mutate(optins_trans = log_interval_vec(optins, limit_lower = 0, offset = 1)) %>%
  mutate(optins_trans = standardize_vec(optins_trans)) %>%
  filter_by_time(.start_date = "2018-07-03") %>%
  mutate(optins_trans_cleaned = ts_clean_vec(optins_trans, period = 7)) %>%
  mutate(optins_trans = ifelse(
    optin_time %>% between_time("2018-11-18", "2018-11-20"),
    optins_trans_cleaned,
    optins_trans)
  ) %>%
  select(-optins, -optins_trans_cleaned)

# save key parameters
limit_lower <- 0
limit_upper <- 3650.8
offset <- 1
std_mean <- -5.25529020756467
std_sd <- 1.1109817111334


# * Creating Features -----------------------------------------------------

# - Extend to Future Window
# - Add any lags to full dataset
# - Add any external regressors to full dataset

horizon <- 7 * 8
lag_period <- 7 * 8
rolling_periods <- c(30, 60, 90)

data_prep_full_tbl <- subscribers_prep_tbl %>%
  # Add future window
  bind_rows(future_frame(.data = ., optin_time, .length_out = horizon)) %>%
  # Add Autocorrelated Lags
  tk_augment_lags(optins_trans, .lags = lag_period) %>%
  # Add rolling features
  tk_augment_slidify(
    optins_trans_lag56, mean, .period = rolling_periods, .align = "center", .partial = TRUE
  ) %>%
  # Add Events
  left_join(events_daily_tbl, by = c("optin_time" = "event_date")) %>%
  mutate(event = ifelse(is.na(event), 0, event)) %>%
  # Reformat Columns
  rename_with(.cols = contains("lag"), .fn = ~ str_c("lag_", .))

data_prep_full_tbl %>%
  pivot_longer(-optin_time) %>%
  plot_time_series(optin_time, value, name, .smooth = FALSE)
data_prep_full_tbl %>% tail(7 * 8 + 1)


# * Separate into Modelling & Forecast Data -------------------------------

data_prep_full_tbl %>% tail(horizon + 1)

data_prep_tbl <- data_prep_full_tbl %>%
  slice_head(n = nrow(.) - horizon)

forecast_tbl <- data_prep_full_tbl %>%
  slice_tail(n = horizon)


# * Train / Test Sets -----------------------------------------------------

splits <- time_series_split(data_prep_tbl, assess = horizon, cumulative = TRUE)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(optin_time, optins_trans)


# * Recipes ---------------------------------------------------------------

# Baseline Recipe
# - Time Series Signature - Adds bulk time-based features
# - Interaction: wday.lbl:week2
# - Fourier Features
rcp_spec <- recipe(optins_trans ~ ., data = training(splits)) %>%
  # Time Series Signature
  step_timeseries_signature(optin_time) %>%
  step_rm(matches("(iso)|(xts)|(hour)|(minute)|(second)|(am.pm)")) %>%
  step_normalize(matches("(index.num)|(year)|(yday)")) %>%
  step_dummy(all_nominal(), one_hot = TRUE) %>%
  # Interaction
  step_interact(~ matches("week2") * matches("wday.lbl")) %>%
  # Fourier
  step_fourier(optin_time, period = c(7, 14, 30, 90, 365), K = 2)
rcp_spec %>% prep() %>% juice() %>% glimpse()

# Spline Recipe
# - natural spline series on index.num
rcp_spec_spline <- rcp_spec %>%
  step_ns(ends_with("index.num"), deg_free = 2) %>%
  step_rm(optin_time) %>%
  step_rm(starts_with("lag_"))
rcp_spec_spline %>% prep() %>% juice() %>% glimpse()

# Lag Recipe
# - lags of optins_trans and rolls
rcp_spec_lag <- rcp_spec %>%
  step_naomit(starts_with("lag_")) %>%
  step_rm(optin_time)
rcp_spec_lag %>% prep() %>% juice() %>% glimpse()


# * Model Engine Specification --------------------------------------------

model_spec_lm <- linear_reg() %>%
  set_engine("lm")


# * Model Workflows -------------------------------------------------------

# LM Spline Workflow
wrkfl_fit_lm_1_spline <- workflow() %>%
  add_model(model_spec_lm) %>%
  add_recipe(rcp_spec_spline) %>%
  fit(training(splits))
wrkfl_fit_lm_1_spline
wrkfl_fit_lm_1_spline %>%
  extract_fit_parsnip() %>%
  pluck("fit") %>%
  summary()

# LM Lag Workflow
wrkfl_fit_lm_2_lag <- workflow() %>%
  add_model(model_spec_lm) %>%
  add_recipe(rcp_spec_lag) %>%
  fit(training(splits))
wrkfl_fit_lm_2_lag
wrkfl_fit_lm_2_lag %>%
  extract_fit_parsnip() %>%
  pluck("fit") %>%
  summary()


# * Modeltime -------------------------------------------------------------

# Calibration
calibration_tbl <- modeltime_table(
  wrkfl_fit_lm_1_spline,
  wrkfl_fit_lm_2_lag
) %>%
  update_model_description(1, "LM - Spline Recipe") %>%
  update_model_description(2, "LM - Lag Recipe") %>%
  modeltime_calibrate(new_data = testing(splits))
calibration_tbl

# Forecasting
calibration_tbl %>%
  modeltime_forecast(new_data = testing(splits), actual_data = data_prep_tbl) %>%
  plot_modeltime_forecast()

# Accuracy
calibration_tbl %>% modeltime_accuracy()

# Refitting
refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = data_prep_tbl)

refit_tbl %>%
  modeltime_forecast(new_data = forecast_tbl, actual_data = data_prep_tbl) %>%
  mutate(
    across(
      .value:.conf_hi,
      .fns = ~ standardize_inv_vec(x = ., mean = std_mean, sd = std_sd)
    )
  ) %>%
  mutate(
    across(
      .value:.conf_hi,
      .fns = ~ log_interval_inv_vec(
        x = ., limit_lower = limit_lower, limit_upper = limit_upper, offset = offset
      )
    )
  ) %>%
  plot_modeltime_forecast()


# * Save Artifacts --------------------------------------------------------

feature_engineering_artifacts_list <- list(
  # Data
  data = list(
    "data_prep_tbl" = data_prep_tbl,
    "forecast_tbl" = forecast_tbl
  ),
  # Recipes
  recipes = list(
    "rcp_spec" = rcp_spec,
    "rcp_spec_spline" = rcp_spec_spline,
    "rcp_spec_lag" = rcp_spec_lag
  ),
  # Models / Workflows
  models = list(
    "wrkfl_fit_lm_1_spline" = wrkfl_fit_lm_1_spline,
    "wrkfl_fit_lm_2_lag" = wrkfl_fit_lm_2_lag
  ),
  # Inversion Parameters
  standardize = list(
    std_mean = std_mean,
    std_sd   = std_sd
  ),
  log_interval = list(
    limit_lower = limit_lower,
    limit_upper = limit_upper,
    offset      = offset
  )
)

feature_engineering_artifacts_list %>%
  write_rds("artifacts/feature_engineering_artifacts_list.rds")

