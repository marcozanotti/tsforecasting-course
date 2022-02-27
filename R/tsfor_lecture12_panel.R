# Time Series Forecasting: Machine Learning and Deep Learning with R & Python ----

# Lecture 12: Panel Time Series Forecasting -------------------------------
# 2021/2022
# Marco Zanotti

# Goals:
# - Nested Forecasting
# - Nested Forecasting with many models
# - Global Modelling
# - Global Modelling with many models



# Packages ----------------------------------------------------------------

source("R/utils.R")
source("R/packages.R")


# Data & Artifacts --------------------------------------------------------

subscribers_tbl <- read_rds("data/subscribers.rds")
events_tbl <- read_rds("data/events.rds")


# Data Preparation --------------------------------------------------------

subscribers_tbl %>% count(member_rating)

# subscribers data by group
subscribers_daily_tbl <- subscribers_tbl %>%
  rename(id = member_rating) %>%
  mutate(id = ifelse(id == 2, id, 1) %>% as.factor()) %>%
  group_by(id) %>%
  summarise_by_time(optin_time, .by = "day", optins = n()) %>%
  pad_by_time(.pad_value = 0)

subscribers_daily_tbl %>%
  plot_time_series(optin_time, log1p(optins), .smooth = FALSE)

subscribers_prep_tbl <- subscribers_daily_tbl %>%
  mutate(optins_trans = log_interval_vec(optins, limit_lower = 0, offset = 1)) %>%
  mutate(optins_trans = standardize_vec(optins_trans)) %>%
  filter_by_time(.start_date = "2018-07-05", .end_date = "2020-02-29") %>%
  mutate(optins_trans_cleaned = ts_clean_vec(optins_trans, period = 7)) %>%
  mutate(
    optins_trans = ifelse(
      optin_time %>% between_time("2018-11-18", "2018-11-20"),
      optins_trans_cleaned,
      optins_trans
    )
  ) %>%
  select(-optins, -optins_trans_cleaned)

subscribers_prep_tbl %>%
  plot_time_series(optin_time, optins_trans, .smooth = FALSE)

# events data
events_daily_tbl <- events_tbl %>%
  mutate(event_date = ymd_hms(event_date)) %>%
  summarise_by_time(event_date, .by = "day", event = n())



# NESTED FORECASTING ------------------------------------------------------
# Modeltime Nested Data Workflow ------------------------------------------

# * Feature Engineering ---------------------------------------------------

?extend_timeseries

horizon <- 7 * 8
lag_period <- 7 * 8
rolling_periods <- c(30, 60, 90)

data_prep_full_tbl <- subscribers_prep_tbl %>%
  # Extend each time series
  # future_frame(.data = ., optin_time, .length_out = horizon, .bind_data = TRUE) %>%
  extend_timeseries(
    .id_var = id,
    .date_var = optin_time,
    .length_future = horizon
  ) %>%
  group_by(id) %>%
  # Add lags
  tk_augment_lags(optins_trans, .lags = lag_period) %>%
  # Add rolling features
  tk_augment_slidify(
    optins_trans_lag56,
    mean,
    .period = rolling_periods,
    .align = "center",
    .partial = TRUE
  ) %>%
  # Add Events
  left_join(events_daily_tbl, by = c("optin_time" = "event_date")) %>%
  mutate(event = ifelse(is.na(event), 0, event)) %>%
  # Reformat Columns
  rename_with(.cols = contains("lag"), .fn = ~ str_c("lag_", .)) %>%
  ungroup()

data_prep_full_tbl %>%
  group_by(id) %>%
  pivot_longer(cols = -c(id, optin_time)) %>%
  plot_time_series(optin_time, value, name, .smooth = FALSE)
data_prep_full_tbl %>% group_by(id) %>% slice_tail(n = horizon + 1)


# * Modelling & Forecast Data ---------------------------------------------

?nest_timeseries

nested_data_tbl <- data_prep_full_tbl %>%
  # Split into actual data & forecast data
  nest_timeseries(.id_var = id, .length_future = horizon)

# from now on, we will work with list columns (that is nested data)


# * Train / Test Sets -----------------------------------------------------

?split_nested_timeseries
?extract_nested_train_split
?extract_nested_test_split

# splits <- time_series_split(data_prep_tbl, assess = horizon, cumulative = TRUE)
nested_data_tbl <- nested_data_tbl %>%
  split_nested_timeseries(.length_test = horizon)

extract_nested_train_split(nested_data_tbl, .row_id = 1)
extract_nested_test_split(nested_data_tbl, .row_id = 1)


# * Recipes ---------------------------------------------------------------

# Baseline Recipe
# - Time Series Signature - Adds bulk time-based features
# - Interaction: wday.lbl:week2
# - Fourier Features
rcp_spec <-
  # recipe(optins_trans ~ ., data = training(splits)) %>%
  recipe(optins_trans ~ ., data = extract_nested_train_split(nested_data_tbl)) %>%
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



# Modeltime Nested Modelling Workflow -------------------------------------

?linear_reg()
# - Baseline model for ML


# * Engines ---------------------------------------------------------------

model_spec_lm <- linear_reg() %>%
  set_engine("lm")


# * Workflows -------------------------------------------------------------

# LM + Splines
wrkfl_fit_lm_spline <- workflow() %>%
  add_model(model_spec_lm) %>%
  add_recipe(rcp_spec_spline)

# LM + Lags
wrkfl_fit_lm_lag <- workflow() %>%
  add_model(model_spec_lm) %>%
  add_recipe(rcp_spec_lag)


# * Calibration -----------------------------------------------------------

?modeltime_nested_fit
?control_nested_fit

nested_modeltime_tbl <- nested_data_tbl %>%
  modeltime_nested_fit(
    model_list = list(
      wrkfl_fit_lm_spline,
      wrkfl_fit_lm_lag
    ),
    control = control_nested_fit(
      verbose = TRUE,
      allow_par = FALSE
    )
  )

nested_modeltime_tbl
# nested modeltime tables


# * Evaluation ------------------------------------------------------------

?extract_nested_test_accuracy
?extract_nested_test_forecast
?extract_nested_error_report

# Accuracy
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  table_modeltime_accuracy()

# Plotting
nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  group_by(id) %>%
  plot_modeltime_forecast()

# Error reporting
nested_modeltime_tbl %>%
  extract_nested_error_report()


# Helper function to quickly calibrate, evaluate and plot
# (to use only with few time series)
nested_calibrate_evaluate_plot(
  nested_data_tbl,
  workflows = list(wrkfl_fit_lm_spline, wrkfl_fit_lm_lag),
  id_var = "id",
  parallel = FALSE
)


# * Refitting -------------------------------------------------------------

?modeltime_nested_select_best
?modeltime_nested_refit

nested_modeltime_best_tbl <- nested_modeltime_tbl %>%
  modeltime_nested_select_best(metric = "rmse")

# refit_tbl <- calibration_tbl %>%
#   modeltime_refit(data = data_prep_tbl)
nested_best_refit_tbl <- nested_modeltime_best_tbl %>%
  modeltime_nested_refit(
    control = control_refit(
      verbose = TRUE,
      allow_par = FALSE
    )
  )

# Error reporting
nested_best_refit_tbl %>% extract_nested_error_report()


# * Forecasting -----------------------------------------------------------

?modeltime_nested_forecast
?extract_nested_future_forecast

# refit_tbl %>%
#   modeltime_forecast(
#     # h = "16 weeks",
#     new_data = forecast_tbl,
#     actual_data = data_prep_tbl,
#     conf_interval = .8
#   ) %>%
#   plot_modeltime_forecast(.conf_interval_fill = "lightblue"  )

nested_best_refit_tbl %>%
  extract_nested_future_forecast()

nested_forecast_tbl <- nested_best_refit_tbl %>%
  modeltime_nested_forecast(
    control = control_nested_forecast(
      verbose   = TRUE,
      allow_par = FALSE
    )
  )

nested_forecast_tbl %>%
  group_by(id) %>%
  plot_modeltime_forecast()



# Nested Multiple Models Workflow -----------------------------------------

# Engines & Workflows -----------------------------------------------------
# * ARIMA XGBOOST ---------------------------------------------------------

# Auto-ARIMA with XGBoost
model_spec_auto_arima_xgb <- arima_boost(
  # Auto-ARIMA params
  seasonal_period = "auto",
  # XGBOOST params
  mtry = 0.75,
  min_n = 20,
  tree_depth = 3,
  learn_rate = 0.25,
  loss_reduction = 0.15,
  trees = 300
) %>%
  set_engine(
    "auto_arima_xgboost",
    counts = FALSE # painful to discover, explain why! (mtry as counts)
  )

set.seed(123)
wrkfl_fit_auto_arima_xgb <- workflow() %>%
  add_model(model_spec_auto_arima_xgb) %>%
  add_recipe(rcp_spec %>% step_rm(starts_with("lag")))


# * PROPHET XGBOOST -------------------------------------------------------

# PROPHET with boosting
model_spec_prophet_xgb <- prophet_boost(
  # PROPHET params
  changepoint_num = 25,
  changepoint_range = 0.8,
  seasonality_daily = FALSE,
  seasonality_weekly = FALSE,
  seasonality_yearly = FALSE,
  # XGBOOST params
  mtry = 0.75,
  min_n = 20,
  tree_depth = 3,
  learn_rate = 0.2,
  loss_reduction = 0.15,
  trees = 300
) %>%
  set_engine(
    "prophet_xgboost",
    counts = FALSE
  )

set.seed(123)
wrkfl_fit_prophet_xgb <- workflow() %>%
  add_model(model_spec_prophet_xgb) %>%
  add_recipe(rcp_spec %>% step_rm(starts_with("lag")))


# * RANDOM FOREST ---------------------------------------------------------

# RF
model_spec_rf <- rand_forest(
  mode = "regression",
  mtry = 25,
  trees = 1000,
  min_n = 25
) %>%
  set_engine("ranger")

set.seed(123) # RF + Splines
wrkfl_fit_rf_spline <- workflow() %>%
  add_model(model_spec_rf) %>%
  add_recipe(rcp_spec_spline)

set.seed(123) # RF + Lags
wrkfl_fit_rf_lag <- workflow() %>%
  add_model(model_spec_rf) %>%
  add_recipe(rcp_spec_lag)


# * NEURAL NETWORK --------------------------------------------------------

# NNETAR
model_spec_nnetar <- nnetar_reg(
  non_seasonal_ar = 2,
  seasonal_ar = 1,
  hidden_units = 10,
  penalty = 10,
  num_networks = 10,
  epochs = 50
) %>%
  set_engine("nnetar")

set.seed(123)
wrkfl_fit_nnetar <- workflow() %>%
  add_model(model_spec_nnetar) %>%
  add_recipe(rcp_spec)


# Calibration, Evaluation & Plotting --------------------------------------

n_cores <- parallel::detectCores()
parallel_start(n_cores - 1)

set.seed(123)
nested_modeltime_tbl <- nested_data_tbl %>%
  modeltime_nested_fit(
    model_list = list(
      wrkfl_fit_auto_arima_xgb,
      wrkfl_fit_prophet_xgb,
      wrkfl_fit_rf_spline,
      wrkfl_fit_rf_lag,
      wrkfl_fit_nnetar
    ),
    control = control_nested_fit(
      verbose = TRUE,
      allow_par = TRUE
    )
  )

parallel_stop()

# Accuracy
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  table_modeltime_accuracy()

# Plotting
nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  group_by(id) %>%
  plot_modeltime_forecast()

# Error reporting
nested_modeltime_tbl %>%
  extract_nested_error_report()


# Refitting & Forecasting -------------------------------------------------

# Select best model for each time series
nested_modeltime_best_tbl <- nested_modeltime_tbl %>%
  modeltime_nested_select_best(metric = "rmse")

parallel_start(n_cores - 1)

# Refitting
nested_best_refit_tbl <- nested_modeltime_best_tbl %>%
  modeltime_nested_refit(
    control = control_refit(
      verbose = TRUE,
      allow_par = TRUE
    )
  )

parallel_stop()

# Error reporting
nested_best_refit_tbl %>% extract_nested_error_report()

# Forecasting
nested_forecast_tbl <- nested_best_refit_tbl %>%
  modeltime_nested_forecast(
    control = control_nested_forecast(
      verbose   = TRUE,
      allow_par = FALSE
    )
  )

nested_forecast_tbl %>%
  group_by(id) %>%
  plot_modeltime_forecast()



# GLOBAL MODELLING --------------------------------------------------------
# Modeltime Global Data Workflow ------------------------------------------

# * Feature Engineering ---------------------------------------------------

subscribers_prep_tbl %>%
  plot_time_series(optin_time, optins_trans, .smooth = FALSE)

horizon <- 7 * 8

data_prep_full_tbl <- subscribers_prep_tbl %>%
  group_by(id) %>%
  future_frame(.data = ., optin_time, .length_out = horizon, .bind_data = TRUE) %>%
  # Add Events
  left_join(events_daily_tbl, by = c("optin_time" = "event_date")) %>%
  mutate(event = ifelse(is.na(event), 0, event)) %>%
  ungroup()

data_prep_full_tbl %>%
  group_by(id) %>%
  pivot_longer(cols = -c(id, optin_time)) %>%
  plot_time_series(optin_time, value, name, .smooth = FALSE)
data_prep_full_tbl %>% group_by(id) %>% slice_tail(n = horizon + 1)


# * Modelling & Forecast Data ---------------------------------------------

data_prep_tbl <- data_prep_full_tbl %>%
  drop_na()
forecast_tbl <- data_prep_full_tbl %>%
  filter(is.na(optins_trans))


# * Train / Test Sets -----------------------------------------------------

splits <- time_series_split(data_prep_tbl, assess = horizon, cumulative = TRUE)
splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(optin_time, optins_trans)


# * Recipes ---------------------------------------------------------------

rcp_spec <- recipe(optins_trans ~ ., data = training(splits)) %>%
  update_role(id, new_role = "id") %>%
  step_mutate_at(id, fn = droplevels) %>%
  step_timeseries_signature(optin_time) %>%
  step_rm(matches("(iso)|(xts)|(hour)|(minute)|(second)|(am.pm)")) %>%
  step_normalize(matches("(index.num)|(year)|(yday)")) %>%
  step_dummy(all_nominal(), one_hot = TRUE) %>%
  step_rm(optin_time)
rcp_spec %>% prep() %>% juice() %>% glimpse()




# Global Modelling Workflow -----------------------------------------------

?linear_reg()
# - Baseline model for ML


# * Engines ---------------------------------------------------------------

model_spec_lm <- linear_reg() %>%
  set_engine("lm")


# * Workflows -------------------------------------------------------------

wrkfl_fit_lm <- workflow() %>%
  add_model(model_spec_lm) %>%
  add_recipe(rcp_spec) %>%
  fit(training(splits))


# * Calibration -----------------------------------------------------------

calibration_tbl <- modeltime_table(wrkfl_fit_lm) %>%
  modeltime_calibrate(testing(splits), id = "id")


# * Evaluation ------------------------------------------------------------

# Global accuracy
calibration_tbl %>%
  modeltime_accuracy()

# Local accuracy
calibration_tbl %>%
  modeltime_accuracy(acc_by_id = TRUE)

calibration_tbl %>%
  modeltime_forecast(new_data = testing(splits), actual_data = data_prep_tbl, conf_by_id = TRUE) %>%
  group_by(id) %>%
  plot_modeltime_forecast(.conf_interval_fill = "lightblue")


# * Refitting -------------------------------------------------------------

refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = data_prep_tbl)


# * Forecasting -----------------------------------------------------------

refit_tbl %>%
  modeltime_forecast(new_data = forecast_tbl, actual_data = data_prep_tbl, conf_by_id  = TRUE) %>%
  group_by(id) %>%
  plot_modeltime_forecast(.conf_interval_fill = "lightblue")



# Global Multiple Models Workflow -----------------------------------------

# Engines & Workflows -----------------------------------------------------
# * ELASTIC NET -----------------------------------------------------------

model_spec_elanet <- linear_reg(
  mode = "regression",
  penalty = 0.01,
  mixture = 0.5
) %>%
  set_engine("glmnet")

wrkfl_fit_elanet <- workflow() %>%
  add_model(model_spec_elanet) %>%
  add_recipe(rcp_spec) %>%
  fit(training(splits))


# * SVM -------------------------------------------------------------------

model_spec_svm_rbf <- svm_rbf(
  mode = "regression",
  cost = 1,
  rbf_sigma = 0.01,
  margin = 0.1
) %>%
  set_engine("kernlab")

set.seed(123)
wrkfl_fit_svm_rbf <- workflow() %>%
  add_model(model_spec_svm_rbf) %>%
  add_recipe(rcp_spec) %>%
  fit(training(splits))


# * BOOSTING --------------------------------------------------------------

# XGBOOST
model_spec_xgb <- boost_tree(
  mode = "regression",
  mtry = 25,
  trees = 1000,
  min_n = 2,
  tree_depth = 12,
  learn_rate = 0.3,
  loss_reduction = 0
) %>%
  set_engine("xgboost")

set.seed(123)
wrkfl_fit_xgb <- workflow() %>%
  add_model(model_spec_xgb) %>%
  add_recipe(rcp_spec) %>%
  fit(training(splits))

# LIGHT GBM
model_spec_lightgbm <- boost_tree(mode = "regression") %>%
  set_engine("lightgbm")

set.seed(123)
wrkfl_fit_lightgbm <- workflow() %>%
  add_model(model_spec_lightgbm) %>%
  add_recipe(rcp_spec) %>%
  fit(training(splits))

# CAT BOOST
model_spec_catboost <- boost_tree(mode = "regression") %>%
  set_engine("catboost")

set.seed(123)
wrkfl_fit_catboost <- workflow() %>%
  add_model(model_spec_catboost) %>%
  add_recipe(rcp_spec) %>%
  fit(training(splits))


# Calibration, Evaluation & Plotting --------------------------------------

calibration_tbl <- modeltime_table(
  wrkfl_fit_lm,
  wrkfl_fit_elanet,
  wrkfl_fit_svm_rbf,
  wrkfl_fit_xgb,
  wrkfl_fit_lightgbm,
  wrkfl_fit_catboost
) %>%
  modeltime_calibrate(testing(splits), id = "id")

# Global accuracy
calibration_tbl %>%
  modeltime_accuracy()

# Local accuracy
calibration_tbl %>%
  modeltime_accuracy(acc_by_id = TRUE) %>%
  arrange(id)

calibration_tbl %>%
  modeltime_forecast(new_data = testing(splits), actual_data = data_prep_tbl, conf_by_id = TRUE) %>%
  group_by(id) %>%
  plot_modeltime_forecast(.conf_interval_show = FALSE)


# Refitting & Forecasting -------------------------------------------------

# Select Best Global Model
global_model_best <- calibration_tbl %>%
  select_best_id(n = 1, metric = "rmse")

refit_global_tbl <- calibration_tbl %>%
  filter(.model_id %in% global_model_best) %>%
  modeltime_refit(data = data_prep_tbl)

refit_global_tbl %>%
  modeltime_forecast(new_data = forecast_tbl, actual_data = data_prep_tbl, conf_by_id  = TRUE) %>%
  group_by(id) %>%
  plot_modeltime_forecast(.conf_interval_fill = "lightblue")


# Select Best Local Models
local_models_best <- calibration_tbl %>%
  select_best_id(n = 1, metric = "rmse", by_id = TRUE, id_var = "id")

refit_local_tbl <- calibration_tbl %>%
  filter(.model_id %in% local_models_best) %>%
  modeltime_refit(data = data_prep_tbl)

forecast_local_tbl <- vector("list", length(local_models_best))
for (ts_id in as.numeric(unique(data_prep_tbl$id))) {
  forecast_local_tbl[[ts_id]] <- refit_local_tbl %>%
    filter(.model_id == local_models_best[ts_id]) %>%
    modeltime_forecast(
      new_data = forecast_tbl %>% filter(id == ts_id),
      actual_data = data_prep_tbl %>% filter(id == ts_id),
      conf_by_id  = TRUE
    )
}
forecast_local_tbl <- bind_rows(forecast_local_tbl)

forecast_local_tbl %>%
  group_by(id) %>%
  plot_modeltime_forecast(.conf_interval_fill = "lightblue")

