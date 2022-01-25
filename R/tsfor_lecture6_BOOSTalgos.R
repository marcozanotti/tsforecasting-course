# Time Series Forecasting: Machine Learning and Deep Learning with R & Python ----

# Lecture 6: Boosted Time Series Algorithms -------------------------------
# 2021/2022
# Marco Zanotti

# Goals:
# - Understand Boosting Errors
# - ARIMA Boost
# - PROPHET Boost

# https://github.com/AlbertoAlmuinha/boostime



# Packages ----------------------------------------------------------------

source("R/utils.R")
source("R/packages.R")

devtools::install_github("AlbertoAlmuinha/boostime")
library(boostime)



# Data & Artifacts --------------------------------------------------------

artifacts_list <- read_rds("artifacts/feature_engineering_artifacts_list.rds")
data_prep_tbl <- artifacts_list$data$data_prep_tbl
forecast_tbl <- artifacts_list$data$forecast_tbl


# * Train / Test Sets -----------------------------------------------------

splits <- time_series_split(data_prep_tbl, assess = "8 weeks", cumulative = TRUE)
splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(optin_time, optins_trans)


# * Recipes ---------------------------------------------------------------

rcp_spec_fourier <- recipe(
  optins_trans ~ optin_time + .,
  data = training(splits)
) %>%
  step_fourier(optin_time, period = c(7, 14, 30, 90), K = 1)
# recipe for the ARIMA model

rcp_spec <- artifacts_list$recipes$rcp_spec %>%
  step_rm(starts_with("lag"))



# ARIMA XGBOOST -----------------------------------------------------------

?arima_boost()


# * Engines ---------------------------------------------------------------

# SARIMA
model_fit_arima <- arima_reg(
  non_seasonal_ar = 1,
  non_seasonal_differences = 1,
  non_seasonal_ma = 1,
  seasonal_period = 7,
  seasonal_ar = 1,
  seasonal_differences = 1,
  seasonal_ma = 1
) %>%
  set_engine("arima") %>%
  fit(optins_trans ~ optin_time, training(splits))

# Auto-SARIMA with XREG
model_spec_auto_sarima_xregs <- arima_reg() %>%
  set_engine("auto_arima")

# ARIMA with XGBoost
model_spec_arima_xgb <- arima_boost(
  # ARIMA params
  non_seasonal_ar = 1,
  non_seasonal_differences = 1,
  non_seasonal_ma = 1,
  # XGBOOST params
  mtry = 0.75,
  min_n = 20,
  tree_depth = 3,
  learn_rate = 0.25,
  loss_reduction = 0.15,
  trees = 300
) %>%
  set_engine(
    "arima_xgboost",
    counts = FALSE
  )

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

# https://parsnip.tidymodels.org/news/
# Parsnip 0.1.7 / 0.1.6


# * Workflows -------------------------------------------------------------

# Auto-SARIMA with XREG
set.seed(123)
wrkfl_fit_auto_sarima_xregs <- workflow() %>%
  add_model(model_spec_auto_sarima_xregs) %>%
  add_recipe(rcp_spec_fourier) %>%
  fit(training(splits))

# ARIMA with XGBoost + base recipe
set.seed(123)
wrkfl_fit_arima_xgb <- workflow() %>%
  add_model(model_spec_arima_xgb) %>%
  add_recipe(rcp_spec) %>%
  fit(training(splits))

# Auto-ARIMA with XGBoost + base recipe
set.seed(123)
wrkfl_fit_auto_arima_xgb <- workflow() %>%
  add_model(model_spec_auto_arima_xgb) %>%
  add_recipe(rcp_spec) %>%
  fit(training(splits))


# * Calibration, Evaluation & Plotting ------------------------------------

calibrate_evaluate_plot(
  model_fit_arima,
  wrkfl_fit_auto_sarima_xregs,
  wrkfl_fit_arima_xgb,
  wrkfl_fit_auto_arima_xgb
)



# ARIMA LIGHT GBM ---------------------------------------------------------

?boost_arima()
# does not work now, there is a bug


# * Engines ---------------------------------------------------------------

# ARIMA with Light GBM
model_spec_arima_lgbm <- boost_arima(
  # ARIMA params
  non_seasonal_ar = 1,
  non_seasonal_differences = 1,
  non_seasonal_ma = 1,
  # Light GBM params
  mtry = 25,
  min_n = 20,
  tree_depth = 3,
  learn_rate = 0.25,
  loss_reduction = 0.15,
  trees = 300
) %>%
  set_engine("arima_lightgbm")

# Auto-ARIMA with Light GBM
model_spec_auto_arima_lgbm <- boost_arima(
  # Auto-ARIMA params
  seasonal_period = "auto",
  # Light GBM params
  mtry = 25,
  min_n = 20,
  tree_depth = 3,
  learn_rate = 0.25,
  loss_reduction = 0.15,
  trees = 300
) %>%
  set_engine("auto_arima_lightgbm")


# * Workflows -------------------------------------------------------------

# ARIMA with Light GBM + base recipe
# set.seed(123)
# wrkfl_fit_arima_lgbm <- workflow() %>%
#   add_model(model_spec_arima_lgbm) %>%
#   add_recipe(rcp_spec) %>%
#   fit(training(splits))

# Auto-ARIMA with Light GBM + base recipe
# set.seed(123)
# wrkfl_fit_auto_arima_lgbm <- workflow() %>%
#   add_model(model_spec_auto_arima_lgbm) %>%
#   add_recipe(rcp_spec) %>%
#   fit(training(splits))


# * Calibration, Evaluation & Plotting ------------------------------------

# calibrate_evaluate_plot(
#   wrkfl_fit_arima_lgbm,
#   wrkfl_fit_auto_arima_lgbm
# )



# ARIMA CAT BOOST ---------------------------------------------------------

?boost_arima()


# * Engines ---------------------------------------------------------------

# ARIMA with CAT Boost
model_spec_arima_catboost <- boost_arima(
  # ARIMA params
  non_seasonal_ar = 1,
  non_seasonal_differences = 1,
  non_seasonal_ma = 1,
  # CAT Boost params
  mtry = 0.75,
  min_n = 20,
  tree_depth = 3,
  learn_rate = 0.25,
  loss_reduction = 0.15,
  trees = 300
) %>%
  set_engine("arima_catboost")

# Auto-ARIMA with CAT Boost
model_spec_auto_arima_catboost <- boost_arima(
  # Auto-ARIMA params
  seasonal_period = "auto",
  # CAT Boost params
  mtry = 0.75,
  min_n = 20,
  tree_depth = 3,
  learn_rate = 0.25,
  loss_reduction = 0.15,
  trees = 300
) %>%
  set_engine("auto_arima_catboost")


# * Workflows -------------------------------------------------------------

# ARIMA with CAT Boost + base recipe
set.seed(123)
wrkfl_fit_arima_catboost <- workflow() %>%
  add_model(model_spec_arima_catboost) %>%
  add_recipe(rcp_spec) %>%
  fit(training(splits))

# Auto-ARIMA with CAT Boost + base recipe
set.seed(123)
wrkfl_fit_auto_arima_catboost <- workflow() %>%
  add_model(model_spec_auto_arima_catboost) %>%
  add_recipe(rcp_spec) %>%
  fit(training(splits))


# * Calibration, Evaluation & Plotting ------------------------------------

calibrate_evaluate_plot(
  wrkfl_fit_arima_catboost,
  wrkfl_fit_auto_arima_catboost
)



# PROPHET XGBOOST ---------------------------------------------------------

?prophet_boost()


# * Engines ---------------------------------------------------------------

# PROPHET with XREGs
model_fit_prophet_xregs <- prophet_reg(
  seasonality_weekly = TRUE,
  seasonality_yearly = TRUE
) %>%
  set_engine("prophet") %>%
  fit(optins_trans ~ optin_time + event, data = training(splits))

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


# * Workflows -------------------------------------------------------------

# PROPHET with boosting + base recipe
set.seed(123)
wrkfl_fit_prophet_xgb <- workflow() %>%
  add_model(model_spec_prophet_xgb) %>%
  add_recipe(rcp_spec) %>%
  fit(training(splits))


# * Calibration, Evaluation & Plotting ------------------------------------

calibrate_evaluate_plot(
  model_fit_prophet_xregs,
  wrkfl_fit_prophet_xgb
)



# PROPHETLIGHT GBM ---------------------------------------------------------

?boost_arima()
# does not work, there is a bug


# * Engines ---------------------------------------------------------------

# PROPHETwith Light GBM
model_spec_prophet_lgbm <- boost_prophet(
  # PROPHET params
  changepoint_num = 25,
  changepoint_range = 0.8,
  seasonality_daily = FALSE,
  seasonality_weekly = FALSE,
  seasonality_yearly = FALSE,
  # Light GBM params
  mtry = 25,
  min_n = 20,
  tree_depth = 3,
  learn_rate = 0.25,
  loss_reduction = 0.15,
  trees = 300
) %>%
  set_engine("prophet_lightgbm")


# * Workflows -------------------------------------------------------------

# PROPHET with Light GBM + base recipe
# set.seed(123)
# wrkfl_fit_prophet_lgbm <- workflow() %>%
#   add_model(model_spec_prophet_lgbm) %>%
#   add_recipe(rcp_spec) %>%
#   fit(training(splits))


# * Calibration, Evaluation & Plotting ------------------------------------

# calibrate_evaluate_plot(wrkfl_fit_prophet_lgbm)



# PROPHETCAT BOOST ---------------------------------------------------------

?boost_arima()


# * Engines ---------------------------------------------------------------

# PROPHET with CAT Boost
model_spec_prophet_catboost <- boost_prophet(
  # PROPHET params
  changepoint_num = 25,
  changepoint_range = 0.8,
  seasonality_daily = FALSE,
  seasonality_weekly = FALSE,
  seasonality_yearly = FALSE,
  # CAT Boost params
  mtry = 0.75,
  min_n = 20,
  tree_depth = 3,
  learn_rate = 0.25,
  loss_reduction = 0.15,
  trees = 300
) %>%
  set_engine("prophet_catboost")


# * Workflows -------------------------------------------------------------

# PROPHET with CAT Boost + base recipe
set.seed(123)
wrkfl_fit_prophet_catboost <- workflow() %>%
  add_model(model_spec_prophet_catboost) %>%
  add_recipe(rcp_spec) %>%
  fit(training(splits))


# * Calibration, Evaluation & Plotting ------------------------------------

calibrate_evaluate_plot(wrkfl_fit_prophet_catboost)



# BOOST Models' Performance -----------------------------------------------

# * Comparison ------------------------------------------------------------

# * Calibration (full)
calibration_tbl <- modeltime_table(
  model_fit_arima,
  wrkfl_fit_auto_sarima_xregs,
  wrkfl_fit_arima_xgb,
  wrkfl_fit_auto_arima_xgb,
  # wrkfl_fit_arima_lgbm,
  # wrkfl_fit_auto_arima_lgbm,
  wrkfl_fit_arima_catboost,
  wrkfl_fit_auto_arima_catboost,
  model_fit_prophet_xregs,
  wrkfl_fit_prophet_xgb,
  # wrkfl_fit_prophet_lgbm,
  wrkfl_fit_prophet_catboost
  ) %>%
  modeltime_calibrate(testing(splits))

# * Evaluation
calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = TRUE, bordered = TRUE, resizable = TRUE)

calibration_tbl %>%
  modeltime_forecast(new_data = testing(splits), actual_data = data_prep_tbl) %>%
  plot_modeltime_forecast()

# * Refitting & Forecasting

# Best by RMSE
model_boost_best <- calibration_tbl %>%
  select_best_id(n = 3)

refit_tbl <- calibration_tbl %>%
  filter(.model_id %in% model_boost_best) %>%
  modeltime_refit(data = data_prep_tbl)

refit_tbl %>%
  modeltime_forecast(new_data = forecast_tbl, actual_data = data_prep_tbl) %>%
  plot_modeltime_forecast(.conf_interval_fill = "lightblue")


# * Save Artifacts --------------------------------------------------------

calibration_tbl %>%
  write_rds("artifacts/calibration_boost.rds")

