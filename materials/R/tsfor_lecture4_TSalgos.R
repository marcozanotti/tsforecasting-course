# Time Series Forecasting: Machine Learning and Deep Learning with R & Python ----

# Lecture 4: Time Series Algorithms ---------------------------------------
# 2021/2022
# Marco Zanotti

# Goals:
# - Global Baseline Models
# - SARIMAX
# - ETS
# - TBATS
# - STLM
# - PROPHET

setwd("materials")



# Packages ----------------------------------------------------------------

source("R/utils.R")
source("R/packages.R")



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

rcp_spec_fourier <- recipe(optins_trans ~ optin_time + event, data = training(splits)) %>%
  step_fourier(optin_time, period = c(7, 14, 30, 90), K = 1)
# Recipe with Fourier Terms and Events



# S-NAIVE & WINDOWS -------------------------------------------------------

# Naive
# Seasonal Naive
# Mean / Median

?naive_reg()
?window_reg()


# * Engines ---------------------------------------------------------------

# NAIVE
model_fit_naive <- naive_reg() %>%
  set_engine("naive") %>%
  fit(optins_trans ~ optin_time, training(splits))

# SNAIVE
model_fit_snaive <- naive_reg() %>%
  set_engine("snaive") %>%
  fit(optins_trans ~ optin_time, training(splits))

# WINDOW - MEAN
model_fit_mean <- window_reg(
  window_size = 7
) %>%
  set_engine(
    "window_function",
    window_function = mean,
    na.rm = TRUE
  ) %>%
  fit(optins_trans ~ optin_time, training(splits))

# WINDOW - WEIGHTED MEAN
model_fit_wmean <- window_reg(
  window_size = 7
) %>%
  set_engine(
    "window_function",
    window_function = ~ sum(tail(.x, 3) * c(0.1, 0.3, 0.6))
  ) %>%
  fit(optins_trans ~ optin_time, training(splits))

# WINDOW - MEDIAN
model_fit_median <- window_reg(
  window_size = 7
) %>%
  set_engine(
    "window_function",
    window_function = median,
    na.rm = TRUE
  ) %>%
  fit(optins_trans ~ optin_time, training(splits))


# * Calibration -----------------------------------------------------------

calibration_tbl <- modeltime_table(
  model_fit_naive,
  model_fit_snaive,
  model_fit_mean,
  model_fit_wmean,
  model_fit_median
) %>%
  update_modeltime_description(.model_id = 3, .new_model_desc = "MEAN [7]") %>%
  update_modeltime_description(.model_id = 4, .new_model_desc = "WMEAN [7]") %>%
  update_modeltime_description(.model_id = 5, .new_model_desc = "MEDIAN [7]") %>%
  modeltime_calibrate(testing(splits))


# * Evaluation ------------------------------------------------------------

calibration_tbl %>%
  modeltime_accuracy()

calibration_tbl %>%
  modeltime_forecast(new_data = testing(splits), actual_data = data_prep_tbl) %>%
  plot_modeltime_forecast()


# * Refitting & Forecasting -----------------------------------------------

refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = data_prep_tbl)

refit_tbl %>%
  modeltime_forecast(new_data = forecast_tbl, actual_data = data_prep_tbl) %>%
  plot_modeltime_forecast(.conf_interval_fill = "lightblue")



# S-ARIMA-X ---------------------------------------------------------------

# Seasonal Regression with ARIMA Errors and External Regressors
# yt = alpha * L(yt)^k +  beta L(yt)^s + et + gamma * L(et)^k + delta * L(xt)^k

?arima_reg()

# ARIMA is a simple algorithm that relies on Linear Regression
# Strengths:
# - Automated Differencing
# - Automated Parameter Search (auto_arima)
# - Single seasonality modeling included
# - Recursive Lag Forecasting
# Weaknesses:
# - Only single seasonality by default (XREGs can help go beyond single seasonality)
# - Becomes erratic with too many lags
# - Requires Expensive Parameter Search


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

# Auto-SARIMA
model_fit_auto_sarima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(optins_trans ~ optin_time, training(splits))

# Auto-SARIMA with XREG
model_spec_auto_sarima_xregs <- arima_reg() %>%
  set_engine("auto_arima")


# * Workflows -------------------------------------------------------------

# Auto-SARIMA with XREG
wrkfl_fit_auto_sarima_xregs <- workflow() %>%
  add_recipe(rcp_spec_fourier) %>%
  add_model(model_spec_auto_sarima_xregs) %>%
  fit(training(splits))


# * Calibration, Evaluation & Plotting ------------------------------------

calibrate_evaluate_plot(
  model_fit_arima,
  model_fit_auto_sarima,
  wrkfl_fit_auto_sarima_xregs
)



# EXPONENTIAL SMOOTHING (ETS) ---------------------------------------------

# Error, Trend & Seasonality (Holt-Winters Seasonal)

?exp_smoothing

# - Automatic forecasting method based on Exponential Smoothing
# - Single Seasonality
# - Cannot use XREGs (purely univariate)


# * Engines ---------------------------------------------------------------

# ETS Additive
model_fit_ets <- exp_smoothing(
  error = "additive",
  trend = "additive",
  season = "additive"
) %>%
  set_engine("ets") %>%
  fit(optins_trans ~ optin_time, data = training(splits))

# Auto-ETS
model_fit_auto_ets <- exp_smoothing() %>%
  set_engine("ets") %>%
  fit(optins_trans ~ optin_time, data = training(splits))

# ThetaF
model_fit_theta <- exp_smoothing() %>%
  set_engine("theta") %>%
  fit(optins_trans ~ optin_time, data = training(splits))


# * Calibration, Evaluation & Plotting ------------------------------------

calibrate_evaluate_plot(
  model_fit_ets,
  model_fit_auto_ets,
  model_fit_theta
)



# TBATS -------------------------------------------------------------------

# Exponential Smoothing with Box-Cox transformation, ARMA errors, Trend and Seasonality

?seasonal_reg()
?tbats()

# - Multiple Seasonality Model
# - Extension of ETS for complex seasonality
# - Automatic
# - Does not support XREGS


# * Engines ---------------------------------------------------------------

# TBATS with 3 Seasonalities
model_fit_tbats <- seasonal_reg(
  seasonal_period_1 = 7,
  seasonal_period_2 = 30,
  seasonal_period_3 = 365
) %>%
  set_engine("tbats") %>%
  fit(optins_trans ~ optin_time, training(splits))

# Auto-TBATS
model_fit_auto_tbats <- seasonal_reg() %>%
  set_engine("tbats") %>%
  fit(optins_trans ~ optin_time, training(splits))


# * Calibration, Evaluation & Plotting ------------------------------------

calibrate_evaluate_plot(
  model_fit_tbats,
  model_fit_auto_tbats
)



# STLM --------------------------------------------------------------------

# Seasonal & Trend Decomposition using LOESS Models

?seasonal_reg()
?stlm()

# - Uses seasonal decomposition to model trend & seasonality separately
#   - Trend modeled with ARIMA or ETS
#   - Seasonality modeled with Seasonal Naive (SNAIVE)
# - Can handle multiple seasonality
# - ARIMA version accepts XREGS, ETS does not


# * Engines ---------------------------------------------------------------

# STLM with ETS
model_fit_stlm_ets <- seasonal_reg(
  seasonal_period_1 = 7,
  seasonal_period_2 = 30,
  seasonal_period_3 = 364 / 2
) %>%
  set_engine("stlm_ets") %>%
  fit(optins_trans ~ optin_time, data = training(splits))

# STLM with ARIMA
model_fit_stlm_arima <- seasonal_reg(
  seasonal_period_1 = 7,
  seasonal_period_2 = 30,
  seasonal_period_3 = 364 / 2
) %>%
  set_engine("stlm_arima") %>%
  fit(optins_trans ~ optin_time, data = training(splits))

# STLM with ARIMA + XREGS
model_fit_stlm_arima_xregs <- seasonal_reg(
  seasonal_period_1 = 7,
  seasonal_period_2 = 30,
  seasonal_period_3 = 364 / 2
) %>%
  set_engine("stlm_arima") %>%
  fit(optins_trans ~ optin_time + event, data = training(splits))

# Auto-STLM with ARIMA (simply STL with ARIMA on the ts frequency seasonlity)
model_fit_auto_stlm_arima <- seasonal_reg() %>%
  set_engine("stlm_arima") %>%
  fit(optins_trans ~ optin_time, data = training(splits))

# Auto-STLM with ARIMA + XREGS
model_fit_auto_stlm_arima_xregs <- seasonal_reg() %>%
  set_engine("stlm_arima") %>%
  fit(optins_trans ~ optin_time + event, data = training(splits))


# * Calibration, Evaluation & Plotting ------------------------------------

calibrate_evaluate_plot(
  model_fit_stlm_ets,
  model_fit_stlm_arima,
  model_fit_stlm_arima_xregs,
  model_fit_auto_stlm_arima,
  model_fit_auto_stlm_arima_xregs
)



# PROPHET -----------------------------------------------------------------

# Facebook's Prophet Algorithm

?prophet_reg

# Can handle daily, weekly and yearly seasonality
# Automatic
# Can use smoothing trend
# Accepts external regressors (can be used to include other seasonalities)


# * Engines ---------------------------------------------------------------

# PROPHET
model_fit_prophet <- prophet_reg(
  changepoint_num = 10,
  changepoint_range = 0.9,
  seasonality_weekly = TRUE,
  seasonality_yearly = TRUE
) %>%
  set_engine("prophet") %>%
  fit(optins_trans ~ optin_time, data = training(splits))

prophet_model <- model_fit_prophet$fit$models$model_1
prophet_fcst <- predict(
  prophet_model,
  newdata = training(splits) %>% rename(ds = 1, y = 2) %>% select(ds, y)
)
plot(prophet_model, prophet_fcst) +
  add_changepoints_to_plot(prophet_model)
prophet_plot_components(prophet_model, prophet_fcst)

# Auto-PROPHET
model_fit_auto_prophet <- prophet_reg() %>%
  set_engine("prophet") %>%
  fit(optins_trans ~ optin_time, data = training(splits))

prophet_model <- model_fit_auto_prophet$fit$models$model_1
prophet_fcst <- predict(
  prophet_model,
  newdata = training(splits) %>% rename(ds = 1, y = 2) %>% select(ds, y)
)
plot(prophet_model, prophet_fcst) +
  add_changepoints_to_plot(prophet_model)
prophet_plot_components(prophet_model, prophet_fcst)

# PROPHET with XREGs
model_fit_prophet_xregs <- prophet_reg(
  seasonality_weekly = TRUE,
  seasonality_yearly = TRUE
) %>%
  set_engine("prophet") %>%
  fit(optins_trans ~ optin_time + event, data = training(splits))


# * Calibration, Evaluation & Plotting ------------------------------------

calibrate_evaluate_plot(
    model_fit_prophet,
    model_fit_auto_prophet,
    model_fit_prophet_xregs
  )



# TS Models' Performance --------------------------------------------------

# * Comparison ------------------------------------------------------------

# * Calibration (full)
calibration_tbl <- modeltime_table(
  # BASELINE
  model_fit_naive,
  model_fit_snaive,
  model_fit_mean,
  model_fit_wmean,
  model_fit_median,
  # SARIMAX
  model_fit_arima,
  model_fit_auto_sarima,
  wrkfl_fit_auto_sarima_xregs,
  # ETS
  model_fit_ets,
  model_fit_auto_ets,
  model_fit_theta,
  # TBATS
  model_fit_tbats,
  model_fit_auto_tbats,
  # STLM
  model_fit_stlm_ets,
  model_fit_stlm_arima,
  model_fit_stlm_arima_xregs,
  model_fit_auto_stlm_arima,
  model_fit_auto_stlm_arima_xregs,
  # PROPHET
  model_fit_prophet,
  model_fit_auto_prophet,
  model_fit_prophet_xregs
) %>%
  update_modeltime_description(.model_id = 3, .new_model_desc = "MEAN [7]") %>%
  update_modeltime_description(.model_id = 4, .new_model_desc = "WMEAN [7]") %>%
  update_modeltime_description(.model_id = 5, .new_model_desc = "MEDIAN [7]") %>%
  update_model_description(.model_id = 20, .new_model_desc = "Auto-PROPHET") %>%
  modeltime_calibrate(testing(splits))

# * Calibration (best)
calibration_tbl <- modeltime_table(
  model_fit_mean,
  wrkfl_fit_auto_sarima_xregs,
  model_fit_auto_ets,
  model_fit_auto_tbats,
  model_fit_auto_stlm_arima_xregs,
  model_fit_prophet_xregs
) %>%
  update_modeltime_description(.model_id = 1, .new_model_desc = "Baseline - MEAN [7]") %>%
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
model_ts_best <- calibration_tbl %>%
  select_best_id(n = 3)

refit_tbl <- calibration_tbl %>%
  filter(.model_id %in% model_ts_best) %>%
  modeltime_refit(data = data_prep_tbl)

refit_tbl %>%
  modeltime_forecast(new_data = forecast_tbl, actual_data = data_prep_tbl) %>%
  plot_modeltime_forecast(.conf_interval_fill = "lightblue")


# * Save Artifacts --------------------------------------------------------

calibration_tbl %>%
  write_rds("artifacts/calibration_ts.rds")

