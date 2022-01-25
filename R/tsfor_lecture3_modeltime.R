# Time Series Forecasting: Machine Learning and Deep Learning with R & Python ----

# Lecture 3: Tidymodels & Modeltime ---------------------------------------
# 2021/2022
# Marco Zanotti

# Goals:
# - Learn the Modeltime Workflow
# - Understand Accuracy Measurements
# - Understand the Forecast Horizon & Confidence Intervals
# - Understand refitting

# Challenges:
# - Challenge (Optional) - Modeltime



# Packages ----------------------------------------------------------------

source("R/utils.R")
source("R/packages.R")



# Data & Artifacts --------------------------------------------------------

artifacts_list <- read_rds("artifacts/feature_engineering_artifacts_list.rds")
data_prep_tbl <- artifacts_list$data$data_prep_tbl
forecast_tbl <- artifacts_list$data$forecast_tbl


# * Train / Test Sets -----------------------------------------------------

splits <- data_prep_tbl %>%
  time_series_split(assess = "8 weeks", cumulative = TRUE)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(optin_time, optins_trans)

training(splits)
testing(splits)


# * Recipes ---------------------------------------------------------------

rcp_spec_lag <- artifacts_list$recipes$rcp_spec_lag



# Tidymodels --------------------------------------------------------------

# https://www.tidymodels.org/
# https://www.tidymodels.org/find/parsnip/



# Modeltime ---------------------------------------------------------------

# https://business-science.github.io/modeltime/index.html
# https://github.com/business-science/modeltime/issues/5


# * Engines (Algorithms' Specification) -----------------------------------

# - parsnip algorithms
# - modeltime algorithms
# - set_engine()
# - Models must be fit (trained)

# ARIMA
model_fit_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(optins_trans ~ optin_time, data = training(splits))

# ARIMAX
model_spec_arima <- arima_reg() %>%
  set_engine("auto_arima")
rcp_spec_fourier <- recipe(optins_trans ~ optin_time, data = training(splits)) %>%
  step_fourier(optin_time, period = c(7, 14, 30, 90), K = 1)

# GLMNET
model_spec_glmnet <- linear_reg(penalty = 0.1, mixture = 0.5) %>%
  set_engine("glmnet")

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


# * Workflows (Fitting / Training) ----------------------------------------

# - workflow()
# - add_recipe()
# - add_model()
# - Models must be fit (trained)

# ARIMAX
wrkfl_fit_arima <- workflow() %>%
  add_recipe(rcp_spec_fourier) %>%
  add_model(model_spec_arima) %>%
  fit(training(splits))

# GLMNET
wrkfl_fit_glmnet <- workflow() %>%
  add_recipe(rcp_spec_lag) %>%
  add_model(model_spec_glmnet) %>%
  fit(training(splits))

# XGBOOST
wrkfl_fit_xgb <- workflow() %>%
  add_recipe(rcp_spec_lag) %>%
  add_model(model_spec_xgb) %>%
  fit(training(splits))


# * Calibration -----------------------------------------------------------

# - modeltime_table()
# - modeltime_calibrate()
# - Calculates residual model errors on test set
# - Gives us a true prediction error estimate when we model with confidence intervals

model_tbl <- modeltime_table(
  model_fit_arima,
  wrkfl_fit_arima,
  wrkfl_fit_glmnet,
  wrkfl_fit_xgb
) %>%
  update_model_description(3, "GLMNET - Lag Recipe")
model_tbl

calibration_tbl <- model_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl %>%
  dplyr::slice(1) %>%
  unnest(.calibration_data)


# * Evaluation ------------------------------------------------------------

# - modeltime_accuracy()
# - modeltime_forecast()
# - Calculates common accuracy measures (MAE, MAPE, MASE, RMSE, R-SQUARED)
# - Visualize the out-of-sample forecast

# Out-of-Sample
calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = TRUE, bordered = TRUE, resizable = TRUE)

# In-Sample
calibration_tbl %>%
  modeltime_accuracy(new_data = training(splits) %>% drop_na()) %>%
  table_modeltime_accuracy(.interactive = TRUE, bordered = TRUE, resizable = TRUE)

calibration_tbl %>%
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = data_prep_tbl,
    conf_interval = .8
  ) %>%
  plot_modeltime_forecast(
    .conf_interval_show = TRUE,
    .conf_interval_alpha = .5,
    .conf_interval_fill = "lightblue",
    .title = "Subscriber Forecast"
  )


# * Refitting -------------------------------------------------------------

refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = data_prep_tbl)


# * Forecasting -----------------------------------------------------------

refit_tbl %>%
  modeltime_forecast(
    # h = "16 weeks",
    new_data = forecast_tbl,
    actual_data = data_prep_tbl,
    conf_interval = .8
  ) %>%
  plot_modeltime_forecast(.conf_interval_fill = "lightblue"  )



# Extra Topics ------------------------------------------------------------

# * Residuals' Diagnostics ------------------------------------------------

# - Explore in-sample and out-of-sample residuals

residuals_out_tbl <- calibration_tbl %>%
  modeltime_residuals()
residuals_in_tbl <- calibration_tbl %>%
  modeltime_residuals(training(splits) %>% drop_na())

# Time Plot
residuals_out_tbl %>%
  plot_modeltime_residuals(.y_intercept = 0, .y_intercept_color = "blue")
residuals_in_tbl %>%
  plot_modeltime_residuals()

# ACF Plot
residuals_out_tbl %>%
  plot_modeltime_residuals(.type = "acf")
residuals_in_tbl %>%
  plot_modeltime_residuals(.type = "acf")

# Seasonality
residuals_out_tbl %>%
  plot_modeltime_residuals(.type = "seasonality")
residuals_in_tbl %>%
  plot_modeltime_residuals(.type = "seasonality")


# * Expedited Forecasting -------------------------------------------------

# - Fitted on Full dataset (No Train/Test)
# - Forecast directly without confidence intervals

model_fit_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(optins_trans ~ optin_time, data = data_prep_tbl)

model_spec_glmnet <- linear_reg(penalty = 0.1, mixture = 0.5) %>%
  set_engine("glmnet")
wrkfl_fit_glmnet <- workflow() %>%
  add_model(model_spec_glmnet) %>%
  add_recipe(rcp_spec_lag) %>%
  fit(data_prep_tbl)

model_tbl <- modeltime_table(
  model_fit_arima,
  wrkfl_fit_glmnet
)

model_tbl %>%
  modeltime_forecast(
    new_data = forecast_tbl,
    actual_data = data_prep_tbl
  ) %>%
  plot_modeltime_forecast()

