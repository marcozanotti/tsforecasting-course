# TEST & EVALUATE ---------------------------------------------------------
data_selected <- get_data(datasets[1])
ts_freq <- data_selected$frequency |> unique() |> parse_frequency()
input <- list(
  n_future = 12,
  n_assess = 24,
  assess_type = "Rolling",
  method = "ETS",
  error = "auto",
  trend = "auto",
  season = "auto",
  damping = "auto",
  smooth_level = 0.1,
  smooth_trend = 0.1,
  smooth_season = 0.1
)
input <- list(
  n_future = 12,
  n_assess = 24,
  assess_type = "Rolling",
  method = "Elastic Net",
  penalty = 1,
  mixture = 0.5
)
input <- list(
  n_future = 12,
  n_assess = 24,
  assess_type = "Rolling",
  method = "Rolling Average",
  window_size = 12
)

fitted_model <- fit_model(
  data = data_selected, method = input$method, params = input,
  n_assess = input$n_assess, assess_type = input$assess_type, seed = 1992
)
forecast_results <- generate_forecast(
  fitted_model = fitted_model, data = data_selected,
  method = input$method, n_future = input$n_future,
  n_assess = input$n_assess, assess_type = input$assess_type
)

data = data_selected
method = input$method
params = input
n_assess = input$n_assess
assess_type = input$assess_type
seed = 1992
data_splits = fitted_model$splits
fitted_model = fitted_model$fit
n_future = input$n_future

forecast_results$splits |>
  tk_time_series_cv_plan() |>
  plot_time_series_cv_plan(date, value)
forecast_results$fit
forecast_results$residuals
forecast_results$accuracy
forecast_results$test_forecast |> plot_modeltime_forecast()
forecast_results$oos_forecast |> plot_modeltime_forecast()



# COMPARE -----------------------------------------------------------------
data_selected <- get_data(datasets[1])
ts_freq <- data_selected$frequency |> unique() |> parse_frequency()
input <- list(
  n_future = 12,
  n_assess = 24,
  assess_type = "Rolling",
  method = c("ETS", "SARIMA", "H2O AutoML"),
  auto_ets = get_default("auto_ets"),
  error = get_default("error"),
  trend = get_default("trend"),
  season = get_default("season"),
  damping = get_default("damping"),
  smooth_level = get_default("smooth_level"),
  smooth_trend = get_default("smooth_trend"),
  smooth_season = get_default("smooth_season"),
  auto_arima = get_default("auto_arima"),
  non_seasonal_ar = get_default("non_seasonal_ar"),
  non_seasonal_differences = get_default("non_seasonal_differences"),
  non_seasonal_ma = get_default("non_seasonal_ma"),
  seasonal_ar = get_default("seasonal_ar"),
  seasonal_differences = get_default("seasonal_differences"),
  seasonal_ma = get_default("seasonal_ma"),
  h2o_max_time = get_default("h2o_max_time"),
  h2o_max_time_model = get_default("h2o_max_time_model"),
  h2o_nfolds = get_default("h2o_nfolds"),
  h2o_metric = get_default("h2o_metric")
)

fitted_model_list <- map(
  input$method,
  ~ fit_model(
    data = data_selected, method = ., params = input,
    n_assess = input$n_assess, assess_type = input$assess_type, seed = 1992
  )
)
forecast_results <- generate_forecast(
  fitted_model_list = fitted_model_list, data = data_selected,
  method = input$method, n_future = input$n_future,
  n_assess = input$n_assess, assess_type = input$assess_type
)

res <- map(
  input$method,
  ~ fit_model(
    data = data_selected,
    method = .,
    params = input,
    n_assess = input$n_assess,
    assess_type = input$assess_type,
    seed = 1992
  )
) |>
  generate_forecast(
    data = data_selected,
    method = input$method,
    n_future = input$n_future,
    n_assess = input$n_assess,
    assess_type = input$assess_type
  )
res$accuracy |> format_accuracy(single_method = FALSE) |> filter(Type == "Test")



# COMBINE -----------------------------------------------------------------
data_selected <- get_data(datasets[1])
ts_freq <- data_selected$frequency |> unique() |> parse_frequency()
ens_methods <- getOption("tsf.dashboard.methods")[["ens"]]
ens_methods <- getOption("tsf.dashboard.methods")[["stk"]]
input <- list(
  n_future = 12,
  n_assess = 24,
  assess_type = "Rolling",
  method = c("ETS", "SARIMA", "Elastic Net", "H2O AutoML"),
  ens_type = ens_methods,
  auto_ets = get_default("auto_ets"),
  error = get_default("error"),
  trend = get_default("trend"),
  season = get_default("season"),
  damping = get_default("damping"),
  smooth_level = get_default("smooth_level"),
  smooth_trend = get_default("smooth_trend"),
  smooth_season = get_default("smooth_season"),
  auto_arima = get_default("auto_arima"),
  non_seasonal_ar = get_default("non_seasonal_ar"),
  non_seasonal_differences = get_default("non_seasonal_differences"),
  non_seasonal_ma = get_default("non_seasonal_ma"),
  seasonal_ar = get_default("seasonal_ar"),
  seasonal_differences = get_default("seasonal_differences"),
  seasonal_ma = get_default("seasonal_ma"),
  penalty = get_default("penalty"),
  mixture = get_default("mixture"),
  h2o_max_time = get_default("h2o_max_time"),
  h2o_max_time_model = get_default("h2o_max_time_model"),
  h2o_nfolds = get_default("h2o_nfolds"),
  h2o_metric = get_default("h2o_metric")
)
input <- list(
  n_future = 12,
  n_assess = 24,
  assess_type = "Rolling",
  method = c("ETS", "SARIMA"),
  stk_type = stk_methods,
  auto_ets = get_default("auto_ets"),
  error = get_default("error"),
  trend = get_default("trend"),
  season = get_default("season"),
  damping = get_default("damping"),
  smooth_level = get_default("smooth_level"),
  smooth_trend = get_default("smooth_trend"),
  smooth_season = get_default("smooth_season"),
  auto_arima = get_default("auto_arima"),
  non_seasonal_ar = get_default("non_seasonal_ar"),
  non_seasonal_differences = get_default("non_seasonal_differences"),
  non_seasonal_ma = get_default("non_seasonal_ma"),
  seasonal_ar = get_default("seasonal_ar"),
  seasonal_differences = get_default("seasonal_differences"),
  seasonal_ma = get_default("seasonal_ma")
)

fitted_model_list <- map(
  input$method,
  ~ fit_model(
    data = data_selected, method = ., params = input,
    n_assess = input$n_assess, assess_type = input$assess_type, seed = 1992
  )
)

# ensemble simple
forecast_results <- generate_forecast(
  fitted_model_list = fitted_model_list, data = data_selected,
  method = input$method, n_future = input$n_future,
  n_assess = input$n_assess, assess_type = input$assess_type,
  ensemble_methods = input$ens_type, stacking_methods = NULL
)

# stacking
forecast_results <- generate_forecast(
  fitted_model_list = fitted_model_list, data = data_selected,
  method = input$method, n_future = input$n_future,
  n_assess = input$n_assess, assess_type = input$assess_type,
  ensemble_methods = NULL, stacking_methods = input$stk_type
)

fitted_model_list = fitted_model_list
data = data_selected
method = input$method
n_future = input$n_future
n_assess = input$n_assess
assess_type = input$assess_type
ensemble_methods = NULL
stacking_methods = input$stk_type


res <- map(
  input$method,
  ~ fit_model(
    data = data_selected,
    method = .,
    params = input,
    n_assess = input$n_assess,
    assess_type = input$assess_type,
    seed = 1992
  )
) |>
  generate_forecast(
    data = data_selected,
    method = input$method,
    n_future = input$n_future,
    n_assess = input$n_assess,
    assess_type = input$assess_type
  )
res$accuracy |> format_accuracy(single_method = FALSE) |> filter(Type == "Test")



# OPTIMIZE ----------------------------------------------------------------

data_selected <- get_data(datasets[1])
ts_freq <- data_selected$frequency |> unique() |> parse_frequency()
input <- list(
  n_future = 12,
  n_assess = 24,
  assess_type = "Rolling",
  method = "Elastic Net",
  valid_type = "K-Fold CV",
  n_folds = 5,
  metric = "RMSE",
  grid_size = 10,
  tune_elanet = c("Penalty", "Mixture")
)
input <- list(
  tune_n_future = 12,
  tune_n_assess = 24,
  tune_assess_type = "Rolling",
  tune_method = "Random Forest",
  tune_valid_type = "K-Fold CV",
  tune_n_folds = 5,
  tune_valid_metric = "RMSE",
  tune_bayes = TRUE,
  tune_grid_size = 10,
  tune_rf = c("Random Predictors", "Trees")
)

data = data_selected
params = input
n_assess = input$tune_n_assess
assess_type = input$tune_assess_type
method = input$tune_method
validation_type = input$tune_valid_type
n_folds = input$tune_n_folds
validation_metric = input$tune_valid_metric
bayesian_optimization = input$tune_bayes
grid_size = input$tune_grid_size
n_future = input$tune_n_future
seed = 1992

fitted_model_list <- map(
  input$method,
  ~ fit_model_tuning(
    data = data_selected, method = ., params = input,
    n_assess = input$n_assess, assess_type = input$assess_type,
    validation_type = input$valid_type, n_folds = input$n_folds,
    validation_metric = input$metric, grid_size = input$grid_size,
    seed = 1992
  )
)
forecast_results <- generate_forecast(
  fitted_model_list = fitted_model_list, data = data_selected,
  method = input$method, n_future = input$n_future,
  n_assess = input$n_assess, assess_type = input$assess_type,
  ensemble_method = input$ens_type
)

res <- map(
  input$method,
  ~ fit_model_tuning(
    data = data_selected,
    method = .,
    params = input,
    n_assess = input$n_assess,
    assess_type = input$assess_type,
    validation_type = input$valid_type,
    n_folds = input$n_folds,
    validation_metric = input$valid_metric,
    grid_size = input$grid_size,
    seed = 1992
  )
) |>
  generate_forecast(
    data = data_selected,
    method = input$method,
    n_future = input$n_future,
    n_assess = input$n_assess,
    assess_type = input$assess_type
  )
res$accuracy |> format_accuracy(single_method = TRUE)



# SCENARIO -----------------------------------------------------------------
data_selected <- get_data(datasets[1])
ts_freq <- data_selected$frequency |> unique() |> parse_frequency()
input <- list(
  n_future = 12,
  n_assess = 24,
  assess_type = "Rolling",
  method = "ETS",
  auto_ets = TRUE,
  error = "additive",
  trend = "additive",
  season = get_default("season"),
  damping = get_default("damping"),
  smooth_level = get_default("smooth_level"),
  smooth_trend = 0.1,
  smooth_season = get_default("smooth_season"),
  confidence_level = c(0.25, 0.99)
)

data = data_selected
method = input$method
params = input
n_assess = input$n_assess
assess_type = input$assess_type
seed = 1992
n_future = input$n_future
confidence_level = input$confidence_level

fitted_model_list <- map(
  input$method,
  ~ fit_model(
    data = data_selected, method = ., params = input,
    n_assess = input$n_assess, assess_type = input$assess_type, seed = 1992
  )
)
forecast_results <- generate_forecast(
  fitted_model_list = fitted_model_list, data = data_selected,
  method = input$method, n_future = input$n_future,
  n_assess = input$n_assess, assess_type = input$assess_type,
  confidence_level = input$confidence_level
)

# !!!!! ATTENZIONE 0.95 non lo trova  per confronto numerico
forecast_results$oos_forecast |>
  dplyr::filter(.model_desc == "ACTUAL" | .conf_lvl == "0.55") |>
  plot_modeltime_forecast()

beta = 0.05
c(.2, .25, .3, .35) + beta * (1:4)










