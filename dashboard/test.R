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
  method = c("ETS", "SARIMA"),
  auto_ets = TRUE,
  error = "auto",
  trend = "auto",
  season = "auto",
  damping = "auto",
  smooth_level = 0.1,
  smooth_trend = 0.1,
  smooth_season = 0.1,
  auto_arima = TRUE,
  non_seasonal_ar = 1,
  non_seasonal_differences = 1,
  non_seasonal_ma = 1,
  seasonal_ar = 1,
  seasonal_differences = 1,
  seasonal_ma = 1
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
input <- list(
  n_future = 12,
  n_assess = 24,
  assess_type = "Rolling",
  method = c("ETS", "SARIMA", "Elastic Net"),
  ens_type = ens_methods,
  auto_ets = TRUE,
  error = "auto",
  trend = "auto",
  season = "auto",
  damping = "auto",
  smooth_level = 0.1,
  smooth_trend = 0.1,
  smooth_season = 0.1,
  auto_arima = TRUE,
  non_seasonal_ar = 1,
  non_seasonal_differences = 1,
  non_seasonal_ma = 1,
  seasonal_ar = 1,
  seasonal_differences = 1,
  seasonal_ma = 1,
  penalty = 1,
  mixture = 0.5
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
  n_assess = input$n_assess, assess_type = input$assess_type,
  ensemble_method = input$ens_type
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
  tune_elanet = c("Mixture")
)
input <- list(
  n_future = 12,
  n_assess = 24,
  assess_type = "Rolling",
  method = "Random Forest",
  valid_type = "K-Fold CV",
  n_folds = 5,
  metric = "RMSE",
  grid_size = 10,
  tune_xx_rf = c("mtry")
)

data = data_selected
params = input
n_assess = input$n_assess
assess_type = input$assess_type
method = input$method
validation_type = input$valid_type
n_folds = input$n_folds
validation_metric = input$metric
grid_size = input$grid_size
params = input

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
  ensemble_method = input$ens_type
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
