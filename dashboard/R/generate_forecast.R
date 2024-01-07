# function to forecast using time series methods
generate_ts_forecast <- function(data, method, params, n_future, seed = 1992) {

  future_tbl <- data |>
    future_frame(.date_var = date, .length_out = n_future)

  set.seed(seed)

  if (method == "Rolling Average") {

    check_parameters(method, params)
    wkfl_fit <- window_reg(
      window_size = params$window_size
    ) |>
      set_engine("window_function", window_function = mean, na.rm = TRUE) |>
      fit(value ~ date, data = data |> select(-id))

  } else if (method == "ETS") {

    check_parameters(method, params)
    wkfl_fit <- exp_smoothing(
      error = params$error,
      trend = params$trend,
      season = params$season,
      damping = params$damping,
      smooth_level = params$smooth_level,
      smooth_trend = params$smooth_trend,
      smooth_seasonal = params$smooth_seasonal
    ) |>
      set_engine("ets") |>
      fit(value ~ date, data = data |> select(-id))

  } else if (method == "ARIMA") {

    check_parameters(method, params)

  } else {
    stop(paste("Unknown method", method))
  }

  forecast_res <- wkfl_fit |>
    modeltime_table() |>
    modeltime_calibrate(new_data = data) |>
    modeltime_forecast(actual_data = data, new_data = future_tbl)
  model_res <- wkfl_fit |> extract_fit_engine() # model_res <- wkfl_fit |> parse_model(method)

  res <- list("forecast" = forecast_res, "model" = model_res)
  return(res)

}

# function to forecast using machine learning methods
generate_ml_forecast <- function(data, method, params, n_future, seed = 1992) {

  # time_scale <- data |>
  #   tk_index() |>
  #   tk_get_timeseries_summary() |>
  #   pull(scale)

  future_tbl <- data |>
    future_frame(.date_var = date, .length_out = n_future)

  ml_rcp <- recipe(value ~ ., data = data |> select(-id)) |>
    step_timeseries_signature(date) |>
    step_normalize(date_index.num) |>
    step_zv(all_predictors()) |>
    step_rm(matches("(iso)|(xts)|(lbl)")) |>
    step_rm(date)

  set.seed(seed)

  if (method == "Linear Regression") {

    # check_parameters(method, params)
    model_spec <- linear_reg(mode = "regression") |>
      set_engine(engine = "lm")
    wkfl_fit <- workflow() |>
      add_recipe(ml_rcp) |>
      add_model(model_spec) |>
      fit(data = data |> select(-id))

  } else if (method == "Elastic Net") {

    check_parameters(method, params)
    model_spec <- linear_reg(
      mode = "regression",
      penalty = params$penalty,
      mixture = params$mixture
    ) |>
      set_engine(engine = "glmnet")
    wkfl_fit <- workflow() |>
      add_recipe(ml_rcp) |>
      add_model(model_spec) |>
      fit(data = data |> select(-id))

  } else {
    stop(paste("Unknown method", method))
  }

  forecast_res <- wkfl_fit |>
    modeltime_table() |>
    modeltime_calibrate(new_data = data) |>
    modeltime_forecast(actual_data = data, new_data = future_tbl)
  model_res <- wkfl_fit |> extract_fit_engine() # model_res <- wkfl_fit |> parse_model(method)

  res <- list("forecast" = forecast_res, "model" = model_res)
  return(res)

}

# function to generate the forecasts
generate_forecast <- function(data, method, params, n_future, seed = 1992) {

  method_type <- parse_method(method)

  if (method_type == "ts") {
    res <- generate_ts_forecast(data, method, params, n_future, seed)
  } else if (method_type == "ml") {
    res <- generate_ml_forecast(data, method, params, n_future, seed)
  } else {
    stop(paste("Unknown method", method))
  }

  return(res)

}
