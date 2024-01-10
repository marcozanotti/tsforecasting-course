# function to forecast using time series methods
generate_forecast <- function(fitted_model, data, method, n_future, n_assess, assess_type) {

  splits <- timetk::time_series_split(
    data, date_var = date,
    initial = nrow(data) - n_assess,
    assess = n_assess,
    cumulative = ifelse(assess_type == "Expanding", TRUE, FALSE)
  )
  train_tbl <- training(splits) |> select(-id, -frequency)
  test_tbl <- testing(splits) |> select(-id, -frequency)
  future_tbl <- data |>
    future_frame(.date_var = date, .length_out = n_future)

  # model summary
  # fitted_model

  # calibration
  calibration_tbl <- fitted_model |>
    modeltime::modeltime_table() |>
    modeltime::update_modeltime_description(.model_id = 1, .new_model_desc = method) |>
    modeltime::modeltime_calibrate(new_data = test_tbl)

  # residuals
  residuals_tbl <- calibration_tbl |>
    modeltime::modeltime_residuals(new_data = train_tbl)

  # evaluation
  new_mset <- modeltime::default_forecast_accuracy_metric_set(me) # add ME to the default metric set
  accuracy_tbl <- dplyr::bind_rows(
    calibration_tbl |>
      modeltime::modeltime_accuracy(new_data = train_tbl, metric_set = new_mset) |>
      dplyr::mutate(.type = "Train"),
    calibration_tbl |>
      modeltime::modeltime_accuracy(new_data = test_tbl, metric_set = new_mset) |>
      dplyr::mutate(.type = "Test")
  )

  # test forecasting
  test_forecast_tbl <- calibration_tbl |>
    modeltime::modeltime_forecast(
      actual_data = data, new_data = test_tbl,
      conf_interval = 0.95, conf_method = "conformal_split"
    )

  # refitting
  refit_tbl <- calibration_tbl |>
    modeltime::modeltime_refit(data = data)

  # out-of-sample forecasting
  oos_forecast_tbl <- refit_tbl |>
    modeltime::modeltime_forecast(
      actual_data = data, new_data = future_tbl,
      conf_interval = 0.95, conf_method = "conformal_split"
    )

  res <- list(
    "splits" = splits,
    "fit" = fitted_model,
    "residuals" = residuals_tbl,
    "accuracy" = accuracy_tbl,
    "test_forecast" = test_forecast_tbl,
    "oos_forecast" = oos_forecast_tbl
  )
  return(res)

}
