# function to forecast using time series methods
generate_forecast <- function(
    fitted_model_list, data, method, n_future, n_assess, assess_type,
    ensemble_methods = NULL, stacking_methods = NULL, confidence_level = 0.9
  ) {

  logging::loginfo("*** Generating Forecasts ***")
  logging::loginfo(paste("Method(s):", paste0(method, collapse = ", ")))

  # initial split
  logging::loginfo("Initial Split")
  splits <- generate_initial_split(data, n_assess, assess_type)
  train_tbl <- training(splits) |> select(-id, -frequency)
  test_tbl <- testing(splits) |> select(-id, -frequency)

  # future split
  logging::loginfo("Future Frame")
  future_tbl <- future_frame(data, .date_var = date, .length_out = n_future)

  # modeltime table
  logging::loginfo("Modeltime Table")
  modeltime_tbl <- modeltime::modeltime_table()
  for (i in 1:length(method)) {
    modeltime_tbl <- modeltime_tbl |>
      modeltime::add_modeltime_model(model = fitted_model_list[[i]]) |>
      modeltime::update_modeltime_description(.model_id = i, .new_model_desc = method[i])
  }

  # ensembling
  if (!is.null(ensemble_methods)) {
    logging::loginfo("Ensembling")
    weights <- modeltime_tbl |>
      modeltime::modeltime_calibrate(new_data = test_tbl) |>
      modeltime::modeltime_accuracy(new_data = test_tbl) |>
      dplyr::transmute(rank = dplyr::min_rank(-rmse)) |>
      dplyr::pull("rank") # weights / sum(weights)
    ensemble_tbl <- fit_ensemble(modeltime_tbl, ensemble_methods, weights)
    modeltime_tbl <- modeltime::combine_modeltime_tables(modeltime_tbl, ensemble_tbl)
  }

  # stacking
  if (!is.null(stacking_methods)) {
    logging::loginfo("Stacking")
    # doFuture::registerDoFuture()
    # future::plan(strategy = "multisession", workers = parallelly::availableCores() - 1)
    cv_splits <- generate_cv_split(train_tbl, n_assess, assess_type, "Time Series CV", 5)
    rsmpl <- modeltime.resample::modeltime_fit_resamples(
      modeltime_tbl, resamples = cv_splits,
      control = control_resamples(verbose = TRUE, allow_par = TRUE)
    )
    stacking_res <- fit_stack(modeltime_tbl, stacking_methods, rsmpl)
    modeltime_tbl <- modeltime::combine_modeltime_tables(modeltime_tbl, stacking_res$tbl)
    fitted_model_list <- c(fitted_model_list, stacking_res$fit)
    # future::plan(strategy = "sequential")
  }

  # calibration
  logging::loginfo("Calibration")
  calibration_tbl <- modeltime_tbl |>
    modeltime::modeltime_calibrate(new_data = test_tbl)

  # residuals
  logging::loginfo("Residuals")
  residuals_tbl <- calibration_tbl |>
    modeltime::modeltime_residuals(new_data = train_tbl)

  # evaluation
  logging::loginfo("Accuracy Evaluation")
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
  logging::loginfo("Test Forecasting")
  test_forecast_tbl <- calibration_tbl |>
    modeltime::modeltime_forecast(
      actual_data = data, new_data = test_tbl,
      conf_interval = confidence_level, conf_method = "conformal_split"
    )

  # refitting
  logging::loginfo("Refitting")
  if (!is.null(stacking_methods)) {
    refit_tbl <- calibration_tbl |> modeltime::modeltime_refit(data = data, resamples = cv_splits)
  } else {
    refit_tbl <- calibration_tbl |> modeltime::modeltime_refit(data = data)
  }
  for (i in 1:length(method)) {
    refit_tbl <- refit_tbl |>
      modeltime::update_modeltime_description(.model_id = i, .new_model_desc = method[i])
  }

  # out-of-sample forecasting
  logging::loginfo("Out-of-Sample Forecasting")
  if (length(confidence_level) > 1) {
    conf_lvls <- set_confidence_levels(confidence_level, by = 0.05)
    oos_forecast_tbl <- purrr::map(
      conf_lvls,
      ~ modeltime::modeltime_forecast(
        refit_tbl, actual_data = data, new_data = future_tbl,
        conf_interval = ., conf_method = "conformal_split"
      )
    ) |> purrr::map2(conf_lvls, ~ dplyr::mutate(.x, .conf_lvl = .y)) |>
      dplyr::bind_rows() |>
      dplyr::mutate(.conf_lvl = ifelse(.key == "actual", NA_real_, .conf_lvl)) |>
      dplyr::distinct(.keep_all = TRUE)
  } else {
    oos_forecast_tbl <- refit_tbl |>
      modeltime::modeltime_forecast(
        actual_data = data, new_data = future_tbl,
        conf_interval = confidence_level, conf_method = "conformal_split"
      ) |>
      dplyr::mutate(.conf_lvl = ifelse(.key == "actual", NA_real_, confidence_level))
  }

  # model summary
  # fitted_model_list

  if (any(method %in% "H2O AutoML")) { h2o.shutdown(prompt = FALSE) }
  res <- list(
    "splits" = splits,
    "fit" = fitted_model_list,
    "residuals" = residuals_tbl,
    "accuracy" = accuracy_tbl,
    "test_forecast" = test_forecast_tbl,
    "oos_forecast" = oos_forecast_tbl
  )
  return(res)

}

# function to aggregate oos forecasts
aggregate_forecast <- function(
    forecasts, n_future, confidence_level = 0.9, aggregation_fun = "sum"
) {

  rate_of_change <- function(x_t, x_t1) {
    (x_t - x_t1) / x_t1 * 100
  }

  aggregation_fun <- eval(parse(text = aggregation_fun))

  frc_tbl <- forecasts |>
    dplyr::filter(.key == "actual" | .conf_lvl == as.character(confidence_level)) |>
    dplyr::select(.key, .index, .value, .conf_lo, .conf_hi) |>
    dplyr::distinct(.keep_all = TRUE)

  preds <- frc_tbl |> dplyr::filter(.key == "prediction")
  actual_t <- frc_tbl |>
    dplyr::filter(.key == "actual") |>
    dplyr::slice_tail(n = n_future)
  actual_seas_t <- frc_tbl |>
    dplyr::filter(.key == "actual") |>
    dplyr::filter(.index %in% (preds$.index - lubridate::years(1))) # previous year

  agg_preds <- preds |> dplyr::summarise(value = aggregation_fun(.value), sd = sd(.value))
  agg_conf_lo <- preds |> dplyr::summarise(value = aggregation_fun(.conf_lo), sd = sd(.value))
  agg_conf_up <- preds |> dplyr::summarise(value = aggregation_fun(.conf_hi), sd = sd(.value))
  agg_actual_t <- actual_t |> dplyr::summarise(value = aggregation_fun(.value), sd = sd(.value))
  agg_actual_seas_t <- actual_seas_t |> dplyr::summarise(value = aggregation_fun(.value), sd = sd(.value))

  delta_actual_t <- c(
    rate_of_change(agg_preds$value, agg_actual_t$value),
    rate_of_change(agg_conf_lo$value, agg_actual_t$value),
    rate_of_change(agg_conf_up$value, agg_actual_t$value)
  )
  delta_actual_seas_t <- c(
    rate_of_change(agg_actual_t$value, agg_actual_seas_t$value),
    rate_of_change(agg_preds$value, agg_actual_seas_t$value),
    rate_of_change(agg_conf_lo$value, agg_actual_seas_t$value),
    rate_of_change(agg_conf_up$value, agg_actual_seas_t$value)
  )

  res_agg <- tibble::tibble(
    "Period" = c("Previous Season", "Previous Period", NA_character_, "Forecast", "Worst Case", "Best Case"),
    "Value" = c(agg_actual_seas_t$value, agg_actual_t$value, NA_real_, agg_preds$value, agg_conf_lo$value, agg_conf_up$value),
    "Variability" = c(agg_actual_seas_t$sd, agg_actual_t$sd, NA_real_, agg_preds$sd, agg_conf_lo$sd, agg_conf_up$sd),
    "Delta PP" = c(NA_real_, NA_real_, NA_real_, delta_actual_t),
    "Delta PS" = c(NA_real_, delta_actual_seas_t[1], NA_real_, delta_actual_seas_t[2:4])
  )
  return(res_agg)

}

# function to adjust oos forecasts
adjust_forecast <- function(forecasts, adjustment) {

  frc_tbl <- forecasts |>
    dplyr::mutate(
      .value = ifelse(
        .key == "prediction", .value + (.value * (adjustment / 100)), .value
      )
    )
  return(frc_tbl)

}

# function to adjust prediction intervals
adjust_prediction_interval <- function(forecast, type = "linear", beta = 0.01) {

  adj_fun <- function(x, type = "linear", beta = 0.01) {
    if (type == "linear") {
      x + (beta * seq(0, length(x) - 1, 1))
    } else if (type == "exponential") {
      exp(log(x) + (beta * log(seq(1, length(x), 1))))
    } else {
      stop(paste("Unknown type:", type))
    }
  }

  conf_lvls <- forecast |> tidyr::drop_na() |> dplyr::pull(".conf_lvl") |> unique()
  for (lvl in conf_lvls) {
    frc_tmp <- forecast |> dplyr::filter(.conf_lvl == lvl)
    adj_conf_lo_tmp <- adj_fun(frc_tmp$.conf_lo, type, beta)
    adj_conf_hi_tmp <- adj_fun(frc_tmp$.conf_hi, type, beta)
    forecast <- forecast |>
      dplyr::mutate(
        .conf_lo = ifelse(.conf_lvl == lvl, adj_conf_lo_tmp, .conf_lo),
        .conf_hi = ifelse(.conf_lvl == lvl, adj_conf_hi_tmp, .conf_hi)
      )
  }

  return(forecast)

}
