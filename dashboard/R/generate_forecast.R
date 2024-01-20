# function to forecast using time series methods
generate_forecast <- function(
    fitted_model_list, data, method, n_future, n_assess, assess_type,
    ensemble_methods = NULL, stacking_methods = NULL, confidence_level = 0.95
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
      dplyr::mutate(.conf_lvl = ifelse(.model_desc == "ACTUAL", NA_real_, .conf_lvl)) |>
      dplyr::distinct(.keep_all = TRUE)
  } else {
    oos_forecast_tbl <- refit_tbl |>
      modeltime::modeltime_forecast(
        actual_data = data, new_data = future_tbl,
        conf_interval = confidence_level, conf_method = "conformal_split"
      )
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

