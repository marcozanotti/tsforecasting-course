# Helper Functions


# Function to check packages already loaded into NAMESPACE
check_namespace <- function(pkgs) {

  pkgs_notloaded <- pkgs[!pkgs %in% loadedNamespaces()]
  if (length(pkgs_notloaded) == 0) {
    res <- NULL
  } else {
    res <- pkgs_notloaded
  }
  return(res)

}


# Function to install and load the specified packages
install_and_load <- function(pkgs, repos = getOption("repos")) {

  pkgs_inst <- pkgs[!pkgs %in% installed.packages()]

  if (length(pkgs_inst) == 0) {
    lapply(pkgs, library, character.only = TRUE, quietly = TRUE)
    check_res <- check_namespace(pkgs)
    if (is.null(check_res)) {
      res <- "All packages correctly installed and loaded."
    } else {
      res <- paste0(
        "Problems loading packages ",
        paste0(check_res, collapse = ", "),
        "."
      )
    }

  } else {

    inst_res <- vector("character", length(pkgs_inst))

    for (i in seq_along(pkgs_inst)) {
      inst_res_tmp <- tryCatch(
        utils::install.packages(pkgs_inst[i], dependencies = TRUE, repos = repos, quiet = TRUE),
        error = function(e) e,
        warning = function(w) w
      )
      if (!is.null(inst_res_tmp)) {
        inst_res[i] <- inst_res_tmp$message
      }
    }

    pkgs_err <- pkgs_inst[!inst_res == ""]
    if (length(pkgs_err) == 0) {
      lapply(pkgs, library, character.only = TRUE, quietly = TRUE)
      check_res <- check_namespace(pkgs)
      if (is.null(check_res)) {
        res <- "All packages correctly installed and loaded."
      } else {
        res <- paste0(
          "Problems loading packages ",
          paste0(check_res, collapse = ", "),
          "."
        )
      }
    } else {
      pkgs_noerr <- pkgs[!pkgs %in% pkgs_err]
      lapply(pkgs_noerr, library, character.only = TRUE, quietly = TRUE)
      check_res <- check_namespace(pkgs_noerr)
      if (is.null(check_res)) {
        res <- paste0(
          "Problems installing packages ",
          paste0(pkgs_err, collapse = ", "),
          "."
        )
      } else {
        res <- c(
          paste0(
            "Problems installing packages ",
            paste0(pkgs_err, collapse = ", "),
            "."
          ),
          paste0(
            "Problems loading packages ",
            paste0(check_res, collapse = ", "),
            "."
          )
        )
      }
    }

  }

  message(toupper(
    paste0(
      "\n\n\n",
      "\n==================================================================",
      "\nResults:\n ",
      res,
      "\n=================================================================="
    )
  ))
  return(invisible(res))

}

# set global options
set_options <- function() {}

# function to understand if the method is a time series or a machine learning one
parse_method <- function(method) {

  ts_method <- c("Rolling Average", "ETS", "ARIMA")
  ml_method <- c("Linear Regression", "Elastic Net")

  if (method %in% ts_method) {
    res <- "ts"
  } else if (method %in% ml_method) {
    res <- "ml"
  } else {
    stop(paste("Unknown method", method))
  }

  return(res)

}

# check the parameters for ts and ml methods
check_parameters <- function(method, params) {}

# parse model informations
parse_model <- function(fit, method) {
  # for ETS
  wkfl_fit$fit$desc
  wkfl_fit$fit$models$model_1$par
}

# function to forecast using time series methods
ts_forecast <- function(data, method, params, n_future, seed = 1992) {

  future_tbl <- data |>
    future_frame(.date_var = date, .length_out = n_future)

  set.seed(seed)

  if (method == "Rolling Average") {

    # check_parameters(method, params)
    wkfl_fit <- window_reg(
      window_size = params$window_size
    ) |>
      set_engine("window_function", window_function = mean, na.rm = TRUE) |>
      fit(value ~ date, data = data |> select(-id))

  } else if (method == "ETS") {

    # check_parameters(method, params)
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
ml_forecast <- function(data, method, params, n_future, seed = 1992) {

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
    res <- ts_forecast(data, method, params, n_future, seed)
  } else if (method_type == "ml") {
    res <- ml_forecast(data, method, params, n_future, seed)
  } else {
    stop(paste("Unknown method", method))
  }

  return(res)

}


# aggiustare grafico al click iniziale
# aggiungere metodi e parametri come global options
# aggiungere check dei parametri


