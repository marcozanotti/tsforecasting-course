# function to impute missing values
impute_data <- function(data, params, freq) {

  if (params$impute == FALSE) {
    return(data)
  } else {
    logging::loginfo("Imputing missing values...")
    n2f <- trunc(nrow(data) / freq)
    p <- ifelse(n2f < 1, 1, 2)
    data_impute <- data |> mutate(value = ts_impute_vec(value, period = p, lambda = "auto"))
    return(data_impute)
  }

}

# function to transform data
transform_data <- function(data, section, params, freq) {

  if (section == "viz_transf") {

    trf_prm <- getOption("tsf.dashboard.transfs")
    if (!all(trf_prm %in% names(params))) {
      stop(paste("Unknown transformations!"))
    }
    transf_params <- c(
      params$log, params$boxcox, params$norm,
      params$stand, params$diff, params$sdiff
    ) |> as.logical()

    if (all(!transf_params)) {
      return(data)
    } else {
      logging::loginfo("Transforming data...")
      data_transf <- data
      if (params$log) { # Log
        logging::loginfo("Log")
        data_transf <- data_transf |> mutate(value = log1p(value))
      }
      if (params$boxcox) { # Box-Cox
        logging::loginfo("Box-Cox")
        data_transf <- data_transf |> mutate(value = box_cox_vec(value + 1, lambda = "auto"))
      }
      if (params$norm) { # Normalization
        logging::loginfo("Normalization")
        data_transf <- data_transf |> mutate(value = normalize_vec(value))
      }
      if (params$stand) { # Standardization
        logging::loginfo("Standardization")
        data_transf <- data_transf |> mutate(value = standardize_vec(value))
      }
      if (params$diff) { # Differencing
        logging::loginfo("Differencing")
        data_transf <- data_transf |> mutate(value = diff_vec(value, difference = 1)) |> drop_na()
      }
      if (params$sdiff) { # Seasonal differencing
        logging::loginfo("Seasonal Differencing")
        data_transf <- data_transf |> mutate(value = diff_vec(value, difference = 1, lag = freq)) |> drop_na()
      }
      return(data_transf)
    }

  } else if (section == "test_hp") {

    trf_prm <- getOption("tsf.dashboard.test_transfs")
    if (!all(trf_prm %in% names(params))) {
      stop(paste("Unknown transformations!"))
    }
    transf_params <- c(
      params$test_log, params$test_diff, params$test_sdiff
    ) |> as.logical()

    if (all(!transf_params)) {
      return(data)
    } else {
      logging::loginfo("Transforming data...")
      data_transf <- data
      if (params$test_log) { # Log
        logging::loginfo("Log")
        data_transf <- data_transf |> mutate(value = log1p(value))
      }
      if (params$test_diff) { # Differencing
        logging::loginfo("Differencing")
        data_transf <- data_transf |> mutate(value = diff_vec(value, difference = 1)) |> drop_na()
      }
      if (params$test_sdiff) { # Seasonal differencing
        logging::loginfo("Seasonal Differencing")
        data_transf <- data_transf |> mutate(value = diff_vec(value, difference = 1, lag = freq)) |> drop_na()
      }
      return(data_transf)
    }

  } else {
    stop(paste("Unknown section", section))
  }

}

# function to clean data from anomalies
clean_data <- function(data, params) {

  if (params$clean == FALSE) {
    return(data)
  } else {
    logging::loginfo("Cleaning data from anomalies...")
    data_clean <- data |> mutate(value = ts_clean_vec(value))
    return(data_clean)
  }

}

