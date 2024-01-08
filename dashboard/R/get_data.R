# function to get the data
get_data <- function(dataset_name) { # Monthly

  if (dataset_name == "Air Passengers") {
    data <- tibble(
      "date" = seq.Date(as.Date("1949-01-01"), as.Date("1960-12-01"), by = "month"),
      "id" = "Air Passengers",
      "frequency" = "month",
      "value" = datasets::AirPassengers |> as.numeric()
    )
  } else if (dataset_name == "Electricity Demand") { # Half-Hourly
    data <- tibble(
      "date" = tsibbledata::vic_elec$Time,
      "id" = "Electricity Demand",
      "frequency" = "half-hour",
      "value" = tsibbledata::vic_elec$Demand
    )
  } else if (dataset_name == "Stock Price") { # Daily
    data <- tibble(
      "date" = tsibbledata::gafa_stock |> filter(Symbol == "AAPL") |> pull(Date),
      "id" = "Apple Stock Price",
      "frequency" = "bus-day",
      "value" = tsibbledata::gafa_stock |> filter(Symbol == "AAPL") |> pull(Adj_Close)
    )
  } else if (dataset_name == "Tobacco Prod") { # Quarterly
    data <- tibble(
      "date" = seq.Date(as.Date("1950-01-01"), as.Date("1998-04-01"), by = "quarter"),
      "id" = "Tobacco Prod",
      "frequency" = "quarter",
      "value" = tsibbledata::aus_production |> drop_na() |> pull(Tobacco)
    )
  } else if (dataset_name == "EU Population") { # Yearly
    data <- tibble(
      "date" = seq.Date(as.Date("1960-01-01"), as.Date("2017-01-01"), by = "year"),
      "id" = "EU Population",
      "frequency" = "year",
      "value" = tsibbledata::global_economy |> filter(Country == "European Union") |> pull(Population)
    )
  } else if (dataset_name == "People Traffic") { # Weekly
    data <- tibble(
      "date" = seq.Date(as.Date("2000-01-01"), as.Date("2005-06-01"), by = "week"),
      "id" = "People Traffic",
      "frequency" = "week",
      "value" = tsibbledata::ansett |> group_by(Week) |> summarise(value = sum(Passengers)) |> pull(value)
    )
  } else {
    stop(paste("Unknown dataset", dataset_name))
  }

  return(data)

}

# function to impute missing values
impute_data <- function(data, params, freq) {

  if (params$impute == FALSE) {
    return(data)
  } else {
    n2f <- trunc(nrow(data) / freq)
    p <- ifelse(n2f < 1, 1, 2)
    data_impute <- data |> mutate(value = ts_impute_vec(value, period = p, lambda = "auto"))
    return(data_impute)
  }

}

# function to transform data
transform_data <- function(data, params, freq) {

  trf_prm <- getOption("tsf.dashboard.transfs")
  if (!all(trf_prm %in% names(params))) {
    stop(paste("Unknown transformations!"))
  }

  transf_params <- c(
    params$log, params$boxcox, params$norm,
    params$stand, params$diff, params$sdiff
  ) |> as.logical()

  if (!all(transf_params) == FALSE) {
    return(data)
  } else {

    data_transf <- data

    if (params$log) { # Log
      data_transf <- data_transf |> mutate(value = log1p(value))
    }
    if (params$boxcox) { # Box-Cox
      data_transf <- data_transf |> mutate(value = box_cox_vec(value + 1, lambda = "auto"))
    }
    if (params$norm) { # Normalization
      data_transf <- data_transf |> mutate(value = normalize_vec(value))
    }
    if (params$stand) { # Standardization
      data_transf <- data_transf |> mutate(value = standardize_vec(value))
    }
    if (params$diff) { # Differencing
      data_transf <- data_transf |> mutate(value = diff_vec(value, difference = 1)) |> drop_na()
    }
    if (params$sdiff) { # Seasonal differencing
      data_transf <- data_transf |> mutate(value = diff_vec(value, difference = 1, lag = freq)) |> drop_na()
    }

    return(data_transf)
  }

}

# function to clean data from anomalies
clean_data <- function(data, params) {

  if (params$clean == FALSE) {
    return(data)
  } else {
    data_clean <- data |> mutate(value = ts_clean_vec(value))
    return(data_clean)
  }

}

