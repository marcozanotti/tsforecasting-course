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
