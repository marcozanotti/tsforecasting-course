# function to extract significance stars
extract_significance <- function(p_value) {

  res <- dplyr::case_when(
    p_value < 0.001 ~ "***",
    p_value >= 0.001 & p_value < 0.01 ~ "**",
    p_value >= 0.01 & p_value < 0.05 ~ "*",
    p_value >= 0.05 & p_value < 0.1 ~ ".",
    TRUE ~ ""
  )
  return(res)

}

# function to perform hypothesis testing
compute_hptests <- function(data, digits = 4) {

  # urca::ur.kpss() type = c("mu", "tau"), use.lag = NULL
  # urca::ur.pp() type = c("Z-alpha", "Z-tau"), model = c("constant", "trend"), use.lag = NULL
  # urca::ur.df() type = c("none", "drift", "trend"), lags = 1

  logging::loginfo("Hypothesis testing...")
  x <- data$value
  x <- x[which(!is.na(x))]
  k <- trunc((length(x) - 1) ^ (1 / 3))

  res <- tibble::tibble(
    "Type" = c(rep("normality", 2), rep("autocorrelation", 2), rep("stationarity", 3)),
    "Test" = c(
      "Jarque-Bera", "Shapiro-Wilk",
      "Box-Pierce", "Ljung-Box",
      "Augmented Dickey-Fuller", "Phillips-Perron", "KPSS"
    ),
    "H0" = c(
      "Normality", "Normality",
      "No autocorrelation", "No autocorrelation",
      "No stationarity", "No stationarity", "Stationarity"
    ),
    "Statistic" = rep(0, 7),
    "P_value" = rep(0, 7),
    "Signif" = rep("", 7)
  )

  # normality tests
  logging::loginfo("Normality Tests")
  tmp <- tseries::jarque.bera.test(x)
  res[1, 4] <- unname(tmp$statistic)
  res[1, 5] <- tmp$p.value
  tmp <- stats::shapiro.test(x)
  res[2, 4] <- unname(tmp$statistic)
  res[2, 5] <- tmp$p.value

  # autocorrelation tests
  logging::loginfo("Autocorrelation Tests")
  tmp <- stats::Box.test(x, type = "Box-Pierce", lag = k)
  res[3, 4] <- unname(tmp$statistic)
  res[3, 5] <- tmp$p.value
  tmp <- stats::Box.test(x, type = "Ljung-Box", lag = k)
  res[4, 4] <- unname(tmp$statistic)
  res[4, 5] <- tmp$p.value

  # stationarity
  logging::loginfo("Statinarity Tests")
  tmp <- suppressWarnings(tseries::adf.test(x, k = k))
  res[5, 4] <- unname(tmp$statistic)
  res[5, 5] <- tmp$p.value
  tmp <- suppressWarnings(tseries::pp.test(x))
  res[6, 4] <- unname(tmp$statistic)
  res[6, 5] <- tmp$p.value
  tmp <- suppressWarnings(tseries::kpss.test(x))
  res[7, 4] <- unname(tmp$statistic)
  res[7, 5] <- tmp$p.value

  res <- res |>
    dplyr::mutate(Signif = extract_significance(P_value)) |>
    dplyr::mutate(
      Statistic = round(Statistic, digits),
      P_value = round(P_value, digits)
    ) |>
    # dplyr::mutate(
    #   result = paste0(
    #     round(statistic, digits), " ", signif, " \n", "(", round(p_value, digits), ")"
    #   )
    # ) |>
    dplyr::select(-Type)
  return(res)

}
