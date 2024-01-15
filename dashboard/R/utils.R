# set global options
set_options <- function() {

  op <- options()
  op.tsf.dashboard <- list(
    tsf.dashboard.methods = list(
      "ts" = c("Naive", "Seasonal Naive", "Rolling Average", "ETS", "Theta", "SARIMA", "TBATS", "STLM", "Prophet"),
      "ml" = c("Linear Regression", "Elastic Net", "MARS", "KNN", "SVM", "Random Forest", "Boosted Trees", "Cubist"),
      "dl" = c("Feed-Forward", "COMING SOON!"),
      "mix" = c("Feed-Forward AR", "ARIMA-Boost", "Prophet-Boost"),
      "ens" = c("Average", "Weighted Average", "Median"),
      "stk" = c("Linear Regression", "Elastic Net")
    ),
    tsf.dashboard.methods_params = list(
      "Naive" = NULL,
      "Seasonal Naive" = NULL,
      "Rolling Average" = c("window_size"),
      "ETS" = c(
        "auto_ets", "error", "trend", "season", "damping",
        "smooth_level", "smooth_trend", "smooth_season"
      ),
      "Theta" = NULL,
      "SARIMA" = c(
        "auto_arima", "non_seasonal_ar", "non_seasonal_differences", "non_seasonal_ma",
        "seasonal_ar", "seasonal_differences", "seasonal_ma"
      ),
      "TBATS" = c("auto_tbats", "tbats_seasonal_period_1", "tbats_seasonal_period_2", "tbats_seasonal_period_3"),
      "STLM" = c("auto_stlm", "trend_model", "stlm_seasonal_period_1", "stlm_seasonal_period_2", "stlm_seasonal_period_3"),
      "Prophet" = c(
        "auto_prophet", "growth", "logistic_cap", "logistic_floor",
        "changepoint_num", "changepoint_range", "prophet_season",
        "seasonality_yearly", "seasonality_weekly", "seasonality_daily",
        "prior_scale_changepoints", "prior_scale_seasonality", "prior_scale_holidays"
      ),
      "Linear Regression" = NULL,
      "Elastic Net" = c("penalty", "mixture"),
      "MARS" = c("num_terms", "prod_degree", "prune_method"),
      "KNN" = c("neighbors"),
      "SVM" = c("boundary", "cost", "margin"),
      "Random Forest" = c("rf_mtry", "rf_trees", "rf_min_n"),
      "Boosted Trees" = c(
        "boost_method",
        "boost_mtry", "boost_trees", "boost_min_n", "boost_tree_depth",
        "boost_learn_rate", "boost_loss_reduction", "boost_sample_size"
      ),
      "Cubist" = c("committees", "cub_neighbors", "max_rules"),
      "Feed-Forward" = c("ff_hidden_units", "ff_penalty", "ff_epochs", "ff_dropout", "ff_learn_rate"),
      "Feed-Forward AR" = c(
        "ffar_non_seasonal_ar", "ffar_seasonal_ar",
        "ffar_hidden_units", "ffar_penalty", "ffar_epochs", "ffar_num_networks"
      ),
      "ARIMA-Boost" = c(
        "arima_boost_mtry", "arima_boost_trees", "arima_boost_min_n",
        "arima_boost_tree_depth", "arima_boost_learn_rate", "arima_boost_loss_reduction",
        "arima_boost_sample_size"
      ),
      "Prophet-Boost" = c(
        "prophet_boost_mtry", "prophet_boost_trees", "prophet_boost_min_n",
        "prophet_boost_tree_depth", "prophet_boost_learn_rate", "prophet_boost_loss_reduction",
        "prophet_boost_sample_size"
      )
    ),
    tsf.dashboard.transfs = c("log", "boxcox", "norm", "stand", "diff", "sdiff"),
    tsf.dashboard.test_transfs = c("test_log", "test_diff", "test_sdiff"),
    tsf.dashboard.metrics = c("mae", "mape", "mase", "smape", "rmse")
  )
  toset <- !(names(op.tsf.dashboard) %in% names(op))
  if (any(toset)) options(op.tsf.dashboard[toset])

  return(invisible(NULL))

}

# function to convert frequency from character to numeric
parse_frequency <- function(frequency) {
  if (frequency == "year") {
    freq <- 1
  } else if (frequency == "semester") {
    freq <- 2
  } else if (frequency == "quarter") {
    freq <- 4
  } else if (frequency == "month") {
    freq <- 12
  } else if (frequency == "week") {
    freq <- 52
  } else if (frequency == "bus-day") {
    freq <- 252
  } else if (frequency == "day") {
    freq <- 365
  } else if (frequency == "bus-hour") {
    freq <- 252 * 24
  } else if (frequency == "hour") {
    freq <- 365 * 24
  } else if (frequency == "bus-half-hour") {
    freq <- 252 * 48
  } else if (frequency == "half-hour") {
    freq <- 365 * 48
  } else {
    stop(paste("Unknown frequency", frequency))
  }
  return(freq)
}

# function to understand if the method is a time series or a machine learning one
parse_method <- function(method) {

  mtd <- getOption("tsf.dashboard.methods")
  if (method %in% mtd$ts) {
    res <- "ts"
  } else if (method %in% mtd$ml) {
    res <- "ml"
  } else if (method %in% mtd$dl) {
    res <- "dl"
  } else if (method %in% mtd$mix) {
    res <- "mix"
  } else if (method %in% mtd$ens) {
    res <- "ens"
  } else {
    stop(paste("Unknown method", method))
  }
  return(res)

}

# check the parameters for ts and ml methods
check_parameters <- function(method, params) {

  mtd_prm <- getOption("tsf.dashboard.methods_params")[[method]]
  if (!all(mtd_prm %in% names(params))) {
    stop(paste("Parameters for", method, "are not correct!"))
  }

}

# parse model informations
parse_model <- function(fit, method) {
  # for ETS
  # wkfl_fit$fit$desc
  # wkfl_fit$fit$models$model_1$par
}

# function to get default parameters' values
get_default <- function(parameter, return_value = TRUE) {

  def <- list(
    "window_size" = 12, # Rolling Average
    "auto_ets" = TRUE, "error" = "additive", "trend" = "none", "season" = "none", # ETS
    "damping" = "none", "smooth_level" = 0.1, "smooth_trend" = 0, "smooth_season" = 0,
    "auto_arima" = TRUE, "non_seasonal_ar" = 0, "non_seasonal_differences" = 0, # SARIMA
    "non_seasonal_ma" = 0, "seasonal_ar" = 0, "seasonal_differences" = 0, "seasonal_ma" = 0,
    "auto_tbats" = TRUE, "tbats_seasonal_period_1" = 12, # TBATS
    "tbats_seasonal_period_2" = 0, "tbats_seasonal_period_3" = 0,
    "auto_stlm" = TRUE, "trend_model" = "ETS", "stlm_seasonal_period_1" = 12, # STLM
    "stlm_seasonal_period_2" = 0, "stlm_seasonal_period_3" = 0,
    "auto_prophet" = TRUE, "growth" = "linear", "logistic_cap" = 0, "logistic_floor" = 0, # Prophet
    "changepoint_num" = 25, "changepoint_range" = 0.8, "prophet_season" = "additive",
    "seasonality_yearly" = TRUE, "seasonality_weekly" = FALSE, "seasonality_daily" = FALSE,
    "prior_scale_changepoints" = 0.5, "prior_scale_seasonality" = 10, "prior_scale_holidays" = 10,
    "penalty" = 1, "mixture" = 0.5, # Elastic Net
    "num_terms" = 20, "prod_degree" = 1, "prune_method" = "backward", # MARS
    "neighbors" = 5, # KNN
    "boundary" = "linear", "cost" = 1, "margin" = 0.1, # SVM
    "rf_mtry" = 5, "rf_trees" = 500, "rf_min_n" = 5, # Random Forest
    "boost_method" = "XGBoost", # Boosted Trees
    "boost_mtry" = 5, "boost_trees" = 100, "boost_min_n" = 1, "boost_tree_depth" = 6,
    "boost_learn_rate" = 0.3, "boost_loss_reduction" = 0, "boost_sample_size" = 1,
    "committees" = 1, "cub_neighbors" = 0, "max_rules" = 20, # Cubist
    "ff_hidden_units" = 10, "ff_penalty" = 0, "ff_epochs" = 100, "ff_dropout" = 0.1, "ff_learn_rate" = 0.3, # Feed-Forward
    "ffar_non_seasonal_ar" = 1, "ffar_seasonal_ar" = 0, # Feed-Forward AR
    "ffar_hidden_units" = 10, "ffar_penalty" = 0, "ffar_epochs" = 100, "ffar_num_networks" = 20,
    "arima_boost_mtry" = 5, "arima_boost_trees" = 100, "arima_boost_min_n" = 1, "arima_boost_tree_depth" = 6, # ARIMA-Boost
    "arima_boost_learn_rate" = 0.3, "arima_boost_loss_reduction" = 0, "arima_boost_sample_size" = 1,
    "prophet_boost_mtry" = 5, "prophet_boost_trees" = 100, "prophet_boost_min_n" = 1, "prophet_boost_tree_depth" = 6, #  Prophet-Boost
    "prophet_boost_learn_rate" = 0.3, "prophet_boost_loss_reduction" = 0, "prophet_boost_sample_size" = 1
  )

  if (return_value) {
    return(def[[parameter]])
  } else {
    return(def[parameter])
  }

}

# function to clean strings
clean_chr <- function(x) {
  stringr::str_replace_all(x, "_", " ") |>
    stringr::str_to_title()
}

# function to clean back strings
clean_chr_inv <- function(x) {
  stringr::str_replace_all(x, " ", "_") |>
    stringr::str_to_lower()
}
