# function to get default parameters values
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
    "rf_mtry" = 5, "rf_trees" = 500, "rf_min_n" = 5, # Random Forest
    "boost_mtry" = 5, "boost_trees" = 100, "boost_min_n" = 1, "boost_tree_depth" = 6, # Boosted Trees
    "boost_learn_rate" = 0.3, "boost_loss_reduction" = 0, "boost_sample_size" = 1,
    "committees" = 1, "cub_neighbors" = 0, "max_rules" = 20 # Cubist
  )

  if (return_value) {
    return(def[[parameter]])
  } else {
    return(def[parameter])
  }

}

# function to generate the model specification
generate_model_spec <- function(method, params) {

  if (method == "Naive") {

    model_spec <- naive_reg() |>
      set_engine("naive")

  } else if (method == "Seasonal Naive") {

    model_spec <- naive_reg() |>
      set_engine("snaive")

  } else if (method == "Rolling Average") {

    model_spec <- window_reg(
      window_size = params$window_size
    ) |>
      set_engine("window_function", window_function = mean, na.rm = TRUE)

  } else if (method == "ETS") {

    if (params$auto_ets) {
      model_spec <- exp_smoothing() |>
        set_engine("ets")
    } else {
      model_spec <- exp_smoothing(
        error = params$error,
        trend = params$trend,
        season = params$season,
        damping = params$damping,
        smooth_level = params$smooth_level,
        smooth_trend = params$smooth_trend,
        smooth_seasonal = params$smooth_seasonal
      ) |>
        set_engine("ets")
    }

  } else if (method == "Theta") {

    model_spec <- exp_smoothing() |>
      set_engine("theta")

  } else if (method == "SARIMA") {

    if (params$auto_arima) {
      model_spec <- arima_reg() |>
        set_engine("auto_arima")
    } else {
      model_spec <- arima_reg(
        non_seasonal_ar = params$non_seasonal_ar,
        non_seasonal_differences = params$non_seasonal_differences,
        non_seasonal_ma = params$non_seasonal_ma,
        seasonal_ar = params$seasonal_ar,
        seasonal_differences = params$seasonal_differences,
        seasonal_ma = params$seasonal_ma
      ) |>
        set_engine("arima")
    }

  } else if (method == "TBATS") {

    if (params$auto_tbats) {
      model_spec <- seasonal_reg() |>
        set_engine("tbats")
    } else {
      model_spec <- seasonal_reg(
        seasonal_period_1 = params$tbats_seasonal_period_1,
        seasonal_period_2 = params$tbats_seasonal_period_2,
        seasonal_period_3 = params$tbats_seasonal_period_3
      ) |>
        set_engine("tbats")
    }

  } else if (method == "STLM") {

    if (params$trend_model == "ETS") {
      if (params$auto_stlm) {
        model_spec <- seasonal_reg() |>
          set_engine("stlm_ets")
      } else {
        model_spec <- seasonal_reg(
          seasonal_period_1 = params$stlm_seasonal_period_1,
          seasonal_period_2 = params$stlm_seasonal_period_2,
          seasonal_period_3 = params$stlm_seasonal_period_3
        ) |>
          set_engine("stlm_ets")
      }
    } else {
      if (params$auto_stlm) {
        model_spec <- seasonal_reg() |>
          set_engine("stlm_arima")
      } else {
        model_spec <- seasonal_reg(
          seasonal_period_1 = params$stlm_seasonal_period_1,
          seasonal_period_2 = params$stlm_seasonal_period_2,
          seasonal_period_3 = params$stlm_seasonal_period_3
        ) |>
          set_engine("stlm_arima")
      }
    }

  } else if (method == "Prophet") {

    if (params$auto_prophet) {
      model_spec <- prophet_reg() |>
        set_engine("prophet")
    } else {
      model_spec <- prophet_reg(
        growth = params$growth,
        changepoint_num = params$changepoint_num,
        changepoint_range = params$changepoint_range,
        season = params$prophet_season,
        seasonality_yearly = params$seasonality_yearly,
        seasonality_weekly = params$seasonality_weekly,
        seasonality_daily = params$seasonality_daily,
        prior_scale_changepoints = params$prior_scale_changepoints,
        prior_scale_seasonality = params$prior_scale_seasonality,
        prior_scale_holidays = params$prior_scale_holidays,
        logistic_cap = params$logistic_cap,
        logistic_floor = params$logistic_floor
      ) |>
        set_engine("prophet")
    }

  } else if (method == "Linear Regression") {

    model_spec <- linear_reg(mode = "regression") |>
      set_engine(engine = "lm")

  } else if (method == "Elastic Net") {

    model_spec <- linear_reg(
      mode = "regression",
      penalty = params$penalty,
      mixture = params$mixture
    ) |>
      set_engine(engine = "glmnet")

  } else if (method == "MARS") {

    model_spec <- mars(
      mode = "regression",
      num_terms = params$num_terms,
      prod_degree = params$prod_degree,
      prune_method = params$prune_method
    ) |>
      set_engine("earth") # endspan = 100

  } else if (method == "KNN") {

    model_spec <- nearest_neighbor(
      mode = "regression",
      neighbors = params$neighbors
    ) |>
      set_engine("kknn")

  } else if (method == "Random Forest") {

    model_spec <- rand_forest(
      mode = "regression",
      mtry = params$rf_mtry,
      trees = params$rf_trees,
      min_n = params$rf_min_n
    ) |>
      set_engine("ranger")

  } else if (method == "Boosted Trees") {

    model_spec <- boost_tree(
      mode = "regression",
      mtry = params$boost_mtry,
      trees = params$boost_trees,
      min_n = params$boost_min_n,
      tree_depth = params$boost_tree_depth,
      learn_rate = params$boost_learn_rate,
      loss_reduction = params$boost_loss_reduction,
      sample_size = params$boost_sample_size
    ) |>
      set_engine("xgboost")

  } else if (method == "Cubist") {

    model_spec <- cubist_rules(
      committees = params$committees,
      neighbors = params$cub_neighbors,
      max_rules = params$max_rules
    ) |>
      set_engine("Cubist")

  } else {
    stop(paste("Unknown method", method))
  }

  return(model_spec)

}

# function to perform model estimation
# the argument assess_type is not effectively used with the split function
# because it creates train and test splits! To effectively implement expanding
# and rolling window evaluation one must rely on time_series_cv() function,
# setting assess = 1 and cumulative = TRUE or FALSE. The model fitting then
# has to be performed using the fit_resamples() function (as in tuning).
fit_model <- function(data, method, params, n_assess, assess_type, seed = 1992) {

  check_parameters(method, params)

  set.seed(seed)
  method_type <- parse_method(method)

  splits <- timetk::time_series_split(
    data, date_var = date,
    initial = nrow(data) - n_assess,
    assess = n_assess,
    cumulative = ifelse(assess_type == "Expanding", TRUE, FALSE)
  )
  train_tbl <- training(splits) |> select(-id, -frequency)

  if (method_type == "ts") {
    rcp_spec <- recipe(value ~ ., data = train_tbl)
  } else if (method_type == "ml") {
    rcp_spec <- recipe(value ~ ., data = train_tbl) |>
      step_timeseries_signature(date) |>
      step_normalize(date_index.num) |>
      step_zv(all_predictors()) |>
      step_rm(matches("(iso)|(xts)|(lbl)")) |>
      step_rm(date)
  } else {
    stop(paste("Unknown method type", method_type))
  }

  # model specification
  model_spec <- generate_model_spec(method, params)

  # fitting
  wkfl_fit <- workflow() |>
    add_recipe(rcp_spec) |>
    add_model(model_spec) |>
    fit(data = train_tbl)

  return(wkfl_fit)

}
