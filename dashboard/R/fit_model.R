# function to generate initial split
generate_initial_split <- function(data, n_assess, assess_type) {

  splits <- timetk::time_series_split(
    data, date_var = date,
    initial = nrow(data) - n_assess,
    assess = n_assess,
    cumulative = ifelse(assess_type == "Expanding", TRUE, FALSE)
  )
  return(splits)

}

# function to generato cross validation split
generate_cv_split <- function(
    data, n_assess, assess_type, validation_type = "Time Series CV", n_folds = 5,
    seed = 1992
) {

  set.seed(seed)
  if (validation_type == "Time Series CV") {
    cv_splits <- modeltime.resample::time_series_cv(
      train_tbl, date_var = date,
      initial = nrow(train_tbl) - n_assess,
      assess = trunc(n_assess / n_folds),
      slice_limit = n_folds,
      cumulative = ifelse(assess_type == "Expanding", TRUE, FALSE)
    )
  } else {
    cv_splits <- rsample::vfold_cv(train_tbl, v = n_folds)
  }
  return(cv_splits)

}

#function to generate the recipe specification
generate_recipe_spec <- function(data, method) {

  method_type <- parse_method(method)

  if (method_type == "ts") {

    rcp_spec <- recipe(value ~ ., data = data)

  } else if (method_type == "ml" | method_type == "dl") {

    rcp_spec <- recipe(value ~ ., data = data) |>
      step_timeseries_signature(date) |>
      step_mutate(date = as.numeric(date)) |>
      step_zv(all_predictors()) |>
      step_rm(matches("(iso)|(xts)|(index.num)")) |>
      step_dummy(all_nominal(), one_hot = TRUE)

  } else if (method_type == "mix") {

    rcp_spec <- recipe(value ~ ., data = data) |>
      step_timeseries_signature(date) |>
      step_normalize(date_index.num) |>
      step_zv(all_predictors()) |>
      step_rm(matches("(iso)|(xts)")) |>
      step_dummy(all_nominal(), one_hot = TRUE)

  } else {
    stop(paste("Unknown method type", method_type))
  }

  return(rcp_spec)

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
      window_size = !!params$window_size
    ) |>
      set_engine("window_function", window_function = mean, na.rm = TRUE)

  } else if (method == "ETS") {

    if (params$auto_ets) {
      model_spec <- exp_smoothing() |>
        set_engine("ets")
    } else {
      model_spec <- exp_smoothing(
        error = !!params$error,
        trend = !!params$trend,
        season = !!params$season,
        damping = !!params$damping,
        smooth_level = !!params$smooth_level,
        smooth_trend = !!params$smooth_trend,
        smooth_seasonal = !!params$smooth_seasonal
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
        non_seasonal_ar = !!params$non_seasonal_ar,
        non_seasonal_differences = !!params$non_seasonal_differences,
        non_seasonal_ma = !!params$non_seasonal_ma,
        seasonal_ar = !!params$seasonal_ar,
        seasonal_differences = !!params$seasonal_differences,
        seasonal_ma = !!params$seasonal_ma
      ) |>
        set_engine("arima")
    }

  } else if (method == "TBATS") {

    if (params$auto_tbats) {
      model_spec <- seasonal_reg() |>
        set_engine("tbats")
    } else {
      model_spec <- seasonal_reg(
        seasonal_period_1 = !!params$tbats_seasonal_period_1,
        seasonal_period_2 = !!params$tbats_seasonal_period_2,
        seasonal_period_3 = !!params$tbats_seasonal_period_3
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
          seasonal_period_1 = !!params$stlm_seasonal_period_1,
          seasonal_period_2 = !!params$stlm_seasonal_period_2,
          seasonal_period_3 = !!params$stlm_seasonal_period_3
        ) |>
          set_engine("stlm_ets")
      }
    } else {
      if (params$auto_stlm) {
        model_spec <- seasonal_reg() |>
          set_engine("stlm_arima")
      } else {
        model_spec <- seasonal_reg(
          seasonal_period_1 = !!params$stlm_seasonal_period_1,
          seasonal_period_2 = !!params$stlm_seasonal_period_2,
          seasonal_period_3 = !!params$stlm_seasonal_period_3
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
        growth = !!params$growth,
        changepoint_num = !!params$changepoint_num,
        changepoint_range = !!params$changepoint_range,
        season = !!params$prophet_season,
        seasonality_yearly = !!params$seasonality_yearly,
        seasonality_weekly = !!params$seasonality_weekly,
        seasonality_daily = !!params$seasonality_daily,
        prior_scale_changepoints = !!params$prior_scale_changepoints,
        prior_scale_seasonality = !!params$prior_scale_seasonality,
        prior_scale_holidays = !!params$prior_scale_holidays,
        logistic_cap = !!params$logistic_cap,
        logistic_floor = !!params$logistic_floor
      ) |>
        set_engine("prophet")
    }

  } else if (method == "Linear Regression") {

    model_spec <- linear_reg(mode = "regression") |>
      set_engine(engine = "lm")

  } else if (method == "Elastic Net") {

    model_spec <- linear_reg(
      mode = "regression",
      penalty = !!params$penalty,
      mixture = !!params$mixture
    ) |>
      set_engine(engine = "glmnet")

  } else if (method == "MARS") {

    model_spec <- mars(
      mode = "regression",
      num_terms = !!params$num_terms,
      prod_degree = !!params$prod_degree,
      prune_method = !!params$prune_method
    ) |>
      set_engine("earth") # endspan = 100

  } else if (method == "KNN") {

    model_spec <- nearest_neighbor(
      mode = "regression",
      neighbors = !!params$neighbors
    ) |>
      set_engine("kknn")

  } else if (method == "SVM") {

    if (params$boundary == "Linear") {
      model_spec <- svm_linear(
        mode = "regression",
        cost = !!params$cost,
        margin = !!params$margin
      ) |>
        set_engine("kernlab")
    } else {
      model_spec <- svm_rbf(
        mode = "regression",
        cost = !!params$cost,
        margin = !!params$margin,
        rbf_sigma = !!params$rbf_sigma
      ) |>
        set_engine("kernlab")
    }

  } else if (method == "Random Forest") {

    model_spec <- rand_forest(
      mode = "regression",
      mtry = !!params$rf_mtry,
      trees = !!params$rf_trees,
      min_n = !!params$rf_min_n
    ) |>
      set_engine("ranger")

  } else if (method == "Boosted Trees") {

    if (params$boost_method == "XGBoost") {
      model_spec <- boost_tree(
        mode = "regression",
        mtry = !!params$boost_mtry,
        trees = !!params$boost_trees,
        min_n = !!params$boost_min_n,
        tree_depth = !!params$boost_tree_depth,
        learn_rate = !!params$boost_learn_rate,
        loss_reduction = !!params$boost_loss_reduction,
        sample_size = !!params$boost_sample_size
      ) |>
        set_engine("xgboost")
    } else if (params$boost_method == "LightGBM") {
      model_spec <- boost_tree(
        mode = "regression",
        mtry = !!params$boost_mtry,
        trees = !!params$boost_trees,
        min_n = !!params$boost_min_n,
        tree_depth = !!params$boost_tree_depth,
        learn_rate = !!params$boost_learn_rate,
        loss_reduction = !!params$boost_loss_reduction,
        sample_size = !!params$boost_sample_size
      ) |>
        set_engine("lightgbm")
    } else {
      stop(paste("Unknown Boosting method", params$boost_method))
    }

  } else if (method == "Cubist") {

    model_spec <- cubist_rules(
      committees = !!params$committees,
      neighbors = !!params$cub_neighbors,
      max_rules = !!params$max_rules
    ) |>
      set_engine("Cubist")

  } else if (method == "Feed-Forward") {

    model_spec <- mlp(
      mode = "regression",
      hidden_units = !!params$ff_hidden_units,
      penalty = !!params$ff_penalty,
      epochs = !!params$ff_epochs,
      dropout = !!params$ff_dropout,
      learn_rate = !!params$ff_learn_rate
    ) |>
      set_engine("nnet")

  } else if (method == "Feed-Forward AR") {

    model_spec <- nnetar_reg(
      mode = "regression",
      non_seasonal_ar = !!params$ffar_non_seasonal_ar,
      seasonal_ar = !!params$ffar_seasonal_ar,
      hidden_units = !!params$ffar_hidden_units,
      penalty = !!params$ffar_penalty,
      epochs = !!params$ffar_epochs,
      num_networks = !!params$ffar_num_networks
    ) |>
      set_engine("nnetar")

  } else if (method == "ARIMA-Boost") {

    model_spec <- arima_boost(
      mode = "regression",
      mtry = !!params$arima_boost_mtry,
      trees = !!params$arima_boost_trees,
      min_n = !!params$arima_boost_min_n,
      tree_depth = !!params$arima_boost_tree_depth,
      learn_rate = !!params$arima_boost_learn_rate,
      loss_reduction = !!params$arima_boost_loss_reduction,
      sample_size = !!params$arima_boost_sample_size
    ) |>
      set_engine("auto_arima_xgboost")

  } else if (method == "Prophet-Boost") {

    model_spec <- prophet_boost(
      mode = "regression",
      mtry = !!params$prophet_boost_mtry,
      trees = !!params$prophet_boost_trees,
      min_n = !!params$prophet_boost_min_n,
      tree_depth = !!params$prophet_boost_tree_depth,
      learn_rate = !!params$prophet_boost_learn_rate,
      loss_reduction = !!params$prophet_boost_loss_reduction,
      sample_size = !!params$prophet_boost_sample_size
    ) |>
      set_engine("prophet_xgboost")

  } else {
    stop(paste("Unknown method", method))
  }

  return(model_spec)

}

# function to generate the model specification for tuning
update_tune_model_parameters <- function(method, params) {

  # function to get default tuning parameters
  get_tune <- function(parameter, value) {
    if (value == FALSE) {
      get_default(parameter)
    } else {
      tune()
    }
  }

  mth_params <- getOption("tsf.dashboard.methods_params")[[method]]
  prm_name <- grep("_xx_", names(params), value = TRUE)
  tune_params <- clean_chr_inv(params[[prm_name]])
  is_to_tune <- mth_params %in% tune_params
  params_list <- map2(mth_params, is_to_tune, get_tune) |> set_names(mth_params)
  update_params <- c(params, params_list)

  return(update_params)

}

# function to perform grid specification
generate_grid_spec <- function(method, model_spec, grid_size, seed = 1992) {

  set.seed(seed)
  if (method %in% c("Random Forest", "Boosted Trees")) {
    grid_spec <- grid_latin_hypercube(
      hardhat::extract_parameter_set_dials(model_spec) |>
        recipes::update(mtry = mtry(range = c(1, 15))),
      size = grid_size
    )
  } else {
    grid_spec <- dials::grid_latin_hypercube(
      hardhat::extract_parameter_set_dials(model_spec),
      size = grid_size
    )
  }
  return(grid_spec)

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

  # initial split
  splits <- generate_initial_split(data, n_assess, assess_type)
  train_tbl <- training(splits) |> select(-id, -frequency)

  # recipe specification
  rcp_spec <- generate_recipe_spec(train_tbl, method)

  # model specification
  model_spec <- generate_model_spec(method, params)

  # workflow specification
  wkfl_spec <- workflow() |> add_recipe(rcp_spec) |> add_model(model_spec)

  # fitting
  wkfl_fit <- wkfl_spec |> fit(data = train_tbl)

  return(wkfl_fit)

}

# function to perform ensemble model estimation
# it is just adding the ensemble method to the modeltime table
# because no fitting is required for simple ensembles
fit_ensemble <- function(modeltime_table, ensemble_method, weights, seed = 1992) {

  set.seed(seed)
  ensemble_tbl <- modeltime::modeltime_table()

  if ("Average" %in% ensemble_method) {
    ens_tmp <- modeltime.ensemble::ensemble_average(modeltime_table, type = "mean") |>
      modeltime::modeltime_table() |>
      modeltime::update_modeltime_description(.model_id = 1, .new_model_desc = "Average")
    ensemble_tbl <- modeltime::combine_modeltime_tables(ensemble_tbl, ens_tmp)
  }

  if ("Weighted Average" %in% ensemble_method) {
    ens_tmp <- modeltime.ensemble::ensemble_weighted(
      modeltime_table, loadings = weights, scale_loadings = TRUE
    ) |>
      modeltime::modeltime_table() |>
      modeltime::update_modeltime_description(.model_id = 1, .new_model_desc = "W-Average")
    ensemble_tbl <- modeltime::combine_modeltime_tables(ensemble_tbl, ens_tmp)
  }

  if ("Median" %in% ensemble_method) {
    ens_tmp <- modeltime.ensemble::ensemble_average(modeltime_table, type = "median") |>
      modeltime::modeltime_table() |>
      modeltime::update_modeltime_description(.model_id = 1, .new_model_desc = "Median")
    ensemble_tbl <- modeltime::combine_modeltime_tables(ensemble_tbl, ens_tmp)
  }

  return(ensemble_tbl)

}

# function to perform stacking model estimation
fit_stack <- function(modeltime_table, stack_method, seed = 1992) {

  set.seed(seed)
  stack_tbl <- modeltime::modeltime_table()

  ens_model_spec <- modeltime.ensemble::ensemble_model_spec(
    modeltime_table,
    model_spec = linear_reg() |> set_engine("lm"),
    control = control_grid(verbose = TRUE)
  )

  if ("Stacked" %in% stack_method) {
    ens_tmp <- modeltime.ensemble::ensemble_stacked(modeltime_table) |>
      modeltime::modeltime_table() |>
      modeltime::update_modeltime_description(.model_id = 1, .new_model_desc = "Stacked")
    stack_tbl <- modeltime::combine_modeltime_tables(stack_tbl, ens_tmp)
  }

  return(stack_tbl)

}

# function to perform model optimization
#
fit_model_tuning <- function(
    data, method, params, n_assess, assess_type,
    validation_type = "Time Series CV",
    n_folds = 5, validation_metric = "rmse", grid_size = 10,
    seed = 1992
) {

  # check_parameters(method, params)
  set.seed(seed)

  # initial split
  splits <- generate_initial_split(data, n_assess, assess_type)
  train_tbl <- training(splits) |> select(-id, -frequency)

  # validation split
  cv_splits <- generate_cv_split(
    train_tbl, n_assess, assess_type, validation_type, n_folds, seed
  )

  # recipe specification
  rcp_spec <- generate_recipe_spec(train_tbl, method)

  # model specification
  params_new <- update_tune_model_parameters(method, params)
  model_spec <- generate_model_spec(method, params_new)

  # workflow specification
  wkfl_spec <- workflow() |> add_recipe(rcp_spec) |> add_model(model_spec)

  # grid specification
  # grid_spec <- generate_grid_spec(method, model_spec, grid_size, seed)

  # tuning
  if (n_folds > 10 | grid_size > 25) {
    doFuture::registerDoFuture()
    future::plan(strategy = "multisession", workers = parallelly::availableCores() - 1)
    message("Number of parallel workers: ", future::nbrOfWorkers())
  }
  tune_fit <- wkfl_spec |>
    tune::tune_grid(
      resamples = cv_splits,
      grid = params$grid_size, # grid_spec
      metrics = modeltime::default_forecast_accuracy_metric_set(),
      control = tune::control_grid(save_pred = FALSE, allow_par = TRUE)
    )
  future::plan(strategy = "sequential")

  # picking best model
  best_fit <- tune::show_best(tune_fit, metric = tolower(validation_metric), n = 1)

  # fitting (fit to training with optimal values)
  wkfl_fit <- wkfl_spec |> tune::finalize_workflow(best_fit) |> fit(train_tbl)

  return(wkfl_fit)

}
