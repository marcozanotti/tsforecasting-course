# Time Series Forecasting: Machine Learning and Deep Learning with R & Python ----

# Lecture 9: Hyperparameter Tuning ----------------------------------------
# 2021/2022
# Marco Zanotti

# Goals:
# - Sequential / Non-Sequential Models
# - Time Series / V-Fold Cross Validation
# - Grid Search
# - Tuning

# Challenges:
# - Challenge 3 - Hyperparameter Tuning



# Packages ----------------------------------------------------------------

source("R/utils.R")
source("R/packages.R")



# Data & Artifacts --------------------------------------------------------

artifacts_list <- read_rds("artifacts/feature_engineering_artifacts_list.rds")
data_prep_tbl <- artifacts_list$data$data_prep_tbl
forecast_tbl <- artifacts_list$data$forecast_tbl


# * Train / Test Sets -----------------------------------------------------

splits <- time_series_split(data_prep_tbl, assess = "8 weeks", cumulative = TRUE)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(optin_time, optins_trans)



# Models ------------------------------------------------------------------

calibration_ts_tbl <- read_rds("artifacts/calibration_ts.rds")
calibration_ml_tbl <- read_rds("artifacts/calibration_ml.rds")
calibration_boost_tbl <- read_rds("artifacts/calibration_boost.rds")

# combine modeltime tables (calibration removed)
model_tbl <- combine_modeltime_tables(
  calibration_ts_tbl,
  calibration_ml_tbl,
  calibration_boost_tbl
)

# calibrate models again
calibration_tbl <- model_tbl %>%
  modeltime_refit(training(splits)) %>% # refitting helps solving errors from saved models
  modeltime_calibrate(testing(splits))

calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(defaultPageSize = 40, bordered = TRUE, resizable = TRUE)



# Sequential Models Tuning ------------------------------------------------

# - Sequential Model Definition:
#   - Creates Lags internally
#   - Predicts next H observations (recursive)
#   - All data must be sequential (ordered by date)
#   - Cannot use V-Fold Cross Validation / Must use Time Series Cross Validation
# - Examples of Sequential Models:
#   - ARIMA
#   - Exponential Smoothing
#   - NNETAR

# NNETAR ------------------------------------------------------------------

wrkfl_fit_nnetar <- calibration_tbl %>%
  pluck_modeltime_model(33)
wrkfl_fit_nnetar


# * Recipes ---------------------------------------------------------------

rcp_spec_nnetar <- wrkfl_fit_nnetar %>%
  extract_preprocessor() %>%
  step_rm(starts_with("lag"))
rcp_spec_nnetar %>% prep() %>% juice() %>% glimpse()


# * Engines ---------------------------------------------------------------

wrkfl_fit_nnetar %>% extract_spec_parsnip()

model_spec_nnetar <- nnetar_reg(
  non_seasonal_ar = tune(id = "non_seasonal_ar"),
  seasonal_ar = tune(),
  seasonal_period = 7,
  hidden_units = tune(),
  num_networks = 10,
  penalty = tune(),
  epochs = 50
) %>%
  set_engine("nnetar")


# * Workflows -------------------------------------------------------------

wrkfl_tune_nnetar <- wrkfl_fit_nnetar %>%
  update_recipe(rcp_spec_nnetar) %>%
  update_model(model_spec_nnetar)


# * Cross Validation (TSCV) -----------------------------------------------

# Time Series Cross Validation
?time_series_cv

resamples_tscv_lag <- time_series_cv(
  data = training(splits) %>% drop_na(),
  cumulative = TRUE, # expanding window
  initial = "6 months",
  assess = "8 weeks",
  skip = "4 weeks",
  slice_limit = 6
)

resamples_tscv_lag %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(optin_time, optins_trans)


# * Grids -----------------------------------------------------------------

# Parameters
parameters(model_spec_nnetar)

non_seasonal_ar()
seasonal_ar()
hidden_units()
penalty()

# Latin Hypercube Grid
?grid_latin_hypercube()

set.seed(123)
grid_spec_nnetar_lh1 <- grid_latin_hypercube(
  parameters(model_spec_nnetar),
  size = 15
)

set.seed(123)
grid_spec_nnetar_lh2 <- grid_latin_hypercube(
  non_seasonal_ar(range = c(1, 3)),
  seasonal_ar(range = c(1, 2)),
  hidden_units(range = c(2, 5)),
  penalty(range = c(-4.8, -2.9), trans = scales::log10_trans()),
  size = 15
)

# types of available grids
?grid_random()
?grid_regular()
?grid_max_entropy()


# * Tuning ----------------------------------------------------------------

# - Expensive Operation (15 combinations * 2 grids * 6 slices = 180 NNETAR)
# - Parallel Processing is very important

?tune_grid()
?plan() # remember: show the difference between parallel and sequential!


# Setup Parallel Processing
registerDoFuture()
n_cores <- parallel::detectCores()
plan(strategy = cluster, workers = parallel::makeCluster(n_cores))


# Tuning 1 with TSCV
tic()
set.seed(123)
tune_res_nnetar_lh1 <- wrkfl_tune_nnetar %>%
  tune_grid(
    resamples = resamples_tscv_lag,
    grid = grid_spec_nnetar_lh1,
    metrics = default_forecast_accuracy_metric_set(),
    control = control_grid(verbose = TRUE, save_pred = TRUE)
  )
toc()

# Tuning 2 with TSCV
tic()
set.seed(123)
tune_res_nnetar_lh2 <- wrkfl_tune_nnetar %>%
  tune_grid(
    resamples = resamples_tscv_lag,
    grid = grid_spec_nnetar_lh2,
    metrics = default_forecast_accuracy_metric_set(),
    control = control_grid(verbose = TRUE, save_pred = TRUE)
  )
toc()


# Reset Sequential Plan
plan(strategy = sequential)


# Inspect Results (LH2 better)
tune_res_nnetar_lh1 %>% show_best(metric = "rmse", n = Inf)
tune_res_nnetar_lh1 %>%
  autoplot() +
  geom_smooth(se = FALSE)

tune_res_nnetar_lh2 %>% show_best(metric = "rmse", n = Inf)
tune_res_nnetar_lh2 %>%
  autoplot() +
  geom_smooth(se = FALSE)


# * Calibration, Evaluation & Plotting ------------------------------------

# Re-train the model on the whole training set
set.seed(123)
wrkfl_fit_nnetar_tscv <- wrkfl_tune_nnetar %>%
  finalize_workflow(
    tune_res_nnetar_lh2 %>%
      show_best(metric = "rmse", n = Inf) %>%
      slice_min(mean)
  ) %>%
  fit(training(splits))

# Re-evaluate the model on the whole test set
calibrate_evaluate_plot(
  wrkfl_fit_nnetar,
  wrkfl_fit_nnetar_tscv
)



# Non-Sequential Models Tuning --------------------------------------------

# - Non-Sequential Model Definition:
#   - Uses date features
#   - Lags Created Externally
#   - Spline can be modeled with random missing observations
#   - Can be Tuned using K-Fold Cross Validation
# - Examples:
#   - Machine Learning Algorithms that use Calendar Features (e.g. GLMNet, XGBoost)
#   - Prophet
# - IMPORTANT: Can use time_series_cv() or vfold_cv(). Usually better performance with vfold_cv().

# PROPHET BOOST -----------------------------------------------------------

wrkfl_fit_prophet_boost <- calibration_tbl %>%
  pluck_modeltime_model(35)
wrkfl_fit_prophet_boost


# * Recipes ---------------------------------------------------------------

wrkfl_fit_prophet_boost %>% extract_preprocessor() %>% prep() %>% juice() %>% glimpse()


# * Engines ---------------------------------------------------------------

wrkfl_fit_prophet_boost %>% extract_spec_parsnip()

model_spec_prophet_boost <- prophet_boost(
  changepoint_num = 25,
  changepoint_range = 0.8,
  seasonality_yearly = FALSE,
  seasonality_weekly = FALSE,
  seasonality_daily = FALSE,
  mtry = tune(),
  trees = 300,
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune()
) %>%
  set_engine("prophet_xgboost")


# * Workflows -------------------------------------------------------------

wrkfl_tune_prophet_boost <- wrkfl_fit_prophet_boost %>%
  update_model(model_spec_prophet_boost)


# * Cross Validation (VFCV) -----------------------------------------------

# V-FOLD Cross Validation
?vfold_cv()

set.seed(123)
resamples_vfold <- vfold_cv(training(splits), v = 10)

resamples_vfold %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(optin_time, optins_trans, .facet_ncol = 2)


# * Grids -----------------------------------------------------------------

# Parameters
parameters(model_spec_prophet_boost)

mtry()
min_n()
tree_depth()
learn_rate()
loss_reduction()

# Latin Hypercube Grid
?grid_latin_hypercube()
?update.parameters()

set.seed(123)
grid_spec_prophet_boost_lh1 <- grid_latin_hypercube(
  parameters(model_spec_prophet_boost) %>%
    update(mtry = mtry(range = c(1, 65))), # update a single parameter
  size = 15
)

set.seed(123)
grid_spec_prophet_boost_lh2 <- grid_latin_hypercube(
  mtry(range = c(1, 65)),
  min_n(),
  tree_depth(),
  learn_rate(range = c(-1.5, -0.8)),
  loss_reduction(),
  size = 15
)


# * Tuning ----------------------------------------------------------------

# - Expensive Operation
# - Parallel Processing is very important

?plan() # remember: show the difference between parallel and sequential!


# Setup Parallel Processing
registerDoFuture()
n_cores <- parallel::detectCores()
plan(strategy = cluster, workers = parallel::makeCluster(n_cores))


# Tuning 1 with VFCV
tic()
set.seed(123)
tune_res_prophet_boost_lh1 <- wrkfl_tune_prophet_boost %>%
  tune_grid(
    resamples = resamples_vfold,
    grid = grid_spec_prophet_boost_lh1,
    metrics = default_forecast_accuracy_metric_set(),
    control = control_grid(verbose = TRUE, save_pred = TRUE)
  )
toc()

# Tuning 2 with VFCV
tic()
set.seed(123)
tune_res_prophet_boost_lh2 <- wrkfl_tune_prophet_boost %>%
  tune_grid(
    resamples = resamples_vfold,
    grid = grid_spec_prophet_boost_lh2,
    metrics = default_forecast_accuracy_metric_set(),
    control = control_grid(verbose = TRUE, save_pred = TRUE)
  )
toc()


# Reset Sequential Plan
plan(strategy = sequential)


# Inspect Results (LH2 better)
tune_res_prophet_boost_lh1 %>% show_best(metric = "rmse", n = Inf)
tune_res_prophet_boost_lh1 %>%
  autoplot() +
  geom_smooth(se = FALSE)

tune_res_prophet_boost_lh2 %>% show_best(metric = "rmse", n = Inf)
tune_res_prophet_boost_lh2 %>%
  autoplot() +
  geom_smooth(se = FALSE)


# * Calibration, Evaluation & Plotting ------------------------------------

# Re-train the model on the whole training set
set.seed(123)
wrkfl_fit_prophet_boost_vfcv <- wrkfl_tune_prophet_boost %>%
  finalize_workflow(
    tune_res_prophet_boost_lh2 %>%
      show_best(metric = "rmse", n = Inf) %>%
      slice_min(mean)
  ) %>%
  fit(training(splits))

# Re-evaluate the model on the whole test set
calibrate_evaluate_plot(
  wrkfl_fit_prophet_boost,
  wrkfl_fit_prophet_boost_vfcv
)



# Tune Models' Performance ------------------------------------------------

# * Comparison ------------------------------------------------------------

# * Calibration (full)
calibration_tune_tbl <- modeltime_table(
  wrkfl_fit_nnetar,
  wrkfl_fit_nnetar_tscv,
  wrkfl_fit_prophet_boost,
  wrkfl_fit_prophet_boost_vfcv
) %>%
  update_modeltime_description(.model_id = 1, .new_model_desc = "NNETAR") %>%
  update_modeltime_description(.model_id = 2, .new_model_desc = "NNETAR - TUNED") %>%
  update_modeltime_description(.model_id = 3, .new_model_desc = "PROPHET BOOST") %>%
  update_modeltime_description(.model_id = 4, .new_model_desc = "PROPHET BOOST - TUNED") %>%
  modeltime_calibrate(testing(splits))

# * Evaluation
calibration_tune_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = TRUE, bordered = TRUE, resizable = TRUE)

calibration_tune_tbl %>%
  modeltime_forecast(new_data = testing(splits), actual_data = data_prep_tbl) %>%
  plot_modeltime_forecast()

# * Refitting & Forecasting

# Best by RMSE
model_ml_best <- calibration_tbl %>%
  select_best_id(n = 1)

refit_tbl <- calibration_tbl %>%
  filter(.model_id %in% model_ml_best) %>%
  modeltime_refit(data = data_prep_tbl)

refit_tbl %>%
  modeltime_forecast(new_data = forecast_tbl, actual_data = data_prep_tbl) %>%
  plot_modeltime_forecast(.conf_interval_fill = "lightblue")


# * Save Artifacts --------------------------------------------------------

calibration_tune_tbl %>%
  write_rds("artifacts/calibration_tune.rds")

