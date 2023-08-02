# Time Series Forecasting: Machine Learning and Deep Learning with R & Python ----

# Lecture 5: Machine Learning Algorithms ----------------------------------
# Marco Zanotti

# Goals:
# - Linear Regression
# - Elastic Net
# - MARS
# - SVM
# - KNN
# - Random Forest
# - XGBoost, Light GBM, CAT Boost
# - Cubist
# - Neural Networks

# Challenges:
# - Challenge 2 - Testing New Forecasting Algorithms



# Packages ----------------------------------------------------------------

source("R/utils.R")
source("R/packages.R")

# packages treesnip and catboost have to be installed from dev versions
# remotes::install_github("curso-r/treesnip")
# devtools::install_url(
#   "https://github.com/catboost/catboost/releases/download/v1.0.0/catboost-R-Linux-1.0.0.tgz",
#   INSTALL_opts = c("--no-multiarch", "--no-test-load")
# )



# Data & Artifacts --------------------------------------------------------

artifacts_list <- read_rds("artifacts/feature_engineering_artifacts_list.rds")
data_prep_tbl <- artifacts_list$data$data_prep_tbl
forecast_tbl <- artifacts_list$data$forecast_tbl


# * Train / Test Sets -----------------------------------------------------

splits <- time_series_split(data_prep_tbl, assess = "8 weeks", cumulative = TRUE)

splits |>
  tk_time_series_cv_plan() |>
  plot_time_series_cv_plan(optin_time, optins_trans)


# * Recipes ---------------------------------------------------------------

rcp_spec <- artifacts_list$recipes$rcp_spec
rcp_spec_spline <- artifacts_list$recipes$rcp_spec_spline
rcp_spec_lag <- artifacts_list$recipes$rcp_spec_lag



# LINEAR REGRESSION -------------------------------------------------------

?linear_reg()

# - Baseline model for ML


# * Engines ---------------------------------------------------------------

model_spec_lm <- linear_reg() |>
  set_engine("lm")


# * Workflows -------------------------------------------------------------

# LM + Splines
wrkfl_fit_lm_spline <- workflow() |>
  add_model(model_spec_lm) |>
  add_recipe(rcp_spec_spline) |>
  fit(training(splits))

# LM + Lags
wrkfl_fit_lm_lag <- workflow() |>
  add_model(model_spec_lm) |>
  add_recipe(rcp_spec_lag) |>
  fit(training(splits))


# * Calibration, Evaluation & Plotting ------------------------------------

calibrate_evaluate_plot(
  wrkfl_fit_lm_spline,
  wrkfl_fit_lm_lag,
  updated_desc = c("LM - SPLINE", "LM - LAG")
)



# ELASTIC NET -------------------------------------------------------------

?linear_reg()

# - Strengths: Very good for trend
# - Weaknesses: Not as good for complex patterns (i.e. seasonality)


# * Engines ---------------------------------------------------------------

# RIDGE REGRESSION
model_spec_ridge <- linear_reg(
  mode = "regression",
  penalty = 0.01,
  mixture = 0
) |>
  set_engine("glmnet")

# LASSO
model_spec_lasso <- linear_reg(
  mode = "regression",
  penalty = 0.01,
  mixture = 1
) |>
  set_engine("glmnet")

# MIXED
model_spec_elanet <- linear_reg(
  mode = "regression",
  penalty = 0.01,
  mixture = 0.5
) |>
  set_engine("glmnet")


# * Workflows -------------------------------------------------------------

# RIDGE + Splines
wrkfl_fit_ridge_spline <- workflow() |>
  add_model(model_spec_ridge) |>
  add_recipe(rcp_spec_spline) |>
  fit(training(splits))

# RIDGE + Lags
wrkfl_fit_ridge_lag <- workflow() |>
  add_model(model_spec_ridge) |>
  add_recipe(rcp_spec_lag) |>
  fit(training(splits))

# LASSO + Splines
wrkfl_fit_lasso_spline <- workflow() |>
  add_model(model_spec_lasso) |>
  add_recipe(rcp_spec_spline) |>
  fit(training(splits))

# LASSO + Lags
wrkfl_fit_lasso_lag <- workflow() |>
  add_model(model_spec_lasso) |>
  add_recipe(rcp_spec_lag) |>
  fit(training(splits))

# MIXED + Splines
wrkfl_fit_elanet_spline <- workflow() |>
  add_model(model_spec_elanet) |>
  add_recipe(rcp_spec_spline) |>
  fit(training(splits))

# MIXED + Lags
wrkfl_fit_elanet_lag <- workflow() |>
  add_model(model_spec_elanet) |>
  add_recipe(rcp_spec_lag) |>
  fit(training(splits))


# * Calibration, Evaluation & Plotting ------------------------------------

calibrate_evaluate_plot(
  wrkfl_fit_ridge_spline,
  wrkfl_fit_ridge_lag,
  wrkfl_fit_lasso_spline,
  wrkfl_fit_lasso_lag,
  wrkfl_fit_elanet_spline,
  wrkfl_fit_elanet_lag,
  updated_desc = c(
    "RIDGE - SPLINE", "RIDGE - LAG",
    "LASSO - SPLINE", "LASSO - LAG",
    "ELANET - SPLINE", "ELANET - LAG"
  )
)



# MARS --------------------------------------------------------------------

# Multiple Adaptive Regression Splines

?mars()

# - Strengths: Best algorithm for modeling trend
# - Weaknesses:
#   - Not good for complex patterns (i.e. seasonality)
#   - Don't combine with splines! MARS makes splines.


# * Engines ---------------------------------------------------------------

# MARS
model_spec_mars <- mars(
  mode = "regression",
  num_terms = 20
) |>
  set_engine("earth", endspan = 100)


# * Workflows -------------------------------------------------------------

# MARS
wrkfl_fit_mars <- workflow() |>
  add_model(model_spec_mars) |>
  add_formula(optins_trans ~ as.numeric(optin_time)) |>
  fit(training(splits))

# MARS + Lags
wrkfl_fit_mars_lag <- workflow() |>
  add_model(model_spec_mars) |>
  add_recipe(rcp_spec_lag) |>
  fit(training(splits))


# * Calibration, Evaluation & Plotting ------------------------------------

calibrate_evaluate_plot(
  wrkfl_fit_mars,
  wrkfl_fit_mars_lag,
  updated_desc = c("MARS", "MARS + LAG")
)



# SVM ---------------------------------------------------------------------

# Support Vector Machines

?svm_linear
?svm_poly
?svm_rbf

# Strengths: Well-rounded algorithm
# Weaknesses: Needs tuned or can overfit and can be computationally inefficient


# * Engines ---------------------------------------------------------------

# SVM Linear
model_spec_svm_linear <- svm_linear(
  mode = "regression",
  cost = 10,
  margin = 0.1
) |>
  set_engine("kernlab")

# SVM Polynomial
model_spec_svm_poly <- svm_poly(
  mode = "regression",
  cost = 10,
  degree = 2,
  scale_factor = 1,
  margin = 0.1
) |>
  set_engine("kernlab")

# SVM Radial
model_spec_svm_rbf <- svm_rbf(
  mode = "regression",
  cost = 1,
  rbf_sigma = 0.01,
  margin = 0.1
) |>
  set_engine("kernlab")


# * Workflows -------------------------------------------------------------

# SVM Linear + Splines
set.seed(123)
wrkfl_fit_svm_linear_spline <- workflow() |>
  add_model(model_spec_svm_linear) |>
  add_recipe(rcp_spec_spline) |>
  fit(training(splits))

# SVM Linear + Lags
set.seed(123)
wrkfl_fit_svm_linear_lag <- workflow() |>
  add_model(model_spec_svm_linear) |>
  add_recipe(rcp_spec_lag) |>
  fit(training(splits))

# SVM Poly + Splines
set.seed(123)
wrkfl_fit_svm_poly_spline <- workflow() |>
  add_model(model_spec_svm_poly) |>
  add_recipe(rcp_spec_spline) |>
  fit(training(splits))

# SVM Poly + Lags
set.seed(123)
wrkfl_fit_svm_poly_lag <- workflow() |>
  add_model(model_spec_svm_poly) |>
  add_recipe(rcp_spec_lag) |>
  fit(training(splits))

# SVM Radial + Splines
set.seed(123)
wrkfl_fit_svm_rbf_spline <- workflow() |>
  add_model(model_spec_svm_rbf) |>
  add_recipe(rcp_spec_spline) |>
  fit(training(splits))

# SVM Radial + Lags
set.seed(123)
wrkfl_fit_svm_rbf_lag <- workflow() |>
  add_model(model_spec_svm_rbf) |>
  add_recipe(rcp_spec_lag) |>
  fit(training(splits))


# * Calibration, Evaluation & Plotting ------------------------------------

calibrate_evaluate_plot(
  wrkfl_fit_svm_linear_spline,
  wrkfl_fit_svm_linear_lag,
  wrkfl_fit_svm_poly_spline,
  wrkfl_fit_svm_poly_lag,
  wrkfl_fit_svm_rbf_spline,
  wrkfl_fit_svm_rbf_lag,
  updated_desc = c(
    "SVM Linear - Splines", "SVM Linear - Lags",
    "SVM Poly - Splines", "SVM Poly - Lags",
    "SVM RBF - Splines", "SVM RBF - Lags"
  )
)



# KNN ---------------------------------------------------------------------

# K Neighrest Neighbors

?nearest_neighbor()

# - Strengths: Uses neighboring points to estimate
# - Weaknesses: Cannot predict beyond the maximum/minimum target (e.g. increasing trend)
# - Solution: Model trend separately (if needed).
#   - Can combine with ARIMA, Linear Regression, Mars, or Prophet

# Trend Issue Example
sample_data_tbl <- tibble(
  date = tk_make_timeseries("2021", by = "quarter", length_out = 20),
  value = 1:20
)
sample_data_tbl |> plot_time_series(date, value, .smooth = FALSE)
model_fit_knn <- nearest_neighbor(mode = "regression") |>
  set_engine("kknn") |>
  fit(value ~ as.numeric(date), sample_data_tbl)
modeltime_table(model_fit_knn) |>
  modeltime_forecast(
    new_data = bind_rows(
      sample_data_tbl,
      future_frame(sample_data_tbl, .length_out = "2 years")
    ),
    actual_data = sample_data_tbl
  ) |>
  plot_modeltime_forecast()


# * Engines ---------------------------------------------------------------

model_spec_knn <- nearest_neighbor(
  mode = "regression",
  neighbors = 50,
  dist_power = 10,
  weight_func = "optimal"
) |>
  set_engine("kknn")


# * Workflows -------------------------------------------------------------

# KNN + Splines
set.seed(123)
wrkfl_fit_knn_spline <- workflow() |>
  add_model(model_spec_knn) |>
  add_recipe(rcp_spec_spline) |>
  fit(training(splits))

# KNN + Lags
set.seed(123)
wrkfl_fit_knn_lag <- workflow() |>
  add_model(model_spec_knn) |>
  add_recipe(rcp_spec_lag) |>
  fit(training(splits))


# * Calibration, Evaluation & Plotting ------------------------------------

calibrate_evaluate_plot(
  wrkfl_fit_knn_spline,
  wrkfl_fit_knn_lag,
  updated_desc = c("KNN - Splines", "KNN - Lags")
)



# RANDOM FOREST -----------------------------------------------------------

?rand_forest()

# - Strengths: Can model seasonality very well
# - Weaknesses:
#   - Cannot predict beyond the maximum/minimum target (e.g. increasing trend)
# - Solution: Model trend separately (if needed).
#   - Can combine with ARIMA, Linear Regression, Mars, or Prophet


# * Engines ---------------------------------------------------------------

model_spec_rf <- rand_forest(
  mode = "regression",
  mtry = 25,
  trees = 1000,
  min_n = 25
) |>
  set_engine("randomForest")
# set_engine("ranger") # faster implementation


# * Workflows -------------------------------------------------------------

# RF + Splines
set.seed(123)
wrkfl_fit_rf_spline <- workflow() |>
  add_model(model_spec_rf) |>
  add_recipe(rcp_spec_spline) |>
  fit(training(splits))

# RF + Lags
set.seed(123)
wrkfl_fit_rf_lag <- workflow() |>
  add_model(model_spec_rf) |>
  add_recipe(rcp_spec_lag) |>
  fit(training(splits))


# * Calibration, Evaluation & Plotting ------------------------------------

calibrate_evaluate_plot(
  wrkfl_fit_rf_spline,
  wrkfl_fit_rf_lag,
  updated_desc = c("RF - Splines", "RF - Lags")
)



# BOOSTING ----------------------------------------------------------------

?boost_tree()

# LIGHT GBM
# https://lightgbm.readthedocs.io/en/latest/
# https://github.com/microsoft/LightGBM

# CAT BOOST
# https://catboost.ai/en/docs/
# https://github.com/catboost/catboost

# - Strengths: Best for seasonality & complex patterns
# - Weaknesses:
#   - Cannot predict beyond the maximum/minimum target (e.g. increasing trend)
# - Solution: Model trend separately (if needed).
#   - Can combine with ARIMA, Linear Regression, Mars, or Prophet
#   - prophet_boost & arima_boost: Do this


# * Engines ---------------------------------------------------------------

# XGBOOST
model_spec_xgb <- boost_tree(
  mode = "regression",
  mtry = 25,
  trees = 1000,
  min_n = 2,
  tree_depth = 12,
  learn_rate = 0.3,
  loss_reduction = 0
) |>
  set_engine("xgboost")

# LIGHT GBM
model_spec_lightgbm <- boost_tree(mode = "regression") |>
  set_engine("lightgbm")
# objective = "reg:tweedie"

# CAT BOOST
# model_spec_catboost <- boost_tree(mode = "regression") |>
#   set_engine("catboost")
# loss_function = "Tweedie:variance_power=1.5"


# * Workflows -------------------------------------------------------------

# XGBOOST + Splines
set.seed(123)
wrkfl_fit_xgb_spline <- workflow() |>
  add_model(model_spec_xgb) |>
  add_recipe(rcp_spec_spline) |>
  fit(training(splits))

wrkfl_fit_xgb_spline |>
  extract_fit_parsnip() |>
  pluck("fit") |>
  xgboost::xgb.importance(model = _) |>
  xgboost::xgb.plot.importance()

# XGBOOST + Lags
set.seed(123)
wrkfl_fit_xgb_lag <- workflow() |>
  add_model(model_spec_xgb) |>
  add_recipe(rcp_spec_lag) |>
  fit(training(splits))

wrkfl_fit_xgb_lag |>
  extract_fit_parsnip() |>
  pluck("fit") |>
  xgboost::xgb.importance(model = _) |>
  xgboost::xgb.plot.importance()


# LIGHT GBM + Splines
set.seed(123)
wrkfl_fit_lightgbm_spline <- workflow() |>
  add_model(model_spec_lightgbm) |>
  add_recipe(rcp_spec_spline) |>
  fit(training(splits))

# LIGHT GBM + Lags
set.seed(123)
wrkfl_fit_lightgbm_lag <- workflow() |>
  add_model(model_spec_lightgbm) |>
  add_recipe(rcp_spec_lag) |>
  fit(training(splits))

wrkfl_fit_lightgbm_lag |>
  parsnip::extract_fit_engine() |>
  lightgbm::lgb.importance() |>
  lightgbm::lgb.plot.importance()


# CAT BOOST + Splines
# set.seed(123)
# wrkfl_fit_catboost_spline <- workflow() |>
#   add_model(model_spec_catboost) |>
#   add_recipe(rcp_spec_spline) |>
#   fit(training(splits))
#
# # CAT BOOST + Lags
# set.seed(123)
# wrkfl_fit_catboost_lag <- workflow() |>
#   add_model(model_spec_catboost) |>
#   add_recipe(rcp_spec_lag) |>
#   fit(training(splits))
#
# wrkfl_fit_catboost_lag |>
#   parsnip::extract_fit_engine() |>
#   catboost::catboost.get_feature_importance() |>
#   as_tibble(rownames = "feature") |>
#   rename(value = V1) |>
#   arrange(-value) |>
#   mutate(feature = as_factor(feature) |> fct_rev()) |>
#   dplyr::slice(1:10) |>
#   ggplot(aes(value, feature)) +
#   geom_col()


# * Calibration, Evaluation & Plotting ------------------------------------

calibrate_evaluate_plot(
  wrkfl_fit_xgb_spline,
  wrkfl_fit_xgb_lag,
  # wrkfl_fit_lightgbm_spline,
  # wrkfl_fit_lightgbm_lag,
  # wrkfl_fit_catboost_spline,
  # wrkfl_fit_catboost_lag,
  updated_desc = c(
    "XGB - Splines", "XGB - Lags"#,
    # "LIGHT GBM - Splines", "LIGHT GBM - Lags",
    # "CATBOOST - Splines", "CATBOOST - Lags"
  )
)
# learn how to train Light GBM and CAT Boost


# CUBIST ------------------------------------------------------------------

?cubist_rules()

# - Like XGBoost, but the terminal (final) nodes are fit using linear regression
# - Does better than tree-based algorithms when time series has trend
# - Can predict beyond maximum


# * Engines ---------------------------------------------------------------

model_spec_cubist <- cubist_rules(
  committees = 50,
  neighbors = 7,
  max_rules = 100
) |>
  set_engine("Cubist")


# * Workflows -------------------------------------------------------------

# CUBIST + Splines
set.seed(123)
wrkfl_fit_cubist_spline <- workflow() |>
  add_model(model_spec_cubist) |>
  add_recipe(rcp_spec_spline) |>
  fit(training(splits))

# CUBIST + Lag
set.seed(123)
wrkfl_fit_cubist_lag <- workflow() |>
  add_model(model_spec_cubist) |>
  add_recipe(rcp_spec_lag) |>
  fit(training(splits))


# * Calibration, Evaluation & Plotting ------------------------------------

calibrate_evaluate_plot(
  wrkfl_fit_cubist_spline,
  wrkfl_fit_cubist_lag,
  updated_desc = c("CUBIST - Splines", "CUBIST - Lags")
)



# NEURAL NETWORK ----------------------------------------------------------

?mlp()
?nnetar_reg()

# - Single Layer Multi-layer Perceptron Network
# - Simple network - Like linear regression with non-linear functions
# - Can improve learning by adding more hidden units, epochs, etc

# - NNET with Lagged Features (AR)
# - Is a sequential model (comes from the forecast package)
# - Must include date feature


# * Engines ---------------------------------------------------------------

# NNET
model_spec_nnet <- mlp(
  mode = "regression",
  hidden_units = 10,
  penalty = 1,
  epochs = 100
) |>
  set_engine("nnet")

# NNETAR
model_spec_nnetar <- nnetar_reg(
  non_seasonal_ar = 2,
  seasonal_ar = 1,
  hidden_units = 10,
  penalty = 10,
  num_networks = 10,
  epochs = 50
) |>
  set_engine("nnetar")


# * Workflows -------------------------------------------------------------

# NNET + Splines
set.seed(123)
wrkfl_fit_nnet_spline <- workflow() |>
  add_model(model_spec_nnet) |>
  add_recipe(rcp_spec_spline) |>
  fit(training(splits))

# NNET + Lags
set.seed(123)
wrkfl_fit_nnet_lag <-  workflow() |>
  add_model(model_spec_nnet) |>
  add_recipe(rcp_spec_lag) |>
  fit(training(splits))

# NNETAR
set.seed(123)
wrkfl_fit_nnetar <- workflow() |>
  add_model(model_spec_nnetar) |>
  add_recipe(rcp_spec) |>
  fit(training(splits) |> drop_na())


# * Calibration, Evaluation & Plotting ------------------------------------

calibrate_evaluate_plot(
  wrkfl_fit_nnet_spline,
  wrkfl_fit_nnet_lag,
  wrkfl_fit_nnetar,
  updated_desc = c("NNET - Splines", "NNET - Lags")
)



# ML Models' Performance --------------------------------------------------

# * Comparison ------------------------------------------------------------

# * Calibration (full)
calibration_tbl <- modeltime_table(
  # BASELINE
  wrkfl_fit_lm_spline,
  wrkfl_fit_lm_lag,
  # ELASTIC NET
  wrkfl_fit_ridge_spline,
  wrkfl_fit_ridge_lag,
  wrkfl_fit_lasso_spline,
  wrkfl_fit_lasso_lag,
  wrkfl_fit_elanet_spline,
  wrkfl_fit_elanet_lag,
  # MARS
  wrkfl_fit_mars,
  wrkfl_fit_mars_lag,
  # SVM
  wrkfl_fit_svm_linear_spline,
  wrkfl_fit_svm_linear_lag,
  wrkfl_fit_svm_poly_spline,
  wrkfl_fit_svm_poly_lag,
  wrkfl_fit_svm_rbf_spline,
  wrkfl_fit_svm_rbf_lag,
  # KNN
  wrkfl_fit_knn_spline,
  wrkfl_fit_knn_lag,
  # RANDOM FOREST
  wrkfl_fit_rf_spline,
  wrkfl_fit_rf_lag,
  # BOOSTING
  wrkfl_fit_xgb_spline,
  wrkfl_fit_xgb_lag,
  # CUBIST
  wrkfl_fit_cubist_spline,
  wrkfl_fit_cubist_lag,
  # NEURAL NETWORK
  wrkfl_fit_nnet_spline,
  wrkfl_fit_nnet_lag,
  wrkfl_fit_nnetar
) |>
  update_modeltime_description(.model_id = 3, .new_model_desc = "RIDGE") |>
  update_modeltime_description(.model_id = 4, .new_model_desc = "RIDGE") |>
  update_modeltime_description(.model_id = 5, .new_model_desc = "LASSO") |>
  update_modeltime_description(.model_id = 6, .new_model_desc = "LASSO") |>
  update_modeltime_description(.model_id = 7, .new_model_desc = "ELANET") |>
  update_modeltime_description(.model_id = 8, .new_model_desc = "ELANET") |>
  update_modeltime_description(.model_id = 9, .new_model_desc = "MARS") |>
  update_modeltime_description(.model_id = 10, .new_model_desc = "MARS") |>
  update_modeltime_description(.model_id = 11, .new_model_desc = "SVM Linear") |>
  update_modeltime_description(.model_id = 12, .new_model_desc = "SVM Linear") |>
  update_modeltime_description(.model_id = 13, .new_model_desc = "SVM Poly") |>
  update_modeltime_description(.model_id = 14, .new_model_desc = "SVM Poly") |>
  update_modeltime_description(.model_id = 15, .new_model_desc = "SVM RBF") |>
  update_modeltime_description(.model_id = 16, .new_model_desc = "SVM RBF") |>
  mutate(.model_desc_2 = str_c(.model_desc, rep_along(.model_desc, c(" - Spline", " - Lag")))) |>
  mutate(.model_desc = ifelse(.model_id == 27, .model_desc, .model_desc_2)) |>
  select(-.model_desc_2) |>
  modeltime_calibrate(testing(splits))

# * Calibration (best)
calibration_tbl <- modeltime_table(
  wrkfl_fit_lm_lag,
  wrkfl_fit_ridge_lag,
  wrkfl_fit_mars_lag,
  wrkfl_fit_svm_rbf_spline,
  wrkfl_fit_knn_spline,
  wrkfl_fit_rf_lag,
  wrkfl_fit_xgb_lag,
  wrkfl_fit_cubist_lag,
  wrkfl_fit_nnetar
) |>
  update_modeltime_description(.model_id = 1, .new_model_desc = "LM - Lag") |>
  update_modeltime_description(.model_id = 2, .new_model_desc = "RIDGE - Lag") |>
  update_modeltime_description(.model_id = 3, .new_model_desc = "MARS - Lag") |>
  update_modeltime_description(.model_id = 4, .new_model_desc = "SVM RBF - Spline") |>
  update_modeltime_description(.model_id = 5, .new_model_desc = "KNN - Spline") |>
  update_modeltime_description(.model_id = 6, .new_model_desc = "RF - Lag") |>
  update_modeltime_description(.model_id = 7, .new_model_desc = "XGB - Lag") |>
  update_modeltime_description(.model_id = 8, .new_model_desc = "CUBIST - Lag") |>
  modeltime_calibrate(testing(splits))

# * Evaluation
calibration_tbl |>
  modeltime_accuracy() |>
  table_modeltime_accuracy(.interactive = TRUE, bordered = TRUE, resizable = TRUE)

calibration_tbl |>
  modeltime_forecast(new_data = testing(splits), actual_data = data_prep_tbl) |>
  plot_modeltime_forecast()

# * Refitting & Forecasting

# Best by RMSE
model_ml_best <- calibration_tbl |>
  select_best_id(n = 3)

refit_tbl <- calibration_tbl |>
  filter(.model_id %in% model_ml_best) |>
  modeltime_refit(data = data_prep_tbl)

refit_tbl |>
  modeltime_forecast(new_data = forecast_tbl, actual_data = data_prep_tbl) |>
  plot_modeltime_forecast(.conf_interval_fill = "lightblue")


# * Save Artifacts --------------------------------------------------------

calibration_tbl |>
  write_rds("artifacts/calibration_ml.rds")

