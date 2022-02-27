# Time Series Forecasting: Machine Learning and Deep Learning with R & Python ----

# Lecture 10: Ensemble Learning -------------------------------------------
# 2021/2022
# Marco Zanotti

# Goals:
# - Ensemble Learning
# - Simple Averaging Ensemble
# - Weight Averaging Ensemble
# - Stacking and Metalearning
# - Multi-level Ensemble
# - Stacking Hyperparameter tuning
# - TSCV vs VFCV performances

# Challenges:
# - Challenge 4 - Testing Forecasting Algorithms



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


# * Recipes ---------------------------------------------------------------

rcp_spec <- artifacts_list$recipes$rcp_spec
rcp_spec_spline <- artifacts_list$recipes$rcp_spec_spline
rcp_spec_lag <- artifacts_list$recipes$rcp_spec_lag



# Models ------------------------------------------------------------------

calibration_ts_tbl <- read_rds("artifacts/calibration_ts.rds")
calibration_ml_tbl <- read_rds("artifacts/calibration_ml.rds")
calibration_boost_tbl <- read_rds("artifacts/calibration_boost.rds")
calibration_tune_tbl <- read_rds("artifacts/calibration_tune.rds")

# combine modeltime tables (calibration removed)
model_tbl <- combine_modeltime_tables(
  calibration_ts_tbl,
  calibration_ml_tbl,
  calibration_boost_tbl,
  calibration_tune_tbl
)

# calibrate models again
calibration_tbl <- model_tbl %>%
  modeltime_refit(training(splits)) %>% # refitting helps solving errors from saved models
  modeltime_calibrate(testing(splits))

calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(defaultPageSize = 40, bordered = TRUE, resizable = TRUE)


# * Sub-Models Selections -------------------------------------------------

# try to beat the tier 1 (PROPHET W/ XGBOOST ERRORS)
model_id_sel1 <- calibration_tbl %>%
  modeltime_accuracy() %>%
  arrange(rmse) %>%
  dplyr::slice(2:10) %>%
  dplyr::slice(-2, -4, -9) %>%
  pull(.model_id)

submodels_sel1_tbl <- calibration_tbl %>%
  filter(.model_id %in% model_id_sel1)
submodels_sel1_tbl

# try to improve bad models
model_id_sel2 <- calibration_tbl %>%
  modeltime_accuracy() %>%
  arrange(rmse) %>%
  dplyr::slice_tail(n = 10) %>%
  dplyr::slice(-3, -4, -5, -10) %>%
  pull(.model_id)

submodels_sel2_tbl <- calibration_tbl %>%
  filter(.model_id %in% model_id_sel2)
submodels_sel2_tbl



# AVERAGE ENSEMBLES -------------------------------------------------------

# - Concept: Use sub-model predictions as inputs, take a simple average (or median)
# - Strengths: Fast to Create (No extra training)
# - Weaknesses: Susceptible to bad models (median can be helpful in these situations)

?ensemble_average()


# * Mean Ensemble ---------------------------------------------------------

# Fitting
ensemble_fit_mean_sel1 <- submodels_sel1_tbl %>%
  ensemble_average(type = "mean")
ensemble_fit_mean_sel2 <- submodels_sel2_tbl %>%
  ensemble_average(type = "mean")

# Evaluating
calibrate_evaluate_plot(
  ensemble_fit_mean_sel1,
  ensemble_fit_mean_sel2,
  updated_desc = c("ENSEMBLE (MEAN) - TOP", "ENSEMBLE (MEAN) - DOWN")
)


# * Median Ensemble -------------------------------------------------------

# Fitting
ensemble_fit_median_sel1 <- submodels_sel1_tbl %>%
  ensemble_average(type = "median")
ensemble_fit_median_sel2 <- submodels_sel2_tbl %>%
  ensemble_average(type = "median")

# Evaluating
calibrate_evaluate_plot(
  ensemble_fit_median_sel1,
  ensemble_fit_median_sel2,
  updated_desc = c("ENSEMBLE (MEDIAN) - TOP", "ENSEMBLE (MEDIAN) - DOWN")
)



# WEIGHTED ENSEMBLES ------------------------------------------------------

# - Concept: Very similar to a simple average
# - Strength:
#   - Can improve over simple average
#   - No training involved / Just as fast as Simple Average
# - Weakness: How to decide the weights?
# - Technique: Use a simple ranking (fastest)

?ensemble_weighted()


# * Simple Ranking Ensemble -----------------------------------------------

# Loadings
loadings_sel1_tbl <- submodels_sel1_tbl %>%
  modeltime_accuracy() %>%
  mutate(rank = min_rank(-rmse)) %>%
  select(.model_id, rank)

loadings_sel2_tbl <- submodels_sel2_tbl %>%
  modeltime_accuracy() %>%
  mutate(rank = min_rank(-rmse)) %>%
  select(.model_id, rank)

# Fitting
ensemble_fit_wt_sel1 <- submodels_sel1_tbl %>%
  ensemble_weighted(loadings = loadings_sel1_tbl$rank)
ensemble_fit_wt_sel1$fit$loadings_tbl

ensemble_fit_wt_sel2 <- submodels_sel2_tbl %>%
  ensemble_weighted(loadings = loadings_sel2_tbl$rank)
ensemble_fit_wt_sel2$fit$loadings_tbl

# Evaluating
calibrate_evaluate_plot(
  ensemble_fit_wt_sel1,
  ensemble_fit_wt_sel2,
  updated_desc = c("ENSEMBLE (WEIGHTED) - TOP", "ENSEMBLE (WEIGHTED) - DOWN")
)

calibration_tbl %>%
  modeltime_accuracy() %>%
  arrange(rmse) %>%
  dplyr::slice(1)
# almost reached our best model



# STACKING ENSEMBLES ------------------------------------------------------

# - Concept: Use sub-model predictions as inputs and train a meta-learner to predict
# - Penalized Regression Technique: Assigns loadings (coefficients) to a
#     Elastic Net Model (glmnet). Those loadings are weights that can be used
#     as a simple weighted prediction (this is what glmnet does).
# - Strengths: This de-weights bad models, improving predictive accuracy
# - Weaknesses: More expensive operation when using Stacking with Cross Validation.

?ensemble_model_spec()


# Setup Parallel Processing
registerDoFuture()
n_cores <- parallel::detectCores()
plan(strategy = cluster, workers = parallel::makeCluster(n_cores))


# * Cross Validation ------------------------------------------------------

?time_series_cv()
?vfold_cv()

# TSCV
resamples_tscv <- training(splits) %>%
  drop_na() %>%
  time_series_cv(
    cumulative = TRUE,
    initial = "6 months",
    assess = "8 weeks",
    skip = "4 weeks",
    slice_limit = 6
  )
resamples_tscv %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(optin_time, optins_trans)

# VFCV
set.seed(123)
resamples_vfold <- vfold_cv(training(splits), v = 10)
resamples_vfold %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(optin_time, optins_trans, .facet_ncol = 2)


# * Resampling ------------------------------------------------------------


?modeltime_fit_resamples()

# Resampling with TSCV
submodels_resamples_tscv_sel1_tbl <- submodels_sel1_tbl %>%
  modeltime_fit_resamples(
    resamples = resamples_tscv,
      verbose = TRUE,
      allow_par = TRUE,
      pkgs = c("Cubist", "rules")
    )
  )
submodels_resamples_tscv_sel1_tbl$.resample_results[[1]]$.predictions

submodels_resamples_tscv_sel2_tbl <- submodels_sel2_tbl %>%
  modeltime_fit_resamples(
    resamples = resamples_tscv,
    control = control_resamples(
      verbose = TRUE,
      allow_par = TRUE,
      pkgs = c("Cubist", "rules")
    )
  )

# Resampling with VFCV
submodels_resamples_vfold_sel1_tbl <- submodels_sel1_tbl %>%
  modeltime_fit_resamples(
    resamples = resamples_vfold,
    control = control_resamples(
      verbose = TRUE,
      allow_par = TRUE,
      pkgs = c("Cubist", "rules")
    )
  )

submodels_resamples_vfold_sel2_tbl <- submodels_sel2_tbl %>%
  modeltime_fit_resamples(
    resamples = resamples_vfold,
    control = control_resamples(
      verbose = TRUE,
      allow_par = TRUE,
      pkgs = c("Cubist", "rules")
    )
  )


# * LM Stack --------------------------------------------------------------

# LM with TSCV
set.seed(123)
ensemble_fit_lm_tscv_sel1 <- submodels_resamples_tscv_sel1_tbl %>%
  ensemble_model_spec(
    model_spec = linear_reg() %>% set_engine("lm"),
    control = control_grid(verbose = TRUE)
  )

set.seed(123)
ensemble_fit_lm_tscv_sel2 <- submodels_resamples_tscv_sel2_tbl %>%
  ensemble_model_spec(
    model_spec = linear_reg() %>% set_engine("lm"),
    control = control_grid(verbose = TRUE)
  )

# LM with VFCV
set.seed(123)
ensemble_fit_lm_vfold_sel1 <- submodels_resamples_vfold_sel1_tbl %>%
  ensemble_model_spec(
    model_spec = linear_reg() %>% set_engine("lm"),
    control = control_grid(verbose = TRUE)
  )

set.seed(123)
ensemble_fit_lm_vfold_sel2 <- submodels_resamples_vfold_sel2_tbl %>%
  ensemble_model_spec(
    model_spec = linear_reg() %>% set_engine("lm"),
    control = control_grid(verbose = TRUE)
  )

# Evaluating
calibrate_evaluate_plot(
  ensemble_fit_lm_tscv_sel1,
  ensemble_fit_lm_tscv_sel2,
  ensemble_fit_lm_vfold_sel1,
  ensemble_fit_lm_vfold_sel2,
  updated_desc = c(
    "ENSEMBLE (LM STACK TSCV) - TOP", "ENSEMBLE (LM STACK TSCV) - DOWN",
    "ENSEMBLE (LM STACK VFCV) - TOP", "ENSEMBLE (LM STACK VFCV) - DOWN"
    )
)


# * ELASTIC NET Stack -----------------------------------------------------

# ELASTIC NET with TSCV
set.seed(123)
ensemble_fit_elanet_tscv_sel1 <- submodels_resamples_tscv_sel1_tbl %>%
  ensemble_model_spec(
    model_spec = linear_reg(
      penalty = tune(),
      mixture = tune()
    ) %>%
      set_engine("glmnet"),
    kfolds = 10,
    grid = 10,
    control = control_grid(
      verbose   = TRUE,
      allow_par = TRUE
    )
  )

set.seed(123)
ensemble_fit_elanet_tscv_sel2 <- submodels_resamples_tscv_sel2_tbl %>%
  ensemble_model_spec(
    model_spec = linear_reg(
      penalty = tune(),
      mixture = tune()
    ) %>%
      set_engine("glmnet"),
    kfolds = 10,
    grid = 10,
    control = control_grid(
      verbose   = TRUE,
      allow_par = TRUE
    )
  )

# ELASTIC NET with VFCV
set.seed(123)
ensemble_fit_elanet_vfold_sel1 <- submodels_resamples_vfold_sel1_tbl %>%
  ensemble_model_spec(
    model_spec = linear_reg(
      penalty = tune(),
      mixture = tune()
    ) %>%
      set_engine("glmnet"),
    kfolds = 10,
    grid = 10,
    control = control_grid(
      verbose   = TRUE,
      allow_par = TRUE
    )
  )

set.seed(123)
ensemble_fit_elanet_vfold_sel2 <- submodels_resamples_vfold_sel2_tbl %>%
  ensemble_model_spec(
    model_spec = linear_reg(
      penalty = tune(),
      mixture = tune()
    ) %>%
      set_engine("glmnet"),
    kfolds = 10,
    grid = 10,
    control = control_grid(
      verbose   = TRUE,
      allow_par = TRUE
    )
  )

# Evaluating
calibrate_evaluate_plot(
  ensemble_fit_elanet_tscv_sel1,
  ensemble_fit_elanet_tscv_sel2,
  ensemble_fit_elanet_vfold_sel1,
  ensemble_fit_elanet_vfold_sel2,
  updated_desc = c(
    "ENSEMBLE (ELANET STACK TSCV) - TOP", "ENSEMBLE (ELANET STACK TSCV) - DOWN",
    "ENSEMBLE (ELANET STACK VFCV) - TOP", "ENSEMBLE (ELANET STACK VFCV) - DOWN"
  )
)


# * SVM Stack -------------------------------------------------------------

# SVM with TSCV
set.seed(123)
ensemble_fit_svm_tscv_sel1 <- submodels_resamples_tscv_sel1_tbl %>%
  ensemble_model_spec(
    model_spec = svm_rbf(
      mode = "regression",
      cost = tune(),
      rbf_sigma = tune(),
      margin = tune()
    ) %>%
      set_engine("kernlab"),
    kfolds = 10,
    grid = 10,
    control = control_grid(
      verbose   = TRUE,
      allow_par = TRUE
    )
  )

set.seed(123)
ensemble_fit_svm_tscv_sel2 <- submodels_resamples_tscv_sel2_tbl %>%
  ensemble_model_spec(
    model_spec = svm_rbf(
      mode = "regression",
      cost = tune(),
      rbf_sigma = tune(),
      margin = tune()
    ) %>%
      set_engine("kernlab"),
    kfolds = 10,
    grid = 10,
    control = control_grid(
      verbose   = TRUE,
      allow_par = TRUE
    )
  )

# SVM with VFCV
set.seed(123)
ensemble_fit_svm_vfold_sel1 <- submodels_resamples_vfold_sel1_tbl %>%
  ensemble_model_spec(
    model_spec = svm_rbf(
      mode = "regression",
      cost = tune(),
      rbf_sigma = tune(),
      margin = tune()
    ) %>%
      set_engine("kernlab"),
    kfolds = 10,
    grid = 10,
    control = control_grid(
      verbose   = TRUE,
      allow_par = TRUE
    )
  )

set.seed(123)
ensemble_fit_svm_vfold_sel2 <- submodels_resamples_vfold_sel2_tbl %>%
  ensemble_model_spec(
    model_spec = svm_rbf(
      mode = "regression",
      cost = tune(),
      rbf_sigma = tune(),
      margin = tune()
    ) %>%
      set_engine("kernlab"),
    kfolds = 10,
    grid = 10,
    control = control_grid(
      verbose   = TRUE,
      allow_par = TRUE
    )
  )

# Evaluating
calibrate_evaluate_plot(
  ensemble_fit_svm_tscv_sel1,
  ensemble_fit_svm_tscv_sel2,
  ensemble_fit_svm_vfold_sel1,
  ensemble_fit_svm_vfold_sel2,
  updated_desc = c(
    "ENSEMBLE (SVM STACK TSCV) - TOP", "ENSEMBLE (SVM STACK TSCV) - DOWN",
    "ENSEMBLE (SVM STACK VFCV) - TOP", "ENSEMBLE (SVM STACK VFCV) - DOWN"
  )
)


# * RANDOM FOREST Stack ---------------------------------------------------

# RF with TSCV
set.seed(123)
ensemble_fit_rf_tscv_sel1 <- submodels_resamples_tscv_sel1_tbl %>%
  ensemble_model_spec(
    model_spec = rand_forest(
      trees = tune(),
      min_n = tune()
    ) %>%
      set_engine("ranger"),
    kfolds = 10,
    grid = 10,
    control = control_grid(
      verbose   = TRUE,
      allow_par = TRUE
    )
  )

set.seed(123)
ensemble_fit_rf_tscv_sel2 <- submodels_resamples_tscv_sel2_tbl %>%
  ensemble_model_spec(
    model_spec = rand_forest(
      trees = tune(),
      min_n = tune()
    ) %>%
      set_engine("ranger"),
    kfolds = 10,
    grid = 10,
    control = control_grid(
      verbose   = TRUE,
      allow_par = TRUE
    )
  )

# RF with VFCV
set.seed(123)
ensemble_fit_rf_vfold_sel1 <- submodels_resamples_vfold_sel1_tbl %>%
  ensemble_model_spec(
    model_spec = rand_forest(
      trees = tune(),
      min_n = tune()
    ) %>%
      set_engine("ranger"),
    kfolds = 10,
    grid = 10,
    control = control_grid(
      verbose   = TRUE,
      allow_par = TRUE
    )
  )

set.seed(123)
ensemble_fit_rf_vfold_sel2 <- submodels_resamples_vfold_sel2_tbl %>%
  ensemble_model_spec(
    model_spec = rand_forest(
      trees = tune(),
      min_n = tune()
    ) %>%
      set_engine("ranger"),
    kfolds = 10,
    grid = 10,
    control = control_grid(
      verbose   = TRUE,
      allow_par = TRUE
    )
  )

# Evaluating
calibrate_evaluate_plot(
  ensemble_fit_rf_tscv_sel1,
  ensemble_fit_rf_tscv_sel2,
  ensemble_fit_rf_vfold_sel1,
  ensemble_fit_rf_vfold_sel2,
  updated_desc = c(
    "ENSEMBLE (RF STACK TSCV) - TOP", "ENSEMBLE (RF STACK TSCV) - DOWN",
    "ENSEMBLE (RF STACK VFCV) - TOP", "ENSEMBLE (RF STACK VFCV) - DOWN"
  )
)


# * BOOSTING Stack --------------------------------------------------------

# XGBOOST with TSCV
set.seed(123)
ensemble_fit_xgb_tscv_sel1 <- submodels_resamples_tscv_sel1_tbl %>%
  ensemble_model_spec(
    model_spec = boost_tree(
      trees = tune(),
      tree_depth = tune(),
      learn_rate = tune(),
      loss_reduction = tune()
    ) %>%
      set_engine("xgboost"),
    kfolds = 10,
    grid = 10,
    control = control_grid(
      verbose   = TRUE,
      allow_par = TRUE
    )
  )

set.seed(123)
ensemble_fit_xgb_tscv_sel2 <- submodels_resamples_tscv_sel2_tbl %>%
  ensemble_model_spec(
    model_spec = boost_tree(
      trees = tune(),
      tree_depth = tune(),
      learn_rate = tune(),
      loss_reduction = tune()
    ) %>%
      set_engine("xgboost"),
    kfolds = 10,
    grid = 10,
    control = control_grid(
      verbose   = TRUE,
      allow_par = TRUE
    )
  )

# XGBOOST with VFCV
set.seed(123)
ensemble_fit_xgb_vfold_sel1 <- submodels_resamples_vfold_sel1_tbl %>%
  ensemble_model_spec(
    model_spec = boost_tree(
      trees = tune(),
      tree_depth = tune(),
      learn_rate = tune(),
      loss_reduction = tune()
    ) %>%
      set_engine("xgboost"),
    kfolds = 10,
    grid = 10,
    control = control_grid(
      verbose   = TRUE,
      allow_par = TRUE
    )
  )

set.seed(123)
ensemble_fit_xgb_vfold_sel2 <- submodels_resamples_vfold_sel2_tbl %>%
  ensemble_model_spec(
    model_spec = boost_tree(
      trees = tune(),
      tree_depth = tune(),
      learn_rate = tune(),
      loss_reduction = tune()
    ) %>%
      set_engine("xgboost"),
    kfolds = 10,
    grid = 10,
    control = control_grid(
      verbose   = TRUE,
      allow_par = TRUE
    )
  )

# Evaluating
calibrate_evaluate_plot(
  ensemble_fit_xgb_tscv_sel1,
  ensemble_fit_xgb_tscv_sel2,
  ensemble_fit_xgb_vfold_sel1,
  ensemble_fit_xgb_vfold_sel2,
  updated_desc = c(
    "ENSEMBLE (XGB STACK TSCV) - TOP", "ENSEMBLE (XGB STACK TSCV) - DOWN",
    "ENSEMBLE (XGB STACK VFCV) - TOP", "ENSEMBLE (XGB STACK VFCV) - DOWN"
  )
)


# * CUBIST Stack ----------------------------------------------------------

# CUBIST with TSCV
set.seed(123)
ensemble_fit_cubist_tscv_sel1 <- submodels_resamples_tscv_sel1_tbl %>%
  ensemble_model_spec(
    model_spec = cubist_rules(
      committees = tune(),
      neighbors = tune(),
      max_rules = tune()
    ) %>%
      set_engine("Cubist"),
    kfolds = 10,
    grid = 10,
    control = control_grid(
      verbose   = TRUE,
      allow_par = TRUE
    )
  )

set.seed(123)
ensemble_fit_cubist_tscv_sel2 <- submodels_resamples_tscv_sel2_tbl %>%
  ensemble_model_spec(
    model_spec = cubist_rules(
      committees = tune(),
      neighbors = tune(),
      max_rules = tune()
    ) %>%
      set_engine("Cubist"),
    kfolds = 10,
    grid = 10,
    control = control_grid(
      verbose   = TRUE,
      allow_par = TRUE
    )
  )

# CUBIST with VFCV
set.seed(123)
ensemble_fit_cubist_vfold_sel1 <- submodels_resamples_vfold_sel1_tbl %>%
  ensemble_model_spec(
    model_spec = cubist_rules(
      committees = tune(),
      neighbors = tune(),
      max_rules = tune()
    ) %>%
      set_engine("Cubist"),
    kfolds = 10,
    grid = 10,
    control = control_grid(
      verbose   = TRUE,
      allow_par = TRUE
    )
  )

set.seed(123)
ensemble_fit_cubist_vfold_sel2 <- submodels_resamples_vfold_sel2_tbl %>%
  ensemble_model_spec(
    model_spec = cubist_rules(
      committees = tune(),
      neighbors = tune(),
      max_rules = tune()
    ) %>%
      set_engine("Cubist"),
    kfolds = 10,
    grid = 10,
    control = control_grid(
      verbose   = TRUE,
      allow_par = TRUE
    )
  )

# Evaluating
calibrate_evaluate_plot(
  ensemble_fit_cubist_tscv_sel1,
  ensemble_fit_cubist_tscv_sel2,
  ensemble_fit_cubist_vfold_sel1,
  ensemble_fit_cubist_vfold_sel2,
  updated_desc = c(
    "ENSEMBLE (CUBIST STACK TSCV) - TOP", "ENSEMBLE (CUBIST STACK TSCV) - DOWN",
    "ENSEMBLE (CUBIST STACK VFCV) - TOP", "ENSEMBLE (CUBIST STACK VFCV) - DOWN"
  )
)


# * NNET Stack ------------------------------------------------------------

# NNET with TSCV
set.seed(123)
ensemble_fit_nnet_tscv_sel1 <- submodels_resamples_tscv_sel1_tbl %>%
  ensemble_model_spec(
    model_spec = mlp(
      hidden_units = tune(),
      penalty = tune(),
      epochs = tune()
    ) %>%
      set_engine("nnet"),
    kfolds = 10,
    grid = 10,
    control = control_grid(
      verbose   = TRUE,
      allow_par = TRUE
    )
  )

set.seed(123)
ensemble_fit_nnet_tscv_sel2 <- submodels_resamples_tscv_sel2_tbl %>%
  ensemble_model_spec(
    model_spec = mlp(
      hidden_units = tune(),
      penalty = tune(),
      epochs = tune()
    ) %>%
      set_engine("nnet"),
    kfolds = 10,
    grid = 10,
    control = control_grid(
      verbose   = TRUE,
      allow_par = TRUE
    )
  )

# NNET with VFCV
set.seed(123)
ensemble_fit_nnet_vfold_sel1 <- submodels_resamples_vfold_sel1_tbl %>%
  ensemble_model_spec(
    model_spec = mlp(
      hidden_units = tune(),
      penalty = tune(),
      epochs = tune()
    ) %>%
      set_engine("nnet"),
    kfolds = 10,
    grid = 10,
    control = control_grid(
      verbose   = TRUE,
      allow_par = TRUE
    )
  )

set.seed(123)
ensemble_fit_nnet_vfold_sel2 <- submodels_resamples_vfold_sel2_tbl %>%
  ensemble_model_spec(
    model_spec = mlp(
      hidden_units = tune(),
      penalty = tune(),
      epochs = tune()
    ) %>%
      set_engine("nnet"),
    kfolds = 10,
    grid = 10,
    control = control_grid(
      verbose   = TRUE,
      allow_par = TRUE
    )
  )

# Evaluating
calibrate_evaluate_plot(
  ensemble_fit_nnet_tscv_sel1,
  ensemble_fit_nnet_tscv_sel2,
  ensemble_fit_nnet_vfold_sel1,
  ensemble_fit_nnet_vfold_sel2,
  updated_desc = c(
    "ENSEMBLE (NNET STACK TSCV) - TOP", "ENSEMBLE (NNET STACK TSCV) - DOWN",
    "ENSEMBLE (NNET STACK VFCV) - TOP", "ENSEMBLE (NNET STACK VFCV) - DOWN"
  )
)


# * Comparing Ensembles ---------------------------------------------------

# TOP Selection
calibration_stack_sel1_tbl <- modeltime_table(
  ensemble_fit_lm_tscv_sel1,
  ensemble_fit_lm_vfold_sel1,
  ensemble_fit_elanet_tscv_sel1,
  ensemble_fit_elanet_vfold_sel1,
  ensemble_fit_svm_tscv_sel1,
  ensemble_fit_svm_vfold_sel1,
  ensemble_fit_rf_tscv_sel1,
  ensemble_fit_rf_vfold_sel1,
  ensemble_fit_xgb_tscv_sel1,
  ensemble_fit_xgb_vfold_sel1,
  ensemble_fit_cubist_tscv_sel1,
  ensemble_fit_cubist_vfold_sel1,
  ensemble_fit_nnet_tscv_sel1,
  ensemble_fit_nnet_vfold_sel1
) %>%
  update_model_description(.model_id = 1, .new_model_desc = "ENSEMBLE (LM STACK TSCV)") %>%
  update_model_description(.model_id = 2, .new_model_desc = "ENSEMBLE (LM STACK VFCV)") %>%
  update_model_description(.model_id = 3, .new_model_desc = "ENSEMBLE (ELANET STACK TSCV)") %>%
  update_model_description(.model_id = 4, .new_model_desc = "ENSEMBLE (ELANET STACK VFCV)") %>%
  update_model_description(.model_id = 5, .new_model_desc = "ENSEMBLE (SVM STACK TSCV)") %>%
  update_model_description(.model_id = 6, .new_model_desc = "ENSEMBLE (SVM STACK VFCV)") %>%
  update_model_description(.model_id = 7, .new_model_desc = "ENSEMBLE (RF STACK TSCV)") %>%
  update_model_description(.model_id = 8, .new_model_desc = "ENSEMBLE (RF STACK VFCV)") %>%
  update_model_description(.model_id = 9, .new_model_desc = "ENSEMBLE (XGB STACK TSCV)") %>%
  update_model_description(.model_id = 10, .new_model_desc = "ENSEMBLE (XGB STACK VFCV)") %>%
  update_model_description(.model_id = 11, .new_model_desc = "ENSEMBLE (CUBIST STACK TSCV)") %>%
  update_model_description(.model_id = 12, .new_model_desc = "ENSEMBLE (CUBIST STACK VFCV)") %>%
  update_model_description(.model_id = 13, .new_model_desc = "ENSEMBLE (NNET STACK TSCV)") %>%
  update_model_description(.model_id = 14, .new_model_desc = "ENSEMBLE (NNET STACK VFCV)") %>%
  modeltime_calibrate(testing(splits))

# Best Model
calibration_tbl %>% modeltime_accuracy() %>% arrange(rmse) %>% dplyr::slice(1)
# Selected Models
submodels_sel1_tbl %>% modeltime_accuracy() %>% arrange(rmse)
# Stack Models
calibration_stack_sel1_tbl %>% modeltime_accuracy() %>% arrange(rmse)

calibration_stack_sel1_tbl %>%
  modeltime_forecast(new_data = testing(splits), actual_data = data_prep_tbl) %>%
  plot_modeltime_forecast()


# DOWN Selection
calibration_stack_sel2_tbl <- modeltime_table(
  ensemble_fit_lm_tscv_sel2,
  ensemble_fit_lm_vfold_sel2,
  ensemble_fit_elanet_tscv_sel2,
  ensemble_fit_elanet_vfold_sel2,
  ensemble_fit_svm_tscv_sel2,
  ensemble_fit_svm_vfold_sel2,
  ensemble_fit_rf_tscv_sel2,
  ensemble_fit_rf_vfold_sel2,
  ensemble_fit_xgb_tscv_sel2,
  ensemble_fit_xgb_vfold_sel2,
  ensemble_fit_cubist_tscv_sel2,
  ensemble_fit_cubist_vfold_sel2,
  ensemble_fit_nnet_tscv_sel2,
  ensemble_fit_nnet_vfold_sel2
) %>%
  update_model_description(.model_id = 1, .new_model_desc = "ENSEMBLE (LM STACK TSCV)") %>%
  update_model_description(.model_id = 2, .new_model_desc = "ENSEMBLE (LM STACK VFCV)") %>%
  update_model_description(.model_id = 3, .new_model_desc = "ENSEMBLE (ELANET STACK TSCV)") %>%
  update_model_description(.model_id = 4, .new_model_desc = "ENSEMBLE (ELANET STACK VFCV)") %>%
  update_model_description(.model_id = 5, .new_model_desc = "ENSEMBLE (SVM STACK TSCV)") %>%
  update_model_description(.model_id = 6, .new_model_desc = "ENSEMBLE (SVM STACK VFCV)") %>%
  update_model_description(.model_id = 7, .new_model_desc = "ENSEMBLE (RF STACK TSCV)") %>%
  update_model_description(.model_id = 8, .new_model_desc = "ENSEMBLE (RF STACK VFCV)") %>%
  update_model_description(.model_id = 9, .new_model_desc = "ENSEMBLE (XGB STACK TSCV)") %>%
  update_model_description(.model_id = 10, .new_model_desc = "ENSEMBLE (XGB STACK VFCV)") %>%
  update_model_description(.model_id = 11, .new_model_desc = "ENSEMBLE (CUBIST STACK TSCV)") %>%
  update_model_description(.model_id = 12, .new_model_desc = "ENSEMBLE (CUBIST STACK VFCV)") %>%
  update_model_description(.model_id = 13, .new_model_desc = "ENSEMBLE (NNET STACK TSCV)") %>%
  update_model_description(.model_id = 14, .new_model_desc = "ENSEMBLE (NNET STACK VFCV)") %>%
  modeltime_calibrate(testing(splits))

# Best Model
calibration_tbl %>% modeltime_accuracy() %>% arrange(rmse) %>% dplyr::slice_tail(n = 1)
# Selected Models
submodels_sel2_tbl %>% modeltime_accuracy() %>% arrange(rmse)
# Stack Models
calibration_stack_sel2_tbl %>% modeltime_accuracy() %>% arrange(rmse)

calibration_stack_sel2_tbl %>%
  modeltime_forecast(new_data = testing(splits), actual_data = data_prep_tbl) %>%
  plot_modeltime_forecast()



# MULTI-LEVEL STACKING ENSEMBLES ------------------------------------------

# TOP
model_stack_lvl3_sel1_tbl <- modeltime_table(
  ensemble_fit_nnet_vfold_sel1,
  ensemble_fit_rf_vfold_sel1,
  ensemble_fit_svm_vfold_sel1
) %>%
  ensemble_weighted(loadings = c(5, 3, 1)) %>%
  modeltime_table()

model_stack_lvl3_sel1_tbl  %>%
  modeltime_accuracy(testing(splits))


# DOWN
model_stack_lvl3_sel2_tbl <- modeltime_table(
  ensemble_fit_lm_tscv_sel2,
  ensemble_fit_elanet_tscv_sel2,
  ensemble_fit_nnet_tscv_sel2
) %>%
  ensemble_weighted(loadings = c(5, 3, 1)) %>%
  modeltime_table()

model_stack_lvl3_sel2_tbl  %>%
  modeltime_accuracy(testing(splits))



# Refitting & Forecasting -------------------------------------------------

calibration_ensemble_tbl <- model_stack_lvl3_sel1_tbl %>%
  modeltime_calibrate(testing(splits))


# Updating submodels only
refit_ensemble_submodel_tbl <- calibration_ensemble_tbl %>%
  modeltime_refit(data = data_prep_tbl)

refit_ensemble_submodel_tbl %>%
  modeltime_forecast(
    new_data = artifacts_list$data$forecast_tbl,
    actual_data = data_prep_tbl
  ) %>%
  plot_modeltime_forecast()


# Updating submodels and superlerner (takes time)
set.seed(123)
refit_ensemble_superlearner_tbl <- calibration_ensemble_tbl %>%
  modeltime_refit(
    data = data_prep_tbl,
    resamples = data_prep_tbl %>% drop_na() %>% vfold_cv(v = 10)
  )

refit_ensemble_superlearner_tbl %>%
  modeltime_forecast(
    new_data = artifacts_list$data$forecast_tbl,
    actual_data = data_prep_tbl
  ) %>%
  plot_modeltime_forecast()


# Parallelization (switch off)
plan(sequential)


# * Save Artifacts --------------------------------------------------------

model_stack_lvl3_sel1_tbl %>%
  write_rds("artifacts/model_stack_lvl3_tbl.rds")

model_stack_lvl3_sel1_tbl$.model[[1]]$model_tbl$.model[[2]]$model_tbl

