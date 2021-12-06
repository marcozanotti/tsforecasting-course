# Time Series Forecasting: Machine Learning and Deep Learning with R & Python ----

# Lecture 11: Recursive Time Series Algorithms ----------------------------
# 2021/2022
# Marco Zanotti

# Goals:
# - Recursivity
# - Panel Recursivity



# Packages ----------------------------------------------------------------

source("R/utils.R")
source("R/packages.R")



# RECURSIVITY - SINGLE TIME SERIES ----------------------------------------

?recursive()

# Data & Artifacts --------------------------------------------------------

subscribers_tbl <- read_rds("data/subscribers.rds")
events_tbl <- read_rds("data/events.rds")


# * Data Preparation ------------------------------------------------------

# subscribers data
subscribers_daily_tbl <- subscribers_tbl %>%
  summarise_by_time(optin_time, .by = "day", optins = n()) %>%
  pad_by_time(.pad_value = 0)

subscribers_daily_tbl %>%
  plot_time_series(optin_time, log1p(optins), .smooth = FALSE)

subscribers_prep_tbl <- subscribers_daily_tbl %>%
  mutate(optins_trans = log_interval_vec(optins, limit_lower = 0, offset = 1)) %>%
  mutate(optins_trans = standardize_vec(optins_trans)) %>%
  filter_by_time(.start_date = "2018-07-03") %>%
  mutate(optins_trans_cleaned = ts_clean_vec(optins_trans, period = 7)) %>%
  mutate(
    optins_trans = ifelse(
      optin_time %>% between_time("2018-11-18", "2018-11-20"),
      optins_trans_cleaned,
      optins_trans
    )
  ) %>%
  select(-optins, -optins_trans_cleaned)

# events data
events_daily_tbl <- events_tbl %>%
  mutate(event_date = ymd_hms(event_date)) %>%
  summarise_by_time(event_date, .by = "day", event = n())


# * Feature Engineering ---------------------------------------------------

# - Extend to Future Window
# - Add any lags to full dataset
# - Add any external regressors to full dataset

horizon <- 7 * 8
lags <- c(1, 2, 7, 14, 30)

data_prep_full_tbl <- subscribers_prep_tbl %>%
  future_frame(.data = ., optin_time, .length_out = horizon, .bind_data = TRUE) %>%
  lag_transf() %>%
  tk_augment_lags(optins_trans, .lags = c(horizon, 90)) %>%
  left_join(events_daily_tbl, by = c("optin_time" = "event_date")) %>%
  mutate(event = ifelse(is.na(event), 0, event))

data_prep_full_tbl %>%
  pivot_longer(-optin_time) %>%
  plot_time_series(optin_time, value, name, .smooth = FALSE)
data_prep_full_tbl %>% slice_tail(n = horizon + 1)


# * Modelling & Forecast Data ---------------------------------------------

data_prep_tbl <- data_prep_full_tbl %>%
  slice_head(n = nrow(.) - horizon) %>%
  drop_na()
forecast_tbl <- data_prep_full_tbl %>%
  slice_tail(n = horizon)


# * Train / Test Sets -----------------------------------------------------

splits <- time_series_split(data_prep_tbl, assess = horizon, cumulative = TRUE)
splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(optin_time, optins_trans)


# * Recipes ---------------------------------------------------------------

# Recipe with calendar features, short-term and long-term dynamics
rcp_spec <- recipe(optins_trans ~ ., data = training(splits)) %>%
  step_timeseries_signature(optin_time) %>%
  step_rm(matches("(iso)|(xts)|(hour)|(minute)|(second)|(am.pm)")) %>%
  step_normalize(matches("(index.num)|(year)|(yday)")) %>%
  step_dummy(all_nominal(), one_hot = TRUE) %>%
  step_rm(optin_time)
rcp_spec %>% prep() %>% juice() %>% glimpse()



# LINEAR REGRESSION -------------------------------------------------------

# * Engines ---------------------------------------------------------------

model_spec_lm <- linear_reg() %>%
  set_engine("lm")


# * Workflows -------------------------------------------------------------

# LM - Non Recursive
wrkfl_fit_lm <- workflow() %>%
  add_model(model_spec_lm) %>%
  add_recipe(rcp_spec) %>%
  fit(training(splits))

# LM - Recursive
wrkfl_fit_lm_recursive <- workflow() %>%
  add_model(model_spec_lm) %>%
  add_recipe(rcp_spec) %>%
  fit(training(splits)) %>%
  recursive(
    transform = lag_transf,
    train_tail = tail(training(splits), horizon)
  )



# ELASTIC NET -------------------------------------------------------------

# * Engines ---------------------------------------------------------------

model_spec_elanet <- linear_reg(
  mode = "regression",
  penalty = 0.01,
  mixture = 0.99
) %>%
  set_engine("glmnet")


# * Workflows -------------------------------------------------------------

# ELASTIC NET - Recursive
wrkfl_fit_elanet_recursive <- workflow() %>%
  add_model(model_spec_elanet) %>%
  add_recipe(rcp_spec) %>%
  fit(training(splits)) %>%
  recursive(
    transform = lag_transf,
    train_tail = tail(training(splits), horizon)
  )



# SVM ---------------------------------------------------------------------

# * Engines ---------------------------------------------------------------

model_spec_svm <- svm_rbf(
  mode = "regression",
  cost = 1,
  rbf_sigma = 0.01,
  margin = 0.1
) %>%
  set_engine("kernlab")


# * Workflows -------------------------------------------------------------

# SVM - Recursive
wrkfl_fit_svm_recursive <- workflow() %>%
  add_model(model_spec_svm) %>%
  add_recipe(rcp_spec) %>%
  fit(training(splits)) %>%
  recursive(
    transform = lag_transf,
    train_tail = tail(training(splits), horizon)
  )



# BOOSTING ----------------------------------------------------------------

# * Engines ---------------------------------------------------------------

model_spec_xgb <- boost_tree(
  mode = "regression",
  mtry = 25,
  trees = 1000,
  min_n = 2,
  tree_depth = 12,
  learn_rate = 0.3,
  loss_reduction = 0
) %>%
  set_engine("xgboost")


# * Workflows -------------------------------------------------------------

# XGBOOST - Recursive
wrkfl_fit_xgb_recursive <- workflow() %>%
  add_model(model_spec_xgb) %>%
  add_recipe(rcp_spec) %>%
  fit(training(splits)) %>%
  recursive(
    transform = lag_transf,
    train_tail = tail(training(splits), horizon)
  )



# ENSEMBLES ---------------------------------------------------------------

# * Workflows -------------------------------------------------------------

# ELANET - NON RECURSIVE!!!
wrkfl_fit_elanet <- workflow() %>%
  add_model(model_spec_elanet) %>%
  add_recipe(rcp_spec) %>%
  fit(training(splits))

# SVM - NON RECURSIVE!!!
wrkfl_fit_svm <- workflow() %>%
  add_model(model_spec_svm) %>%
  add_recipe(rcp_spec) %>%
  fit(training(splits))

# WEIGHTED ENSEMBLE - RECURSIVE
ensemble_fit_mean_recursive <- modeltime_table(
  wrkfl_fit_elanet,
  wrkfl_fit_svm
) %>%
  ensemble_average(type = "mean") %>%
  recursive(
    transform = lag_transf,
    train_tail = tail(training(splits), horizon)
  )



# Recursive Models' Performance -------------------------------------------

# * Evaluation
calibration_tbl <- modeltime_table(
  wrkfl_fit_lm,
  wrkfl_fit_lm_recursive,
  wrkfl_fit_elanet_recursive,
  wrkfl_fit_svm_recursive,
  wrkfl_fit_xgb_recursive,
  ensemble_fit_mean_recursive
) %>%
  modeltime_calibrate(testing(splits))

calibration_tbl %>%
  modeltime_accuracy()

calibration_tbl %>%
  modeltime_forecast(new_data = testing(splits), actual_data = data_prep_tbl) %>%
  plot_modeltime_forecast(.conf_interval_fill = "lightblue")

# * Refitting & Forecasting
refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = data_prep_tbl)

refit_tbl %>%
  modeltime_forecast(new_data = forecast_tbl, actual_data = data_prep_tbl) %>%
  plot_modeltime_forecast(.conf_interval_fill = "lightblue")



# RECURSIVITY - PANEL TIME SERIES -----------------------------------------

?recursive()
?panel_tail()

# Data & Artifacts --------------------------------------------------------

subscribers_tbl <- read_rds("data/subscribers.rds")
events_tbl <- read_rds("data/events.rds")


# * Data Preparation ------------------------------------------------------

subscribers_tbl %>% count(member_rating)

# subscribers data by group
subscribers_daily_tbl <- subscribers_tbl %>%
  rename(id = member_rating) %>%
  mutate(id = ifelse(id == 2, id, 1)) %>%
  group_by(id) %>%
  summarise_by_time(optin_time, .by = "day", optins = n()) %>%
  pad_by_time(.pad_value = 0)

subscribers_daily_tbl %>%
  plot_time_series(optin_time, log1p(optins), .smooth = FALSE)

subscribers_prep_tbl <- subscribers_daily_tbl %>%
  mutate(optins_trans = log_interval_vec(optins, limit_lower = 0, offset = 1)) %>%
  mutate(optins_trans = standardize_vec(optins_trans)) %>%
  filter_by_time(.start_date = "2018-07-05") %>%
  mutate(optins_trans_cleaned = ts_clean_vec(optins_trans, period = 7)) %>%
  mutate(
    optins_trans = ifelse(
      optin_time %>% between_time("2018-11-18", "2018-11-20"),
      optins_trans_cleaned,
      optins_trans
    )
  ) %>%
  select(-optins, -optins_trans_cleaned)

subscribers_prep_tbl %>%
  plot_time_series(optin_time, optins_trans, .smooth = FALSE)

# events data
events_daily_tbl <- events_tbl %>%
  mutate(event_date = ymd_hms(event_date)) %>%
  summarise_by_time(event_date, .by = "day", event = n())


# * Feature Engineering ---------------------------------------------------

# - Extend to Future Window
# - Add any lags to full dataset
# - Add any external regressors to full dataset

horizon <- 7 * 8
lags <- c(1, 2, 7, 14, 30)

data_prep_full_tbl <- subscribers_prep_tbl %>%
  future_frame(.data = ., optin_time, .length_out = horizon, .bind_data = TRUE) %>%
  lag_transf_grouped() %>%
  group_by(id) %>%
  tk_augment_lags(optins_trans, .lags = c(horizon, 90)) %>%
  left_join(events_daily_tbl, by = c("optin_time" = "event_date")) %>%
  mutate(event = ifelse(is.na(event), 0, event))

data_prep_full_tbl %>%
  pivot_longer(cols = -c(id, optin_time)) %>%
  plot_time_series(optin_time, value, name, .smooth = FALSE)
data_prep_full_tbl %>% slice_tail(n = horizon + 1)


# * Modelling & Forecast Data ---------------------------------------------

data_prep_tbl <- data_prep_full_tbl %>%
  slice_head(n = nrow(.) - horizon) %>%
  drop_na() %>%
  ungroup()
forecast_tbl <- data_prep_full_tbl %>%
  slice_tail(n = horizon)


# * Train / Test Sets -----------------------------------------------------

splits <- time_series_split(data_prep_tbl, assess = horizon, cumulative = TRUE)
splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(optin_time, optins_trans)


# * Recipes ---------------------------------------------------------------

# Recipe with calendar features, short-term and long-term dynamics
rcp_spec <- recipe(optins_trans ~ ., data = training(splits)) %>%
  step_timeseries_signature(optin_time) %>%
  step_rm(matches("(iso)|(xts)|(hour)|(minute)|(second)|(am.pm)")) %>%
  step_normalize(matches("(index.num)|(year)|(yday)")) %>%
  step_dummy(all_nominal(), one_hot = TRUE) %>%
  step_rm(optin_time)
rcp_spec %>% prep() %>% juice() %>% glimpse()



# KNN ---------------------------------------------------------------------

# * Engines ---------------------------------------------------------------

model_spec_knn <- nearest_neighbor(
  mode = "regression",
  neighbors = 50,
  dist_power = 10,
  weight_func = "optimal"
) %>%
  set_engine("kknn")


# * Workflows -------------------------------------------------------------

# KNN - Recursive
wrkfl_fit_knn_recursive <- workflow() %>%
  add_model(model_spec_knn) %>%
  add_recipe(rcp_spec) %>%
  fit(training(splits)) %>%
  recursive(
    id = "id",
    transform = lag_transf_grouped,
    train_tail = panel_tail(training(splits), id, horizon)
  )



# RANDOM FOREST -----------------------------------------------------------

# * Engines ---------------------------------------------------------------

model_spec_rf <- rand_forest(
  mode = "regression",
  mtry = 25,
  trees = 1000,
  min_n = 25
) %>%
  set_engine("ranger")


# * Workflows -------------------------------------------------------------

# RF - Recursive
wrkfl_fit_rf_recursive <- workflow() %>%
  add_model(model_spec_rf) %>%
  add_recipe(rcp_spec) %>%
  fit(training(splits)) %>%
  recursive(
    id = "id",
    transform = lag_transf_grouped,
    train_tail = panel_tail(training(splits), id, horizon)
  )



# CUBIST ------------------------------------------------------------------

# * Engines ---------------------------------------------------------------

model_spec_cubist <- cubist_rules(
  committees = 50,
  neighbors = 7,
  max_rules = 100
) %>%
  set_engine("Cubist")


# * Workflows -------------------------------------------------------------

# CUBIST - Recursive
wrkfl_fit_cubist_recursive <- workflow() %>%
  add_model(model_spec_cubist) %>%
  add_recipe(rcp_spec) %>%
  fit(training(splits)) %>%
  recursive(
    id = "id",
    transform = lag_transf_grouped,
    train_tail = panel_tail(training(splits), id, horizon)
  )



# NEURAL NETWORK ----------------------------------------------------------

# * Engines ---------------------------------------------------------------

model_spec_nnet <- mlp(
  mode = "regression",
  hidden_units = 10,
  penalty = 1,
  epochs = 100
) %>%
  set_engine("nnet")


# * Workflows -------------------------------------------------------------

# NNET - Recursive
wrkfl_fit_nnet_recursive <- workflow() %>%
  add_model(model_spec_nnet) %>%
  add_recipe(rcp_spec) %>%
  fit(training(splits)) %>%
  recursive(
    id = "id",
    transform = lag_transf_grouped,
    train_tail = panel_tail(training(splits), id, horizon)
  )



# Recursive Models' Performance -------------------------------------------

# * Evaluation
calibration_tbl <- modeltime_table(
  wrkfl_fit_knn_recursive,
  wrkfl_fit_rf_recursive,
  wrkfl_fit_cubist_recursive,
  wrkfl_fit_nnet_recursive
) %>%
  modeltime_calibrate(testing(splits), id = "id")

calibration_tbl %>%
  modeltime_accuracy(acc_by_id = TRUE) %>%
  arrange(id)

calibration_tbl %>%
  modeltime_forecast(new_data = testing(splits), actual_data = data_prep_tbl, keep_data = TRUE) %>%
  group_by(id) %>%
  plot_modeltime_forecast(.conf_interval_fill = "lightblue")

# * Refitting & Forecasting
refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = data_prep_tbl)

refit_tbl %>%
  modeltime_forecast(new_data = forecast_tbl, actual_data = data_prep_tbl, keep_data = TRUE) %>%
  group_by(id) %>%
  plot_modeltime_forecast(.conf_interval_fill = "lightblue")

