# Time Series Forecasting: Machine Learning and Deep Learning with R & Python ----

# Lecture 7: Deep Learning Algorithms -------------------------------------
# Marco Zanotti

# Goals:
# - GluonTS / Torch
# - Deep AR
# - NBEATS
# - GP Forecaster
# - Deep State

# https://ts.gluon.ai/index.html
# https://business-science.github.io/modeltime.gluonts/
# https://github.com/business-science/modeltime.gluonts
# https://github.com/business-science/modeltime.gluonts/issues/12
# https://pytorch.org/



# Python / R Setup --------------------------------------------------------

# the R Package reticulate allows to use Python functions within R

library(tidyverse)
library(reticulate)

py_config() # Python env in use
conda_version() # installed version of conda
conda_list() # available environments
virtualenv_list()  # available environments
conda_binary() # conda path


# * Installs --------------------------------------------------------------

# Conda
install_miniconda() # recommended
conda_install()
# Windows - Need Visual Studio C++ for Python
# https://github.com/business-science/modeltime.gluonts/issues/4


# * Modeltime GluonTS -----------------------------------------------------

# Installation Guide
# https://business-science.github.io/modeltime.gluonts/articles/managing-envs.html

# step 1: install modeltime.gluonts from github
remotes::install_github("business-science/modeltime.gluonts")
# step 2: load library
library(modeltime.gluonts)
# step 3: install the Python env (only the first time!)
install_gluonts(fresh_install = TRUE, include_pytorch = TRUE)
# step 4: check the r-gluonts env exists
conda_list()
# step 4: check the r-gluonts env
system2(conda_binary(), args = "list -n r-gluonts")
reticulate::py_module_available("gluonts")
# step 5: restart R session (close RStudio!)
# step 6: set GLUONTS_PYTHON env variable
Sys.getenv("GLUONTS_PYTHON")
env_path <- conda_list() %>%
  filter(name == "r-gluonts") %>%
  pull("python")
Sys.setenv(GLUONTS_PYTHON = env_path)
Sys.getenv("GLUONTS_PYTHON")
# use_condaenv("r-gluonts", required = TRUE)
# step 7: reload
library(modeltime.gluonts)


# Python Env (Custom)
?py_install()

py_install(
  packages = c(
    "mxnet==1.6",
    "gluonts==0.6.3",
    "numpy==1.16.6",
    "pandas==1.0.5",
    "scikit-learn==0.23.2",
    "matplotlib==3.3.2",
    "seaborn==0.11.0",
    "pathlib==1.0.1"
  ),
  envname = "my_gluonts_env",
  method = "conda",
  python_version = "3.8",
  pip = TRUE
)
conda_list()


# Test GluonTS
library(timetk)
library(tidymodels)
df <- tibble(id = "X", date = tk_make_timeseries("2016", "2020", by = "month"), y = 1:60)
splits <- df %>% time_series_split(assess = 12, cumulative = TRUE)

model_fit_deepar <- deep_ar(
  id = "id",
  freq = "M",
  prediction_length = 12,
  lookback_length = 36
) %>%
  set_engine("gluonts_deepar") %>%
  fit(y ~ ., data = training(splits))

modeltime_table(model_fit_deepar) %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_forecast(new_data = testing(splits), actual_data = df) %>%
  plot_modeltime_forecast()



# Packages ----------------------------------------------------------------

# reticulate::conda_list()
# env_path <- reticulate::conda_list()[reticulate::conda_list()$name == "r-gluonts", "python"]
# Sys.setenv(GLUONTS_PYTHON = env_path)
# Sys.getenv("GLUONTS_PYTHON")
library(modeltime.gluonts)
reticulate::py_config()

setwd("~/Desktop/RProjects/tsforecasting-course") # sorry for this path
source("R/utils.R")
source("R/packages.R")



# Data & Artifacts --------------------------------------------------------

artifacts_list <- read_rds("artifacts/feature_engineering_artifacts_list.rds")
data_prep_tbl <- artifacts_list$data$data_prep_tbl
forecast_tbl <- artifacts_list$data$forecast_tbl

# Add ID column required by GluonTS Algos
data_prep_tbl <- data_prep_tbl %>% mutate(id = "subscribers", .before = everything())
forecast_tbl <- forecast_tbl %>% mutate(id = "subscribers", .before = everything())


# * Train / Test Sets -----------------------------------------------------

splits <- time_series_split(data_prep_tbl, assess = "8 weeks", cumulative = TRUE)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(optin_time, optins_trans)


# * Recipes ---------------------------------------------------------------

rcp_spec_gluon <- recipe(optins_trans ~ optin_time + id, data = training(splits))
rcp_spec_gluon %>% prep() %>% juice()
rcp_spec_gluon %>% prep() %>% summary()



# GLUONTS DEEP AR ---------------------------------------------------------

?deep_ar()


# * Engines ---------------------------------------------------------------

# GluonTS DeepAR default
model_spec_deepar_0 <- deep_ar(
  id = "id",
  freq = "D",
  prediction_length = 56, # 8 weeks
  cell_type = "lstm",
  num_layers = "2",
  num_cells = "40",
  epochs = 5, # GluonTS default to 100 usually
  batch_size = 32,
  num_batches_per_epoch = 50,
  learn_rate = 10 ^ (-3),
  learn_rate_decay_factor = 0.5,
  penalty = 10 ^ (-8)
) %>%
  set_engine("gluonts_deepar")

# GluonTS DeepAR - LSTM
model_spec_deepar_1 <- deep_ar(
  id = "id",
  freq = "D",
  prediction_length = 56,
  cell_type = "lstm",
  epochs = 10,
  num_batches_per_epoch = 25
) %>%
  set_engine("gluonts_deepar")

# GluonTS DeepAR - GRU
model_spec_deepar_2 <- deep_ar(
  id = "id",
  freq = "D",
  prediction_length = 56,
  cell_type = "gru",
  epochs = 10,
  num_batches_per_epoch = 25
) %>%
  set_engine("gluonts_deepar")


# * Workflows -------------------------------------------------------------

# GluonTS DeepAR default
wrkfl_fit_deepar_0 <- workflow() %>%
  add_model(model_spec_deepar_0) %>%
  add_recipe(rcp_spec_gluon) %>%
  fit(data = training(splits))

# GluonTS DeepAR - LSTM
wrkfl_fit_deepar_1 <- workflow() %>%
  add_model(model_spec_deepar_1) %>%
  add_recipe(rcp_spec_gluon) %>%
  fit(data = training(splits))

# GluonTS DeepAR - GRU
wrkfl_fit_deepar_2 <- workflow() %>%
  add_model(model_spec_deepar_2) %>%
  add_recipe(rcp_spec_gluon) %>%
  fit(data = training(splits))


# * Calibration, Evaluation & Plotting ------------------------------------

calibrate_evaluate_plot(
  wrkfl_fit_deepar_0,
  wrkfl_fit_deepar_1,
  wrkfl_fit_deepar_2,
  updated_desc = c("DeepAR", "DeepAR - LSTM", "DeepAR - GRU")
)



# GLUONTS NBEATS ----------------------------------------------------------

?nbeats()


# * Engines ---------------------------------------------------------------

# GluonTS NBeats default
model_spec_nbeats_0 <- nbeats(
  id = "id",
  freq = "D",
  prediction_length = 56, # 8 weeks
  loss_function = "sMAPE", # GluonTS default to "MAPE"
  num_stacks = 30,
  num_blocks = 1,
  epochs = 5, # GluonTS default to 100 usually
  batch_size = 32,
  num_batches_per_epoch = 50,
  learn_rate = 10 ^ (-3),
  learn_rate_decay_factor = 0.5,
  penalty = 10 ^ (-8)
) %>%
  set_engine("gluonts_nbeats")

# GluonTS NBeats +
model_spec_nbeats_1 <- nbeats(
  id = "id",
  freq = "D",
  prediction_length = 56,
  epochs = 10,
  num_batches_per_epoch = 25
) %>%
  set_engine("gluonts_nbeats")

# GluonTS NBeats Ensemble
model_spec_nbeats_2 <- nbeats(
  id = "id",
  freq = "D",
  prediction_length = 56,
  epochs = 5,
  num_batches_per_epoch = 50,
  lookback_length = c(56, 56 * 2),
  bagging_size = 1
) %>%
  set_engine("gluonts_nbeats_ensemble")


# * Workflows -------------------------------------------------------------

# GluonTS NBeats default
wrkfl_fit_nbeats_0 <- workflow() %>%
  add_model(model_spec_nbeats_0) %>%
  add_recipe(rcp_spec_gluon) %>%
  fit(data = training(splits))

# GluonTS NBeats +
wrkfl_fit_nbeats_1 <- workflow() %>%
  add_model(model_spec_nbeats_1) %>%
  add_recipe(rcp_spec_gluon) %>%
  fit(data = training(splits))

# GluonTS NBeats Ensemble
wrkfl_fit_nbeats_2 <- workflow() %>%
  add_model(model_spec_nbeats_2) %>%
  add_recipe(rcp_spec_gluon) %>%
  fit(data = training(splits))


# * Calibration, Evaluation & Plotting ------------------------------------

calibrate_evaluate_plot(
  wrkfl_fit_nbeats_0,
  wrkfl_fit_nbeats_1,
  wrkfl_fit_nbeats_2,
  updated_desc = c("NBEATS", "NBEATS +", "NBEATS ENSEMBLE")
)



# GLUONTS GP FORECASTER ---------------------------------------------------

?gp_forecaster()


# * Engines ---------------------------------------------------------------

# GluonTS GPForecaster default
model_spec_gpfor_0 <- gp_forecaster(
  id = "id",
  freq = "D",
  prediction_length = 56,
  epochs = 5, # GluonTS default to 100 usually
  batch_size = 32,
  num_batches_per_epoch = 50,
  learn_rate = 10 ^ (-3),
  learn_rate_decay_factor = 0.5,
  penalty = 10 ^ (-8)
) %>%
  set_engine("gluonts_gp_forecaster")

# GluonTS GPForecaster +
model_spec_gpfor_1 <- gp_forecaster(
  id = "id",
  freq = "D",
  prediction_length = 56,
  epochs = 10,
  num_batches_per_epoch = 25
) %>%
  set_engine("gluonts_gp_forecaster")


# * Workflows -------------------------------------------------------------

# GluonTS GPForecaster default
wrkfl_fit_gpfor_0 <- workflow() %>%
  add_model(model_spec_gpfor_0) %>%
  add_recipe(rcp_spec_gluon) %>%
  fit(data = training(splits))

# GluonTS GPForecaster +
wrkfl_fit_gpfor_1 <- workflow() %>%
  add_model(model_spec_gpfor_1) %>%
  add_recipe(rcp_spec_gluon) %>%
  fit(data = training(splits))


# * Calibration, Evaluation & Plotting ------------------------------------

calibrate_evaluate_plot(
  wrkfl_fit_gpfor_0,
  wrkfl_fit_gpfor_1,
  updated_desc = c("GP FORECASTER", "GP FORECASTER +")
)



# GLUONTS DEEP STATE ------------------------------------------------------

?deep_state()


# * Engines ---------------------------------------------------------------

# GluonTS DeepState default
model_spec_deepst_0 <- deep_state(
  id = "id",
  freq = "D",
  prediction_length = 56,
  add_trend = FALSE,
  epochs = 5, # GluonTS default to 100 usually
  batch_size = 32,
  num_batches_per_epoch = 50,
  learn_rate = 10 ^ (-3),
  learn_rate_decay_factor = 0.5,
  penalty = 10 ^ (-8)
) %>%
  set_engine("gluonts_deepstate")

# GluonTS DeepState +
model_spec_deepst_1 <- deep_state(
  id = "id",
  freq = "D",
  prediction_length = 56,
  add_trend = TRUE,
  epochs = 10,
  num_batches_per_epoch = 25
) %>%
  set_engine("gluonts_deepstate")


# * Workflows -------------------------------------------------------------

# GluonTS DeepState default
wrkfl_fit_deepst_0 <- workflow() %>%
  add_model(model_spec_deepst_0) %>%
  add_recipe(rcp_spec_gluon) %>%
  fit(data = training(splits))

# GluonTS DeepState +
wrkfl_fit_deepst_1 <- workflow() %>%
  add_model(model_spec_deepst_1) %>%
  add_recipe(rcp_spec_gluon) %>%
  fit(data = training(splits))


# * Calibration, Evaluation & Plotting ------------------------------------

calibrate_evaluate_plot(
  wrkfl_fit_deepst_0,
  wrkfl_fit_deepst_1,
  updated_desc = c("DEEP STATE", "DEEP STATE +")
)



# TORCH DEEP AR -----------------------------------------------------------

?deep_ar()

# Torch DeepAR
#   Requires:
#   - modeltime.gluonts >= 0.3.0 (R)
#   - gluonts >= 0.8.0 (python)
#   - torch and pytorch-lightning (python)


# * Engines ---------------------------------------------------------------

# Torch DeepAR default
model_spec_deepar_torch_0 <- deep_ar(
  id = "id",
  freq = "D",
  prediction_length = 56, # 8 weeks
  epochs = 5, # Torch default to max_epochs
  batch_size = 32,
  num_cells = 40
) %>%
  set_engine("torch")

# Torch DeepAR +
model_spec_deepar_torch_1 <- deep_ar(
  id = "id",
  freq = "D",
  prediction_length = 56,
  epochs = 10
) %>%
  set_engine("torch")

# Torch DeepAR ++
model_spec_deepar_torch_2 <- deep_ar(
  id = "id",
  freq = "D",
  prediction_length = 56,
  lookback_length = 56 * 4, # takes more time to train
  epochs = 10
) %>%
  set_engine("torch")


# * Workflows -------------------------------------------------------------

# Torch DeepAR default
wrkfl_fit_deepar_torch_0 <- workflow() %>%
  add_model(model_spec_deepar_torch_0) %>%
  add_recipe(rcp_spec_gluon) %>%
  fit(data = training(splits))

# Torch DeepAR +
wrkfl_fit_deepar_torch_1 <- workflow() %>%
  add_model(model_spec_deepar_torch_1) %>%
  add_recipe(rcp_spec_gluon) %>%
  fit(data = training(splits))

# Torch DeepAR ++
wrkfl_fit_deepar_torch_2 <- workflow() %>%
  add_model(model_spec_deepar_torch_2) %>%
  add_recipe(rcp_spec_gluon) %>%
  fit(data = training(splits))


# * Calibration, Evaluation & Plotting ------------------------------------

calibrate_evaluate_plot(
  wrkfl_fit_deepar_torch_0,
  wrkfl_fit_deepar_torch_1,
  wrkfl_fit_deepar_torch_2,
  updated_desc = c("Toch DeepAR", "Toch DeepAR +", "Torch DeepAR ++")
)



# DL Models' Performance --------------------------------------------------

# * Comparison ------------------------------------------------------------

# * Calibration (full)
calibration_tbl <- modeltime_table(
  # GluonTS DEEP AR
  wrkfl_fit_deepar_0,
  wrkfl_fit_deepar_1,
  wrkfl_fit_deepar_2,
  # GluonTS NBEATS
  wrkfl_fit_nbeats_0,
  wrkfl_fit_nbeats_1,
  wrkfl_fit_nbeats_2,
  # GluonTS GP FORECASTER
  wrkfl_fit_gpfor_0,
  wrkfl_fit_gpfor_1,
  # GluonTS DEEP STATE
  wrkfl_fit_deepst_0,
  wrkfl_fit_deepst_1,
  # Torch DEEP AR
  wrkfl_fit_deepar_torch_0,
  wrkfl_fit_deepar_torch_1,
  wrkfl_fit_deepar_torch_2
) %>%
  update_modeltime_description(.model_id = 1, .new_model_desc = "Glu DAR") %>%
  update_modeltime_description(.model_id = 2, .new_model_desc = "Glu DAR - LSTM") %>%
  update_modeltime_description(.model_id = 3, .new_model_desc = "Glu DAR - GRU") %>%
  update_modeltime_description(.model_id = 4, .new_model_desc = "Glu NBE") %>%
  update_modeltime_description(.model_id = 5, .new_model_desc = "Glu NBE +") %>%
  update_modeltime_description(.model_id = 6, .new_model_desc = "Glu NBE ++") %>%
  update_modeltime_description(.model_id = 7, .new_model_desc = "Glu GPF") %>%
  update_modeltime_description(.model_id = 8, .new_model_desc = "Glu GPF +") %>%
  update_modeltime_description(.model_id = 9, .new_model_desc = "Glu DST") %>%
  update_modeltime_description(.model_id = 10, .new_model_desc = "Glu DST +") %>%
  update_modeltime_description(.model_id = 11, .new_model_desc = "Tor DAR") %>%
  update_modeltime_description(.model_id = 12, .new_model_desc = "Tor DAR +") %>%
  update_modeltime_description(.model_id = 13, .new_model_desc = "Tor DAR ++") %>%
  modeltime_calibrate(testing(splits))

# * Evaluation
calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = TRUE, bordered = TRUE, resizable = TRUE)

calibration_tbl %>%
  modeltime_forecast(new_data = testing(splits), actual_data = data_prep_tbl) %>%
  plot_modeltime_forecast()

# * Refitting & Forecasting

# Best by RMSE
model_dl_best <- calibration_tbl %>%
  select_best_id(n = 1)

refit_tbl <- calibration_tbl %>%
  filter(.model_id %in% model_dl_best) %>%
  modeltime_refit(data = data_prep_tbl)

refit_tbl %>%
  modeltime_forecast(new_data = forecast_tbl, actual_data = data_prep_tbl) %>%
  plot_modeltime_forecast(.conf_interval_fill = "lightblue")


# * Save Artifacts --------------------------------------------------------

wrkfl_fit_deepar_0 %>%
  save_gluonts_model(path = "artifacts/gluonts_deepar")

wrkfl_fit_deepar_0 <- load_gluonts_model("artifacts/gluonts_deepar")


# * Deep Learning Ensembles -----------------------------------------------

?ensemble_average()

dl_ensemble_tbl <- modeltime_table(
  wrkfl_fit_nbeats_0,
  wrkfl_fit_deepst_1,
  wrkfl_fit_deepar_torch_2
)

# Fitting
ensemble_fit_mean <- dl_ensemble_tbl %>%
  ensemble_average(type = "mean")
ensemble_fit_median <- dl_ensemble_tbl %>%
  ensemble_average(type = "median")

# Evaluating
calibrate_evaluate_plot(
  ensemble_fit_mean,
  ensemble_fit_median,
  updated_desc = c("ENSEMBLE (MEAN)", "ENSEMBLE (MEDIAN)")
)


# DEEP LEARNING
# - Pros:
#   - Create very powerful models by combining Machine Learning & Deep Learning
#   - Deep Learning is great for global modeling time series
# - Cons:
#   - Lower to train with respect to TS / ML algos
#   - More difficult to train
#   - Does not factor in external regressors
#     - Solution 1: Run DL without. Run ML on the Residuals.
#     - Solution 2: Create an Ensemble with ML & DL

# GLUONTS
# - Pros:
#   - Wide range of algorithms
#   - One round of training and probabilities are incorporated
# - Cons:
#   - Complex to work with ListDataset Structure

# MODELTIME GLUONTS
# - Pros:
#   - Simplifies creating ListDataset() objects
#   - Can compare w/ other forecast algorithms
#   - Good for scaling up predictions
# - Cons:
#   - Requires 2 rounds of training to get confidence intervals
#   - Not all GluonTS Algorithms incorporated

