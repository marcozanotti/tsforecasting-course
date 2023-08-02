# Time Series Forecasting: Machine Learning and Deep Learning with R & Python ----

# Lecture 13: Neural Prophet Algorithm ------------------------------------
# Marco Zanotti

# Goals:
# - learn how to use the Neural version of Prophet



# Python / R Setup --------------------------------------------------------

# the R Package reticulate allows to use Python functions within R
reticulate::py_config() # Python env in use
reticulate::conda_version() # installed version of conda
reticulate::conda_list() # available environments
reticulate::virtualenv_list()  # available environments
reticulate::conda_binary() # conda path


# * Installs --------------------------------------------------------------

# Conda
reticulate::install_miniconda() # recommended
reticulate::conda_install()
# Windows - Need Visual Studio C++ for Python
# https://github.com/business-science/modeltime.gluonts/issues/4


# * Modeltime NeuralProphet -----------------------------------------------

# Installation Guide
# https://github.com/AlbertoAlmuinha/neuralprophet

# step 1: install modeltime.gluonts from github
remotes::install_github("AlbertoAlmuinha/neuralprophet")
# step 2: load library
library(neuralprophet)
# step 3: install the Python env (only the first time!)
neuralprophet::install_nprophet(fresh_install = TRUE)
# step 4: check the nprophet env exists
reticulate::conda_list()
# step 4: check the nprophet env
system2(reticulate::conda_binary(), args = "list -n nprophet")
reticulate::py_module_available("nprophet")
# step 5: restart R session (close RStudio!)
# step 6: set GLUONTS_PYTHON env variable
env_path <- reticulate::conda_list()[reticulate::conda_list()$name == "nprophet", "python"]
Sys.setenv(NPROPHET_PYTHON = env_path)
Sys.getenv("NPROPHET_PYTHON")
reticulate::use_python(python = Sys.getenv("NPROPHET_PYTHON"), required = TRUE)
# reticulate::use_condaenv("nprophet", required = TRUE)
# step 7: reload
library(neuralprophet)


# Python Env (Custom)
?reticulate::py_install()

reticulate::py_install(
  packages = c(
    "neuralprophet==0.2.7",
    "pillow==8.3.0",
    "pandas==1.2.5",
    "numpy",
    "pytorch==1.6"
  ),
  envname = "my_nprophet_env",
  method = "conda",
  python_version = "3.8",
  pip = TRUE
)
reticulate::conda_list()


# Test Neural Prophet
library(timetk)
library(tidymodels)
library(neuralprophet)
df <- tibble(date = tk_make_timeseries("2016", "2020", by = "month"), y = 1:60)
splits <- df |> time_series_split(assess = 12, cumulative = TRUE)

model_fit_nprophet <- neural_prophet(
  freq = "M",
  growth = "linear",
  trend_reg = 3,
  learn_rate = 0.1,
  changepoint_range = 0.8,
  seasonality_mode = "additive"
) %>%
  set_engine("prophet") %>%
  fit(y ~ ., training(splits))

modeltime_table(model_fit_nprophet) |>
  modeltime_calibrate(testing(splits)) |>
  modeltime_forecast(new_data = testing(splits), actual_data = df, conf_interval = 0.95) |>
  plot_modeltime_forecast()



# Packages ----------------------------------------------------------------

env_path <- reticulate::conda_list()[reticulate::conda_list()$name == "nprophet", "python"]
Sys.setenv(NPROPHET_PYTHON = env_path)
Sys.getenv("NPROPHET_PYTHON")
# reticulate::use_python(python = Sys.getenv("NPROPHET_PYTHON"), required = TRUE)
library(neuralprophet)
reticulate::py_config()

# setwd("~/Desktop/RProjects/tsforecasting-course") # sorry for this path
source("R/utils.R")
source("R/packages.R")



# Data & Artifacts --------------------------------------------------------

artifacts_list <- read_rds("artifacts/feature_engineering_artifacts_list.rds")
data_prep_tbl <- artifacts_list$data$data_prep_tbl
forecast_tbl <- artifacts_list$data$forecast_tbl

# data_prep_tbl <- data_prep_tbl |> mutate(id = "subscribers", .before = everything())
# forecast_tbl <- forecast_tbl |> mutate(id = "subscribers", .before = everything())


# * Train / Test Sets -----------------------------------------------------

splits <- time_series_split(data_prep_tbl, assess = "8 weeks", cumulative = TRUE)

splits |>
  tk_time_series_cv_plan() |>
  plot_time_series_cv_plan(optin_time, optins_trans)


# * Recipes ---------------------------------------------------------------



# PROPHET -----------------------------------------------------------------

# Facebook's Prophet Algorithm

?prophet_reg

# Can handle daily, weekly and yearly seasonality
# Automatic
# Can use smoothing trend
# Accepts external regressors (can be used to include other seasonalities)


# * Engines ---------------------------------------------------------------

# PROPHET with XREGs
model_fit_prophet_xregs <- prophet_reg(
  seasonality_weekly = TRUE,
  seasonality_yearly = TRUE
) |>
  set_engine("prophet") |>
  fit(optins_trans ~ optin_time + event, data = training(splits))


# * Calibration, Evaluation & Plotting ------------------------------------

calibrate_evaluate_plot(model_fit_prophet_xregs)



# NEURAL PROPHET ----------------------------------------------------------

# Facebook's Neural Prophet Algorithm

?neural_prophet

# Can handle daily, weekly and yearly seasonality
# Automatic
# Can use smoothing trend
# Accepts external regressors (can be used to include other seasonalities)


# * Engines ---------------------------------------------------------------

# NEURAL PROPHET
model_fit_nprophet <- neural_prophet(
  freq = "M",
  growth = "linear",
  trend_reg = 3,
  learn_rate = 0.1,
  changepoint_range = 0.8,
  seasonality_mode = "additive"
) %>%
  set_engine("prophet") %>%
  fit(optins_trans ~ optin_time, data = training(splits))

# Auto-NEURAL PROPHET
model_fit_auto_nprophet <- neural_prophet() %>%
  set_engine("prophet") %>%
  fit(optins_trans ~ optin_time, data = training(splits))

# NEURAL PROPHET with XREGs
model_fit_nprophet_xregs <- neural_prophet(
  seasonality_weekly = TRUE,
  seasonality_yearly = TRUE
) |>
  set_engine("prophet") |>
  fit(optins_trans ~ optin_time + event, data = training(splits))


# * Calibration, Evaluation & Plotting ------------------------------------

calibrate_evaluate_plot(
  model_fit_nprophet,
  model_fit_auto_nprophet,
  model_fit_nprophet_xregs
)



# PROPHET Models' Performance --------------------------------------------------

# * Comparison ------------------------------------------------------------

# * Calibration (full)
calibration_tbl <- modeltime_table(
  # PROPHET
  model_fit_prophet_xregs,
  # NEURAL PROPHET
  model_fit_nprophet,
  model_fit_auto_nprophet,
  model_fit_nprophet_xregs
) |>
  update_modeltime_description(.model_id = 3, .new_model_desc = "AUTO NEURAL PROPHET") |>
  update_modeltime_description(.model_id = 4, .new_model_desc = "NEURAL PROPHET W/ REGRESSORS") |>
  modeltime_calibrate(testing(splits))

# * Evaluation
calibration_tbl |>
  modeltime_accuracy() |>
  table_modeltime_accuracy(.interactive = TRUE, bordered = TRUE, resizable = TRUE)

calibration_tbl |>
  modeltime_forecast(new_data = testing(splits), actual_data = data_prep_tbl) |>
  plot_modeltime_forecast(.conf_interval_show = FALSE)

# * Refitting & Forecasting

# Best by RMSE
model_ts_best <- calibration_tbl |>
  select_best_id(n = 2)

refit_tbl <- calibration_tbl |>
  filter(.model_id %in% model_ts_best) |>
  modeltime_refit(data = data_prep_tbl)
# error with refitting

refit_tbl |>
  modeltime_forecast(new_data = forecast_tbl, actual_data = data_prep_tbl) |>
  plot_modeltime_forecast(.conf_interval_fill = "lightblue")


