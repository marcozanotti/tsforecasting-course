# Time Series Forecasting: Machine Learning and Deep Learning with R & Python ----

# Lecture 8: Automatic Machine Learning Algorithms ------------------------
# 2021/2022
# Marco Zanotti

# Goals:
# - H20



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

rcp_spec <- recipe(optins_trans ~ ., data = training(splits)) %>%
  step_timeseries_signature(optin_time) %>%
  step_rm(matches("(iso)|(xts)|(hour)|(minute)|(second)|(am.pm)")) %>%
  step_normalize(matches("(index.num)|(year)|(yday)")) %>%
  step_rm(starts_with("lag_"))

rcp_spec %>% prep() %>% juice() %>% glimpse()



# H2O - Automatic ML Framework --------------------------------------------

# H2O AI
# https://docs.h2o.ai/h2o/latest-stable/h2o-docs/index.html
# Algorithms
# https://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science.html
# Modeltime integration Roadmap
# https://github.com/business-science/modeltime.h2o/issues/1


# * Initialize H2O --------------------------------------------------------

# Dependency on JAVA
# Possible problems related to initialization of H2O from R / Python API:
# - Old JAVA version
# - root privileges
# - JAVA 32bit installed and not 64bit
# - JAVA_HOME env variable not set, Sys.getenv('JAVA_HOME')
# Solutions:
# - https://docs.h2o.ai/h2o/latest-stable/h2o-r/docs/index.html
# - https://docs.h2o.ai/h2o/latest-stable/h2o-docs/faq/java.html
# - https://stackoverflow.com/questions/3892510/set-environment-variables-for-system-in-r
# - Sys.setenv('JAVA_HOME')

# Common steps:
# 1) Uninstall H2O
# if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
# if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
# 2) Install the latest version of JAVA
# https://www.oracle.com/technetwork/java/javase/downloads/index.html
# 3) Install H2O again
# install.packages("h2o", type = "source", repos = (c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")))
# library(h2o)
# 4) Set the JAVA_HOME env variable
# Sys.getenv('JAVA_HOME')
# Sys.setenv(JAVA_HOME="/usr/lib/jvm/jdk-17/")
# Sys.getenv('JAVA_HOME')

Sys.setenv(JAVA_HOME = "/usr/lib/jvm/jdk-17/")
h2o.init(nthreads = -1)


# * Engines ---------------------------------------------------------------

?automl_reg()

# Algorithms:
# - DRF (This includes both the Distributed Random Forest (DRF) and
#   Extremely Randomized Trees (XRT) models.
# - GLM (Generalized Linear Model with regularization)
# - XGBoost (XGBoost GBM)
# - GBM (H2O GBM)
# - DeepLearning (Fully-connected multi-layer artificial neural network)
# - StackedEnsemble (Stacked Ensembles, includes an ensemble of all the
#   base models and ensembles using subsets of the base models)

model_spec_h2o <- automl_reg(mode = 'regression') %>%
  set_engine(
    engine = "h2o",
    max_runtime_secs = 30,
    max_runtime_secs_per_model = 30,
    project_name = "project_tsf_course",
    max_models = 10,
    nfolds = 10,
    sort_metric = "rmse",
    exclude_algos = c("DeepLearning"), # remove deeplearning for computation time
    verbosity = NULL,
    seed = 123
  )
model_spec_h2o


# * Workflows -------------------------------------------------------------

# - This step will take some time depending on your engine specifications
wrkfl_fit_h2o <- workflow() %>%
  add_model(model_spec_h2o) %>%
  add_recipe(rcp_spec) %>%
  fit(training(splits))
wrkfl_fit_h2o

wrkfl_fit_h2o %>% automl_leaderboard() %>% head(20)
gbm_name <- "GBM_2_AutoML_1_20220219_112345"
xgb_name <- "XGBoost_3_AutoML_1_20220219_112345"
stack_name <- "StackedEnsemble_AllModels_5_AutoML_1_20220219_112345"

# change default selected models
wrkfl_fit_h20_gbm <- wrkfl_fit_h2o %>%
  automl_update_model(gbm_name)
wrkfl_fit_h20_xgb <- wrkfl_fit_h2o %>%
  automl_update_model(xgb_name)
wrkfl_fit_h20_stack <- wrkfl_fit_h2o %>%
  automl_update_model(stack_name)


# * Calibration, Evaluation & Plotting ------------------------------------

calibrate_evaluate_plot(wrkfl_fit_h2o)

calibrate_evaluate_plot(
  wrkfl_fit_h20_gbm,
  wrkfl_fit_h20_xgb,
  wrkfl_fit_h20_stack
)


# * Refitting & Forecasting -----------------------------------------------

# by the moment re-train all AutoML (see roadmap)
calibration_tbl <- modeltime_table(
  wrkfl_fit_h20_gbm
) %>%
  modeltime_calibrate(testing(splits))

refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = data_prep_tbl)

refit_tbl %>%
  modeltime_forecast(new_data = forecast_tbl, actual_data = data_prep_tbl) %>%
  plot_modeltime_forecast(.conf_interval_fill = "lightblue")


# * Save Artifacts --------------------------------------------------------

wrkfl_fit_h20_gbm %>%
  save_h2o_model(path = "artifacts/h2o_models/h2o_gbm")

load_h2o_model("artifacts/h2o_models/h2o_gbm/")


# * Shout-down H2O --------------------------------------------------------

h2o.shutdown(prompt = FALSE)

