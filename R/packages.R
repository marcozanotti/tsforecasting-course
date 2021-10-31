# Packages
pkgs <- c(
  "tidyverse", "lubridate",
  "timetk", "DataExplorer", "fs",
  "forecast", "prophet",
  "glmnet", "earth",
  "kernlab",
  "kknn",
  "randomForest", "ranger", "xgboost", "treesnip", "lightgbm", "catboost",
  "Cubist", "rules",
  "tidymodels", "modeltime", "modeltime.h2o", "modeltime.ensemble",
  "parallel", "doFuture", "tictoc",
  "reticulate"
)
install_and_load(pkgs)
