# Packages
pkgs <- c(
  "tidyverse", "lubridate", 
  "timetk", "DataExplorer", "fs",
  "forecast", "prophet",
  "glmnet", "earth", 
  "kernlab", 
  "knn", 
  "randomForest", "ranger", "xgboost", 
  "Cubist",
  "tidymodels", "rules", "modeltime",
  "reticulate"
)
install_and_load(pkgs)