# Time Series Forecasting: Machine Learning and Deep Learning with R & Python ----

# Lecture Extra: Clustering -----------------------------------------------
# Marco Zanotti

# Goals:
# - Clustering via tsfeatures



# Packages ----------------------------------------------------------------

library(dplyr)
library(purrr)
library(timetk)
library(tsfeatures)



# Data & Artifacts --------------------------------------------------------

walmart_sales_weekly
View(walmart_sales_weekly)

walmart_sales_weekly |> count(id)
walmart_sales_weekly |>
  group_by(id) |>
  plot_time_series(
    Date, Weekly_Sales,
    .facet_ncol  = 2,
    .interactive = FALSE
  )



# Feature Extraction ------------------------------------------------------

# Custom Function
my_mean <- function(x, na.rm = TRUE) {
  mean(x, na.rm = na.rm)
}

# Using the tk_tsfeatures() function, we can quickly get the “tsfeatures”
# for each of the time series. A few important points:
#
# The features parameter come from the tsfeatures R package. Use one
# of the function names from tsfeatures R package e.g.(“lumpiness”, “stl_features”).
#
# We can supply any function that returns an aggregation (e.g. “mean” will
# apply the base::mean() function).
#
# You can supply custom functions by creating a function and providing it
# (e.g. my_mean() defined below)

tsfeature_tbl <- walmart_sales_weekly |>
  group_by(id) |>
  tk_tsfeatures(
    .date_var = Date,
    .value    = Weekly_Sales,
    .period   = 52,
    .features = c("frequency", "stl_features", "entropy", "acf_features", "my_mean"),
    .scale    = TRUE,
    .prefix   = "ts_"
  ) |>
  ungroup()
tsfeature_tbl
View(tsfeature_tbl)



# Clustering --------------------------------------------------------------

# * K-means ---------------------------------------------------------------
set.seed(123)
cluster_tbl <- tibble(
  cluster = tsfeature_tbl |>
    select(-id) |>
    as.matrix() |>
    kmeans(centers = 3, nstart = 100) |>
    pluck("cluster")
) |>
  bind_cols(tsfeature_tbl)
cluster_tbl

cluster_tbl |>
  select(cluster, id) |>
  right_join(walmart_sales_weekly, by = "id") |>
  group_by(id) |>
  plot_time_series(
    Date, Weekly_Sales,
    .color_var   = cluster,
    .facet_ncol  = 2,
    .interactive = FALSE
  )

