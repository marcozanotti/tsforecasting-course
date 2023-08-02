
library(tidyverse)

d <- read_csv("exam/datasets/wind/evaluation.csv") |>
  mutate(date = ymd_h(date)) |>
  arrange(date)

temp <- read_csv("exam/datasets/wind/template.csv") |>
  mutate(date = ymd_h(date)) |>
  arrange(date)

mzf <- read_csv("exam/datasets/wind/marcozanotti_forecasts.csv") |>
  mutate(date = ymd_h(date)) |>
  arrange(date)

temp
mzf

rmse <- function(actual, estimate) {
  res <- sqrt(sum((actual - estimate)^2))
  return(res)
}


res <- vector("numeric", 7)
for (i in 1:7) {
  res[i] <- round(rmse(temp[[i+2]], mzf[[i+2]]), 2)
}
res
mean(res)

