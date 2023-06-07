# Generate Exam Dataset from M4-Competition

# Follow these steps to obtain the data:
# 1. Go to Mcompetitions/M4-methods (https://github.com/Mcompetitions/M4-methods)
# and download the whole repository (it takes a while since it is 1.4GB).
# 2. Keep only the Dataset folder
# 3. Use this script pointing the path to the Dataset folder to generate the
# sample data required for the exam.



# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)



# Info dataset ------------------------------------------------------------

info_path <- list.files("exam/datasets/m4", pattern = "\\.csv", full.names = TRUE)
m4_info <- read_csv(info_path)

# Function to sample info ids
m4_sample_info <- function(data, type, n, seed = 123) {

  data_sampled <- data %>%
      filter(SP == type) %>%
      mutate(
        date = dmy_hm(StartingDate),
        date = ifelse(
          date > "2019-12-31 00:00",
          str_replace(as.character(date), "^20(\\d\\d)(.*)", "19\\1\\2"),
          as.character(date)
        ),
        date = ymd_hms(date)
      ) %>%
      select(-StartingDate) %>%
      drop_na(date)

  set.seed(seed)
  data_sampled <- data_sampled %>%
    slice_sample(n = n_sample) %>%
    set_names(c("id", "category", "freq", "horizon", "period", "start_date"))

  return(data_sampled)

}

types <- unique(m4_info$SP)
n_sample <- 20 # 20 time series for each period

m4_info_sampled <- map_df(types, ~ m4_sample_info(m4_info, type = ., n = n_sample, seed = 123))



# Train & Test datasets ---------------------------------------------------

train_paths <- list.files("exam/datasets/m4/train", full.names = TRUE)
train <- map(train_paths, read_csv)
test_paths <- list.files("exam/datasets/m4/test", full.names = TRUE)
test <- map(test_paths, read_csv)

# Function to sample and combine train and test data
m4_sample_data <- function(train, test, ids) {

  # function to sample train and test data
  sampling_ids <- function(data, ids) {
    data <- data %>%
      rename("id" = "V1") %>%
      filter(id %in% ids) %>%
      pivot_longer(-id, names_to = "date") %>%
      drop_na() %>%
      mutate(date = as.numeric(str_remove_all(date, "V")) - 1)
    return(data)
  }

  # sampling train
  train_sampled <- map_df(
    seq_along(train),
    ~ sampling_ids(train[[.]], ids = ids)
  ) %>%
    mutate("type" = "train", .after = "id")

  # sampling test
  test_sampled <- map_df(
    seq_along(test),
    ~ sampling_ids(test[[.]], ids = ids)
  ) %>%
    mutate("type" = "test", .after = "id")

  # combining train and test
  data_sampled <- bind_rows(train_sampled, test_sampled) %>%
    arrange(id, desc(type), date)

  return(data_sampled)

}

m4_data_sampled <- m4_sample_data(train, test, ids = unique(m4_info_sampled$id))
unique(m4_data_sampled$id) # n_sample time series for each period
m4_data_sampled %>% count(id) %>% View() # length of each time series



# Data cleaning -----------------------------------------------------------

m4_data_sampled_cleaned <- m4_data_sampled %>%
  left_join(m4_info_sampled) %>%
  left_join(
    m4_data_sampled %>%
      count(id, type) %>%
      filter(type == "train") %>%
      select(-type)
  ) %>%
  mutate(date = ifelse(type == "test", date + n, date)) %>%
  select(-n) %>%
  mutate(
    date = case_when(
      period == "Hourly" ~ start_date + hours(date - 1),
      period == "Daily" ~ start_date + days(date - 1),
      period == "Weekly" ~ start_date + weeks(date - 1),
      period == "Monthly" ~ start_date %m+% months(date - 1),
      period == "Quarterly" ~ start_date %m+% months((date - 1) * 3),
      period == "Yearly" ~ start_date + years(date - 1),
      TRUE ~ NA_POSIXct_
    )
  ) %>%
  select(id, date, value, type, period)



# Save data ---------------------------------------------------------------

m4_sample <- list(
  "info" = m4_info_sampled,
  "data" = m4_data_sampled_cleaned
)
write_rds(m4_sample, file = "exam/datasets/m4/hackaton_m4.rds")
write_csv(m4_info_sampled, file = "exam/datasets/m4/hackaton_m4_info.csv")
write_csv(m4_data_sampled_cleaned, file = "exam/datasets/m4/hackaton_m4.csv")
