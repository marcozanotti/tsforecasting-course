# function to compute the mean error
me_impl <- function(truth, estimate, case_weights = NULL) {
  mean((estimate - truth)) # pay attention to this formulation
}

me_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {

  yardstick::check_numeric_metric(truth, estimate, case_weights)

  if (na_rm) {
    result <- yardstick::yardstick_remove_missing(truth, estimate, case_weights)
    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick::yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  me_impl(truth, estimate, case_weights = case_weights)

}

me <- function(data, ...) {
  UseMethod("me")
}

me <- yardstick::new_numeric_metric(me, direction = "minimize")

me.data.frame <- function(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {

  yardstick::numeric_metric_summarizer(
    name = "me",
    fn = me_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )

}

# function to format accuracy table
format_accuracy <- function(accuracy_table, single_method = TRUE, digits = 2) {

  if (single_method == TRUE) {
    res <- accuracy_table |>
      select(-1, -2) |>
      rename("Type" = ".type") |>
      rename_with(.fn = toupper, .cols = -c("Type")) |>
      relocate("ME", .after = "Type") |>
      mutate(across(where(is.numeric), ~round(., digits))) |>
      pivot_longer(cols = -1, names_to = "Metric", values_to = "Value") |>
      pivot_wider(names_from = "Type", values_from = "Value")
  } else {
    res <- accuracy_table |>
      select(-1) |>
      rename("Algorithm" = ".model_desc", "Type" = ".type") |>
      rename_with(.fn = toupper, .cols = -c("Algorithm", "Type")) |>
      relocate("ME", .after = "Type") |>
      mutate(across(where(is.numeric), ~round(., digits)))
  }
  return(res)

}
