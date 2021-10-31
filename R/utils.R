# Helper Functions


# Function to check packages already loaded into NAMESPACE
check_namespace <- function(pkgs) {

  pkgs_notloaded <- pkgs[!pkgs %in% loadedNamespaces()]
  if (length(pkgs_notloaded) == 0) {
    res <- NULL
  } else {
    res <- pkgs_notloaded
  }
  return(res)

}


# Function to install and load the specified packages
install_and_load <- function(pkgs, repos = getOption("repos")) {

  pkgs_inst <- pkgs[!pkgs %in% installed.packages()]

  if (length(pkgs_inst) == 0) {
    lapply(pkgs, library, character.only = TRUE, quietly = TRUE)
    check_res <- check_namespace(pkgs)
    if (is.null(check_res)) {
      res <- "All packages correctly installed and loaded."
    } else {
      res <- paste0(
        "Problems loading packages ",
        paste0(check_res, collapse = ", "),
        "."
      )
    }

  } else {

    inst_res <- vector("character", length(pkgs_inst))

    for (i in seq_along(pkgs_inst)) {
      inst_res_tmp <- tryCatch(
        utils::install.packages(pkgs_inst[i], dependencies = TRUE, repos = repos, quiet = TRUE),
        error = function(e) e,
        warning = function(w) w
      )
      if (!is.null(inst_res_tmp)) {
        inst_res[i] <- inst_res_tmp$message
      }
    }

    pkgs_err <- pkgs_inst[!inst_res == ""]
    if (length(pkgs_err) == 0) {
      lapply(pkgs, library, character.only = TRUE, quietly = TRUE)
      check_res <- check_namespace(pkgs)
      if (is.null(check_res)) {
        res <- "All packages correctly installed and loaded."
      } else {
        res <- paste0(
          "Problems loading packages ",
          paste0(check_res, collapse = ", "),
          "."
        )
      }
    } else {
      pkgs_noerr <- pkgs[!pkgs %in% pkgs_err]
      lapply(pkgs_noerr, library, character.only = TRUE, quietly = TRUE)
      check_res <- check_namespace(pkgs_noerr)
      if (is.null(check_res)) {
        res <- paste0(
          "Problems installing packages ",
          paste0(pkgs_err, collapse = ", "),
          "."
        )
      } else {
        res <- c(
          paste0(
            "Problems installing packages ",
            paste0(pkgs_err, collapse = ", "),
            "."
          ),
          paste0(
            "Problems loading packages ",
            paste0(check_res, collapse = ", "),
            "."
          )
        )
      }
    }

  }

  message(toupper(
    paste0(
      "\n\n\n",
      "\n==================================================================",
      "\nResults:\n ",
      res,
      "\n=================================================================="
    )
  ))
  return(invisible(res))

}


# Function to calibrate models, evaluate their accuracy and plot results
calibrate_evaluate_plot <- function(..., type = "testing", updated_desc = NULL) {

  if (type == "testing") {
    new_data <- testing(splits)
  } else {
    new_data <- training(splits) %>% drop_na()
  }

  calibration_tbl <- modeltime_table(...)

  if (!is.null(updated_desc)) {
    for (i in seq_along(updated_desc)) {
      calibration_tbl <- calibration_tbl %>%
        update_model_description(.model_id = i, .new_model_desc = updated_desc[i])
    }
  }

  calibration_tbl <- calibration_tbl %>%
    modeltime_calibrate(new_data)

  print(calibration_tbl %>% modeltime_accuracy())

  print(
    calibration_tbl %>%
      modeltime_forecast(new_data = new_data, actual_data = data_prep_tbl) %>%
      plot_modeltime_forecast(.conf_interval_show = FALSE)
  )

  return(invisible(calibration_tbl))

}


# Function to extract the .model_id of the "best" model according to a metric
select_best_id <- function(calibration, metric = "rmse", n) {

  if (metric == "rsq") {

    model_best_id <- calibration %>%
      modeltime_accuracy() %>%
      slice_max(!!rlang::sym(metric), n = n) %>%
      pull(.model_id)

  } else {

    model_best_id <- calibration_tbl %>%
      modeltime_accuracy() %>%
      slice_min(!!rlang::sym(metric), n = n) %>%
      pull(.model_id)

  }

  return(model_best_id)

}


# Function to add lags
lag_transf <- function(data){
  data_lags <- data %>%
    tk_augment_lags(optins_trans, .lags = lags)
  return(data_lags)
}


# Function to add lags by group
lag_transf_grouped <- function(data){
  data_lags <- data %>%
    group_by(id) %>%
    tk_augment_lags(optins_trans, .lags = lags) %>%
    ungroup()
  return(data_lags)
}

