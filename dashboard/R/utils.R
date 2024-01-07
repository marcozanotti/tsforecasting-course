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

# function to install and load the specified packages
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

# set global options
set_options <- function() {

  op <- options()
  op.tsf.dashboard <- list(
    # Global Paths
    tsf.dashboard.methods = list(
      "ts" = c("Rolling Average", "ETS", "ARIMA"),
      "ml" = c("Linear Regression", "Elastic Net")
    ),
    tsf.dashboard.methods_params = list(
      "Rolling Average" = c("window_size"),
      "ETS" = c("error", "trend", "season", "damping", "smooth_level", "smooth_trend", "smooth_season"),
      "ARIMA" = c(""),
      "Linear Regression" = "none",
      "Elastic Net" = c("penalty", "mixture")
    )
  )
  toset <- !(names(op.tsf.dashboard) %in% names(op))
  if (any(toset)) options(op.tsf.dashboard[toset])

  return(invisible(NULL))

}

# function to understand if the method is a time series or a machine learning one
parse_method <- function(method) {

  mtd <- getOption("tsf.dashboard.methods")

  if (method %in% mtd$ts) {
    res <- "ts"
  } else if (method %in% mtd$ml) {
    res <- "ml"
  } else {
    stop(paste("Unknown method", method))
  }

  return(res)

}

# check the parameters for ts and ml methods
check_parameters <- function(method, params) {

  mtd_prm <- getOption("tsf.dashboard.methods_params")[[method]]
  if (!all(mtd_prm %in% names(params))) {
    stop(paste("Parameters for method", method, "are not correct!"))
  }

}

# parse model informations
parse_model <- function(fit, method) {
  # for ETS
  # wkfl_fit$fit$desc
  # wkfl_fit$fit$models$model_1$par
}
