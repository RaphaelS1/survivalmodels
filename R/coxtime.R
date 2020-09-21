#' @title Cox-Time Survival Neural Network
#'
#' @description Cox-Time neural fits a neural network based on the Cox PH with
#'  time-varying effects. Implemented from the `pycox` Python package via \CRANpkg{reticulate}.
#'
#' @param formula `(formula(1))`\cr
#' Objet specifying the model fit, left-hand-side of formula should describe a [survival::Surv()]
#' object.
#' @param data `(data.frame(1))`\cr
#' Training data of `data.frame` like object, internally is coerced with [stats::model.matrix()].
#' @param reverse `(logical(1))`\cr
#' If `TRUE` fits estimator on censoring distribution, otherwise (default) survival distribution.
#' @param time_variable `(character(1))`\cr
#' Alternative method to call the function. Name of the 'time' variable, required if `formula`.
#' or `x` and `Y` not given.
#' @param status_variable `(character(1))`\cr
#' Alternative method to call the function. Name of the 'status' variable, required if `formula`
#' or `x` and `Y` not given.
#' @param x `(data.frame(1))`\cr
#' Alternative method to call the function. Required if `formula, time_variable` and
#' `status_variable` not given. Data frame like object of features which is internally
#' coerced with `model.matrix`.
#' @param y `([survival::Surv()])`\cr
#' Alternative method to call the function. Required if `formula, time_variable` and
#' `status_variable` not given. Survival outcome of right-censored observations.
#' @param frac,standardize_time,log_duration,with_mean,with_std \cr
#' See [prepare_train_data].
#' @param activation \cr
#' See [get_pycox_activation].
#' @param num_nodes,batch_norm,dropout \cr
#' See [build_pytorch_net].
#' @param early_stopping,best_weights,min_delta,patience \cr
#' See [get_pycox_callbacks].
#' @param device `(integer(1)/character(1))`\cr
#' Passed to `pycox.models.Coxtime`, specifies device to compute models on.
#' @param shrink `(numeric(1))`\cr
#' Passed to `pycox.models.Coxtime`, shrinkage parameter for regularization.
#' @param batch_size `(integer(1))`\cr
#' Passed to `pycox.models.Coxtime.fit`, elements in each batch.
#' @param epochs `(integer(1))`\cr
#' Passed to `pycox.models.Coxtime.fit`, number of epochs.
#' @param verbose `(logical(1))`\cr
#' Passed to `pycox.models.Coxtime.fit`, should information be displayed during fitting.
#' @param num_workers `(integer(1))`\cr
#' Passed to `pycox.models.Coxtime.fit`, number of workers used in the dataloader.
#' @param shuffle `(logical(1))`\cr
#' Passed to `pycox.models.Coxtime.fit`, should order of dataset be shuffled?
#' @param ... `ANY` \cr
#' Passed to [get_pycox_optim].
#'
#' @references
#' Kvamme, H., Borgan, Ø., & Scheel, I. (2019).
#' Time-to-event prediction with neural networks and Cox regression.
#' Journal of Machine Learning Research, 20(129), 1–30.
#'
#' @return An object inheriting from class `coxtime`.
#'
#' @export
coxtime <- function(formula = NULL, data = NULL, reverse = FALSE,
                    time_variable = NULL, status_variable = NULL,
                    x = NULL, y = NULL, frac = 0, standardize_time = FALSE,
                    log_duration = FALSE, with_mean = TRUE, with_std = TRUE,
                    activation = "relu", num_nodes = c(32L, 32L), batch_norm = TRUE,
                    dropout = NULL, device = NULL, shrink = 0, early_stopping = FALSE,
                    best_weights  = FALSE,  min_delta = 0, patience = 10L, batch_size = 256L,
                    epochs = 1L, verbose = TRUE, num_workers = 0L, shuffle = TRUE, ...) {

  call <- match.call()

  requireNamespace("reticulate")
  pycox = reticulate::import("pycox")
  torch = reticulate::import("torch")
  torchtuples = reticulate::import("torchtuples")

  data <- clean_data(formula, data, time_variable, status_variable, x, y, reverse)

  data <- prepare_train_data(data$x, data$y, frac = frac,
                             standardize_time = standardize_time, log_duration = log_duration,
                             with_mean = with_mean, with_std = with_std)

  x_train = data$x_train
  y_train = data$y_train

  activation = get_pycox_activation(activation, construct = FALSE)

  net <- pycox$models$cox_time$MLPVanillaCoxTime(
    in_features = x_train$shape[1],
    num_nodes = reticulate::r_to_py(as.integer(num_nodes)),
    activation = activation,
    batch_norm = batch_norm,
    dropout = dropout
  )

  # Get optimizer and set-up model
  model = pycox$models$CoxTime(
    net = net,
    labtrans = data$labtrans,
    optimizer = get_pycox_optim(net = net, ...),
    device = device,
    shrink = shrink,
  )

  model$fit(
    input = x_train,
    target = y_train,
    callbacks = get_pycox_callbacks(early_stopping, best_weights, min_delta, patience),
    val_data = data$val,
    batch_size = as.integer(batch_size),
    epochs = as.integer(epochs),
    verbose = verbose,
    num_workers = as.integer(num_workers),
    shuffle = shuffle
  )

  return(structure(list(y = y_train, x = x_train,
                        xnames = x_train,
                        model = model,
                        call = call),
                   class = "coxtime"
  ))
}

#' @title Predict Method for Cox-Time Survival Neural Network
#'
#' @description Predicted values from a fitted Cox-Time ANN.
#'
#' @param object (`akritascoxtime`)\cr
#' Object of class inheriting from `"coxtime"`.
#' @param newdata `(data.frame(1))`\cr
#' Testing data of `data.frame` like object, internally is coerced with [stats::model.matrix()].
#' If missing then training data from fitted object is used.
#' @param batch_size `(integer(1))`\cr
#' Passed to `pycox.models.Coxtime.fit`, elements in each batch.
#' @param num_workers `(integer(1))`\cr
#' Passed to `pycox.models.Coxtime.fit`, number of workers used in the dataloader.
#' @param type (`numeric(1)`)\cr
#' Type of predicted value. Choices are survival probabilities over all time-points in training
#' data (`"survival"`) or a relative risk ranking (`"risk"`), which is the mean cumulative hazard
#' function over all time-points, or both (`"all"`).
#' @param distr6 `(logical(1))`\cr
#' If `FALSE` (default) and `type` is `"survival"` or `"all"` returns data.frame of survival
#' probabilities, otherwise returns a [distr6::VectorDistribution()].
#' @param ... `ANY` \cr
#' Currently ignored.
#'
#' @references
#' Kvamme, H., Borgan, Ø., & Scheel, I. (2019).
#' Time-to-event prediction with neural networks and Cox regression.
#' Journal of Machine Learning Research, 20(129), 1–30.
#'
#' @return An object inheriting from class `coxtime`.
#'
#' @export
predict.coxtime <- function(object, newdata, batch_size = 256L, num_workers = 0L,
                            type = c("survival", "risk", "all"), distr6 = FALSE, ...) {
  object$model$model$compute_baseline_hazards()

  truth <- object$y
  if (missing(newdata)) {
    newdata <- object$x
  } else {
    newdata <- stats::model.matrix(~., newdata)[, -1, drop = FALSE]
  }

  ord <- match(colnames(newdata), colnames(object$x), nomatch = NULL)
  newdata <- newdata[, !is.na(ord), drop = FALSE]
  newdata <- newdata[, ord[!is.na(ord)], drop = FALSE]
  if (!checkmate::testNames(colnames(newdata), identical.to = colnames(object$x))) {
    stop(sprintf(
      "Names in newdata should be identical to {%s}.",
      paste0(colnames(object$x), collapse = ", ")
    ))
  }

  # get test data
  x_test = reticulate::r_to_py(newdata)$values$astype("float32")

  # predict survival probabilities
  surv = object$model$model$predict_surv_df(
    x_test,
    batch_size = as.integer(batch_size),
    num_workers = as.integer(num_workers)
  )

  ret <- list()

  if (type %in% c("survival", "all")) {
    if (!distr6) {
      ret$surv <- surv
    } else {
      # cast to distr6
      x = rep(list(list(x = round(as.numeric(rownames(surv)), 5), pdf = 0)), nrow(x_test))
      for (i in seq_len(nrow(x_test))) {
        # fix for infinite hazards - invalidate results for NaNs
        if (any(is.nan(surv[, i]))) {
          x[[i]]$pdf = c(1, numeric(length(x[[i]]$x) - 1))
        } else {
          x[[i]]$pdf = round(1 - surv[, i], 6)
          x[[i]]$pdf = c(x[[i]]$pdf[1], diff(x[[i]]$pdf))
          x[[i]]$pdf[x[[i]]$pdf < 0.000001] = 0L
          x[[i]]$pdf
        }
      }

      ret$distr = distr6::VectorDistribution$new(
        distribution = "WeightedDiscrete", params = x,
        decorators = c("CoreStatistics", "ExoticStatistics"))
    }
  }

  if (type %in% c("risk", "all")) {
    ret$risk <- colMeans(-log(surv))
  }

  if (length(ret) == 1) {
    return(ret[[1]])
  } else {
    return(ret)
  }

}
