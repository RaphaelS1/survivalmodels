#' @title DNNSurv Neural Network for Conditional Survival Probabilities
#'
#' @description DNNSurv neural fits a neural network based on pseudo-conditional survival
#' probabilities.
#'
#' @details Code for generating the conditional probabilities and pre-processing data is taken from
#' \url{https://github.com/lilizhaoUM/DNNSurv}.
#'
#' @template param_traindata
#' @template return_train
#'
#' @param cutpoints `(numeric())`\cr
#' Points at which to cut survival time into discrete points.
#' @param cuts `(integer(1))`\cr
#' If `cutpoints` not provided then number of equally spaced points at which to cut survival time.
#' @param custom_model `(keras.engine.training.Model(1))` \cr
#' Optional custom architecture built with [build_keras_net] or directly with \CRANpkg{keras}.
#' Output layer should be of length `1` input is number of features plus number of cuts.
#' @param loss_weights,weighted_metrics See [keras::compile.keras.engine.training.Model].
#' @param optimizer `(character(1))`\cr
#' See [get_keras_optimizer].
#' @param early_stopping `(logical(1))`\cr
#' If `TRUE` then early stopping callback is included.
#' @param min_delta,patience,baseline,restore_best_weights See [keras::callback_early_stopping].
#' @param verbose `(integer(1))` \cr
#' Level of verbosity for printing, `0` or `1`.
#' @param batch_size,epochs,validation_split,shuffle,sample_weight,initial_epoch,steps_per_epoch,validation_steps See [keras::fit.keras.engine.training.Model]. # nolint
#' @param ... `ANY` \cr
#' Passed to [get_keras_optimizer].
#'
#' @references
#' Zhao, L., & Feng, D. (2020).
#' DNNSurv: Deep Neural Networks for Survival Analysis Using Pseudo Values.
#' https://arxiv.org/abs/1908.02337
#'
#' @examples
#' \donttest{
#' if (requireNamespaces(c("keras", "pseudo")))
#'   # all defaults
#'   dnnsurv(data = simsurvdata(10))
#'
#'   # setting common parameters
#'   dnnsurv(time_variable = "time", status_variable = "status", data = simsurvdata(10),
#'           early_stopping = TRUE, epochs = 100L, validation_split = 0.3)
#' }
#'
#' @export
dnnsurv <- function(formula = NULL, data = NULL, reverse = FALSE,
                     time_variable = "time", status_variable = "status",
                     x = NULL, y = NULL, cutpoints = NULL, cuts = 5L,
                    custom_model = NULL, loss_weights = NULL, weighted_metrics = NULL,
                    optimizer = "adam", early_stopping = FALSE, min_delta = 0, patience = 0L,
                    verbose = 0L, baseline = NULL, restore_best_weights = FALSE,
                    batch_size = 32L, epochs = 10L, validation_split = 0, shuffle = TRUE,
                    sample_weight = NULL, initial_epoch = 0L, steps_per_epoch = NULL,
                    validation_steps = NULL, ...) {

  if (!requireNamespace("keras", quietly = TRUE)) {
    stop("Package 'keras' required but not installed.") # nocov
  }

  if (!requireNamespace("pseudo", quietly = TRUE)) {
    stop("Package 'pseudo' required but not installed.") # nocov
  }

  call <- match.call()

  data <- clean_train_data(formula, data, time_variable, status_variable, x, y, reverse)

  time <- data$y[, 1L]

  if (is.null(cutpoints)) {
    if (is.null(cuts)) {
      stop("One of 'cuts' or 'cutpoints' must be provided.")
    }
    cutpoints <- seq.int(min(time), max(time), length.out = cuts)
  }

  pseudo_cond <- .get_pseudo_conditional(
    time,
    data$y[, 2L],
    cutpoints)

  x_train <- cbind(data$x[pseudo_cond$id, ],
                  model.matrix(~ as.factor(pseudo_cond$s) + 0))
  y_train <- pseudo_cond$pseudost

  if (!is.null(custom_model)) {
    model <- custom_model
  } else {
    model <- keras::keras_model_sequential()
    keras::layer_dense(model,
                       units = 8, kernel_regularizer = keras::regularizer_l2(0.0001),
                       activation = "tanh",
                       input_shape = dim(x_train)[[2]])
    keras::layer_dense(model,
                       units = 4, kernel_regularizer = keras::regularizer_l2(0.01),
                       activation = "tanh")
    keras::layer_dense(model, units = 1, activation = "sigmoid")
  }

  keras::compile(
    model,
    loss = "mse",
    metrics = "mae",
    loss_weights = loss_weights,
    weighted_metrics = weighted_metrics,
    optimizer = get_keras_optimizer(optimizer = optimizer, ...)
  )

  if (early_stopping) {
    callbacks <- list(
      keras::callback_early_stopping(
        min_delta = min_delta,
        patience = patience,
        verbose = verbose,
        baseline = baseline,
        restore_best_weights = restore_best_weights
      )
    )
  } else {
    callbacks <- NULL
  }

  keras::fit(
    model,
    x_train,
    y_train,
    callbacks = callbacks,
    batch_size = batch_size,
    epochs = epochs,
    verbose = verbose,
    validation_split = validation_split,
    shuffle = shuffle,
    sample_weight = sample_weight,
    initial_epoch = initial_epoch,
    steps_per_epoch = steps_per_epoch,
    validation_steps = validation_steps
  )

  structure(list(y = data$y, x = data$x,
                 xnames = colnames(data$x),
                 model = model,
                 call = call,
                 cutpoints = cutpoints),
            name = "DNNSurv Neural Network",
            class = c("dnnsurv", "survivalmodel")
  )
}

#' @title Predict Method for DNNSurv
#'
#' @description Predicted values from a fitted object of class dnnsurv.
#'
#' @template return_predict
#'
#' @param object `(dnnsurv(1))`\cr
#' Object of class inheriting from `"dnnsurv"`.
#' @param newdata `(data.frame(1))`\cr
#' Testing data of `data.frame` like object, internally is coerced with [stats::model.matrix()].
#' If missing then training data from fitted object is used.
#' @param batch_size `(integer(1))`\cr
#' Passed to [keras::predict.keras.engine.training.Model], elements in each batch.
#' @param verbose `(integer(1))`\cr
#' Level of verbosity for printing, `0` or `1`.
#' @param steps `(integer(1))`\cr
#' Number of batches before evaluation finished, see [keras::predict.keras.engine.training.Model].
#' @param callbacks `(list())`\cr
#' Optional callbacks to apply during prediction.
#' @param type (`character(1)`)\cr
#' Type of predicted value. Choices are survival probabilities over all time-points in training
#' data (`"survival"`) or a relative risk ranking (`"risk"`), which is the negative mean survival
#' time so higher rank implies higher risk of event, or both (`"all"`).
#' @param distr6 `(logical(1))`\cr
#' If `FALSE` (default) and `type` is `"survival"` or `"all"` returns matrix of survival
#' probabilities, otherwise returns a [distr6::VectorDistribution()].
#' @param ... `ANY` \cr
#' Currently ignored.
#'
#' @examples
#' \donttest{
#' if (requireNamespaces(c("keras", "pseudo")))
#'   fit <- dnnsurv(data = simsurvdata(10))
#'
#'   # predict survival matrix and relative risks
#'   predict(fit, simsurvdata(10), type = "all")
#'
#'   # return as distribution
#'   if (requireNamespaces("distr6")) {
#'     predict(fit, simsurvdata(10), distr6 = TRUE)
#'   }
#' }
#'
#' @export
predict.dnnsurv <- function(object, newdata, batch_size = 32L, verbose = 0L,
                           steps = NULL, callbacks = NULL,
                           type = c("survival", "risk", "all"), distr6 = FALSE,
                         ...) {

  newdata <- clean_test_data(object, newdata)

  x_test_all <- do.call(rbind, replicate(length(object$cutpoints), newdata, simplify = FALSE))
  smatrix_test <- model.matrix(~ as.factor(rep(object$cutpoints, each = nrow(newdata))) + 0)
  x_test_all <- cbind(x_test_all, smatrix_test)

  # predict test data
  pred <- predict(
    object$model,
    x = x_test_all,
    batch_size = batch_size,
    verbose = verbose,
    steps = steps,
    callbacks = callbacks
  )
  pred <- matrix(pred, nrow = nrow(newdata))
  ypred <- lapply(seq_along(object$cutpoints), function(i) {
    apply(pred[, 1:i, drop = FALSE], 1, prod)
  })
  surv <- Reduce(cbind, ypred)
  colnames(surv) <- object$cutpoints
  stopifnot(nrow(newdata) == nrow(surv))

  ret <- list()

  type <- match.arg(type)
  if (type %in% c("survival", "all")) {
    if (!distr6 || !requireNamespace("distr6", quietly = TRUE)) {
      if (distr6) {
        warning("'distr6' not installed, returning 'surv' as matrix.") # nocov
      }
      ret$surv <- surv
    } else {
      # ensure distribution not degenerate
      times <- as.numeric(colnames(surv))
      surv <- cbind(1, surv, 0)
      colnames(surv) <- c(0, times, max(times) + 1e-3)
      cdf = lapply(seq_len(nrow(newdata)), function(.x) list(cdf = 1 - surv[.x,]))

      ret$surv <- distr6::VectorDistribution$new(
        distribution = "WeightedDiscrete",
        shared_params = list(x = as.numeric(colnames(surv))),
        params = cdf,
        decorators = c("CoreStatistics", "ExoticStatistics"))
    }
  }

  if (type %in% c("risk", "all")) {
    ret$risk <- -apply(1 - surv, 1, function(.x) sum(c(.x[1], diff(.x)) * as.numeric(colnames(surv))))
  }

  if (length(ret) == 1) {
    return(ret[[1]])
  } else {
    return(ret)
  }
}
