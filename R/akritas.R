#' @title Akritas Conditional Non-Parametric Survival Estimator
#' @name akritas
#'
#' @description The Akritas survival estimator is a conditional nearest-neighbours approach to the
#' more common Kaplan-Meier estimator. Common usage includes IPCW Survival models and measures,
#' which do not assume that censoring is independent of the covariates.
#'
#' @details
#' This implementation uses a fit/predict interface to allow estimation on unseen data after
#' fitting on training data. This is achieved by fitting the empirical CDF on the training data
#' and applying this to the new data.
#'
#' @param ... `ANY` \cr
#' Additional arguments, currently unused.
#'
#' @template param_traindata
#'
#' @references
#' Akritas, M. G. (1994).
#' Nearest Neighbor Estimation of a Bivariate Distribution Under Random Censoring.
#' Ann. Statist., 22(3), 1299–1327.
#' \doi{10.1214/aos/1176325630}
#'
#' @return An object inheriting from class `akritas`.
#'
#' @examples
#' if (requireNamespaces(c("distr6", "survival"))) {
#'   library(survival)
#'   akritas(Surv(time, status) ~ ., data = rats[1:10, ])
#' }
#' @export
akritas <- function(
    formula = NULL, data = NULL, reverse = FALSE,
    time_variable = "time", status_variable = "status",
    x = NULL, y = NULL, ...) {
  if (!requireNamespaces("distr6")) {
    stop("Package 'distr6' required but not installed.") # nocov
  }

  call <- match.call()

  data <- clean_train_data(formula, data, time_variable, status_variable, x, y, reverse)

  # use multivariate Empirical if multiple covariates otherwise univariate
  if (ncol(data$x) == 1) {
    Fhat <- distr6::Empirical$new(data$x)
  } else {
    Fhat <- distr6::EmpiricalMV$new(data$x)
  }

  return(structure(
    list(
      y = data$y, x = data$x,
      xnames = colnames(data$x),
      Fhat = Fhat,
      FX = Fhat$cdf(data = data$x),
      call = call
    ),
    name = "Akritas Estimator",
    class = c("akritas", "survivalmodel")
  ))
}

#' @title Predict method for Akritas Estimator
#'
#' @description Predicted values from a fitted Akritas estimator.
#'
#' @details
#' This implementation uses a fit/predict interface to allow estimation on unseen data after
#' fitting on training data. This is achieved by fitting the empirical CDF on the training data
#' and applying this to the new data.
#'
#' @template return_predict
#'
#' @param object (`akritas(1)`)\cr
#' Object of class inheriting from `"akritas"`.
#' @param newdata `(data.frame(1))`\cr
#' Testing data of `data.frame` like object, internally is coerced with [stats::model.matrix()].
#' If missing then training data from fitted object is used.
#' @param times `(numeric())`\cr
#' Times at which to evaluate the estimator. If `NULL` (default) then evaluated at all unique times
#' in the training set.
#' @param lambda (`numeric(1)`)\cr
#' Bandwidth parameter for uniform smoothing kernel in nearest neighbours estimation.
#' The default value of `0.5` is arbitrary and should be chosen by the user.
#' @param type (`character(1)`)\cr
#' Type of predicted value. Choices are survival probabilities over all time-points in training
#' data (`"survival"`) or a relative risk ranking (`"risk"`), which is the sum of the predicted
#' cumulative hazard function so higher rank implies higher risk of event, or both (`"all"`).
#' @param distr6 (`logical(1)`)\cr
#' If `FALSE` (default) and `type` is `"survival"` or `"all"` returns matrix of survival
#' probabilities, otherwise returns a [distr6::Matdist()].
#' @param ntime `(numeric(1))`\cr
#' Number of unique time-points in the training set, default is 150.
#' @param round_time `(numeric(1))`\cr
#' Number of decimal places to round time-points to, default is 2, set to `FALSE` for no rounding.
#' @param ... `ANY` \cr
#' Currently ignored.
#'
#' @references
#' Akritas, M. G. (1994).
#' Nearest Neighbor Estimation of a Bivariate Distribution Under Random Censoring.
#' Ann. Statist., 22(3), 1299–1327.
#' \doi{10.1214/aos/1176325630}
#'
#'
#' @examples
#' if (requireNamespaces(c("distr6", "survival"))) {
#'
#' library(survival)
#'
#' train <- 1:10
#' test <- 11:20
#' fit <- akritas(Surv(time, status) ~ ., data = rats[train, ])
#' predict(fit, newdata = rats[test, ])
#'
#' # when lambda = 1, identical to Kaplan-Meier
#' fit <- akritas(Surv(time, status) ~ ., data = rats[1:100, ])
#' predict_akritas <- predict(fit, newdata = rats[1:100, ], lambda = 1)[1, ]
#' predict_km <- survfit(Surv(time, status) ~ 1, data = rats[1:100, ])$surv
#' all(predict_akritas == predict_km)
#'
#' # Use distr6 = TRUE to return a distribution
#' predict_distr <- predict(fit, newdata = rats[test, ], distr6 = TRUE)
#' predict_distr$survival(100)
#'
#' # Return a relative risk ranking with type = "risk"
#' predict(fit, newdata = rats[test, ], type = "risk")
#'
#' # Or survival probabilities and a rank
#' predict(fit, newdata = rats[test, ], type = "all", distr6 = TRUE)
#' }
#' @export
predict.akritas <- function(object, newdata, times = NULL,
  lambda = 0.5,
  type = c("survival", "risk", "all"),
  distr6 = FALSE, ntime = 150, round_time = 2, ...) {

  type <- match.arg(type)
  unique_times <- sort(unique(object$y[, 1, drop = FALSE]))
  if (!is.logical(round_time) || round_time) {
    unique_times <- unique(round(unique_times, round_time))
  }
  # using same method as in ranger
  unique_times <- unique_times[
    unique(round(seq.int(1, length(unique_times), length.out = ntime)))
  ]

  truth <- object$y
  newdata <- clean_test_data(object, newdata)

  ord <- order(truth[, 1], decreasing = TRUE)
  truth <- truth[ord, ]
  fx_train <- object$FX[ord]

  if (is.null(times)) {
    predict_times <- unique_times
  } else {
    predict_times <- sort(unique(times))
  }

  surv <- C_Akritas(
    truth = truth,
    predict_times = predict_times,
    unique_times = unique_times,
    FX_train = fx_train,
    FX_predict = object$Fhat$cdf(data = newdata),
    lambda = lambda
  )

  colnames(surv) <- round(predict_times, 6)
  surv <- fill_na(round(surv, 4))

  ret <- list()

  if (type %in% c("survival", "all")) {
    if (!distr6) {
      ret$surv <- surv
    } else {


      ret$surv <- distr6::as.Distribution(
        1 - surv, fun = "cdf",
        decorators = c("CoreStatistics", "ExoticStatistics")
      )
    }
  }

  if (type %in% c("risk", "all")) {
    ret$risk <- surv_to_risk(surv)
  }

  if (length(ret) == 1) {
    return(ret[[1]])
  } else {
    return(ret)
  }
}
