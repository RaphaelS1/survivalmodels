#' @title Fully Parametric Survival Model
#' @name parametric
#'
#' @description
#'
#' @details
#'
#' @param ... `ANY` \cr
#' Additional arguments, currently unused.
#'
#' @template param_traindata
#'
#' @return An object inheriting from class `parametric`.
#'
#' @examples
#' if (requireNamespaces(c("distr6", "survival"))) {
#'   library(survival)
#'   parametric(Surv(time, status) ~ ., data = rats[1:10, ])
#' }
#' @export
parametric <- function(
    formula = NULL, data = NULL, reverse = FALSE,
    time_variable = "time", status_variable = "status",
    x = NULL, y = NULL, eps = 1e-15, ...) {

  if (!requireNamespaces("distr6")) {
    stop("Package 'distr6' required but not installed.") # nocov
  }

  call <- match.call()

  data <- clean_train_data(formula, data, time_variable, status_variable, x, y, reverse)

  fit <- survreg(Surv(data$y) ~ data$x, x = TRUE, ...)

  location <- as.numeric(fit$coefficients[1])
  scale <- fit$scale

  if (scale < eps) {
    scale <- eps
  } else if (scale > .Machine$double.xmax) {
    scale <- .Machine$double.xmax
  }

  if (location < -709 &&
    fit$dist %in% c("weibull", "exponential", "loglogistic")) {
  location <- -709
  }

  basedist <- switch(fit$dist,
    "weibull" = distr6::Weibull$new(shape = 1 / scale, scale = exp(location),
      decorators = "ExoticStatistics"),
    "exponential" = distr6::Exponential$new(scale = exp(location),
      decorators = "ExoticStatistics"),
    "gaussian" = distr6::Normal$new(mean = location, sd = scale,
      decorators = "ExoticStatistics"),
    "lognormal" = distr6::Lognormal$new(meanlog = location, sdlog = scale,
      decorators = "ExoticStatistics"),
    "loglogistic" = distr6::Loglogistic$new(scale = exp(location),
      shape = 1 / scale,
      decorators = "ExoticStatistics")
  )

  return(structure(
    list(model = fit, basedist = basedist),
    name = "Parametric survival model",
    class = c("parametric", "survivalmodel")
  ))
}

#' @title Predict method for Parametric Model
#'
#' @description Predicted values from a fitted Parametric survival model.
#'
#' @details
#'
#' @template return_predict
#'
#' @param object (`parametric(1)`)\cr
#' Object of class inheriting from `"parametric"`.
#' @param newdata `(data.frame(1))`\cr
#' Testing data of `data.frame` like object, internally is coerced with [stats::model.matrix()].
#' If missing then training data from fitted object is used.
#' @param times `(numeric())`\cr
#' Times at which to evaluate the estimator. If `NULL` (default) then evaluated at all unique times
#' in the training set.
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
#'
#' @examples
#' if (requireNamespaces(c("distr6", "survival"))) {
#'
#' library(survival)
#'
#' train <- 1:10
#' test <- 11:20
#' fit <- parametric(Surv(time, status) ~ ., data = rats[train, ])
#' predict(fit, newdata = rats[test, ])
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
predict.parametric <- function(object, newdata,
  form = c("aft", "ph", "tobit", "po"), times = NULL,
  return_method = c("continuous", "discrete"),
  type = c("survival", "risk", "all"), distr6 = FALSE,
  ntime = 150, round_time = 2, ...) {

  return_method <- match.arg(return_method)
  form <- match.arg(form)
  type <- match.arg(type)

  unique_times <- sort(unique(object$model$y[, 1, drop = FALSE]))
  if (!is.logical(round_time) || round_time) {
    unique_times <- unique(round(unique_times, round_time))
  }
  # using same method as in ranger
  unique_times <- unique_times[
    unique(round(seq.int(1, length(unique_times), length.out = ntime)))
  ]

  truth <- object$model$y
  newdata <- clean_test_data(object$model, newdata)

  if (is.null(times)) {
    predict_times <- unique_times
  } else {
    predict_times <- sort(unique(times))
  }

  basedist <- object$basedist
  fit <- object$model
  lp <- matrix(fit$coefficients[-1], nrow = 1) %*% t(newdata)

  if (return_method == "discrete") {
    surv <- .predict_survreg_discrete(object, newdata, form, predict_times,
      basedist, fit, lp)
  } else {
    surv <- .predict_survreg_continuous(object, newdata, form,
      basedist, fit, lp)
  }

  ret <- list()

  if (type %in% c("risk", "all")) {
    ret$risk <- -surv$lp
  }

  if (type %in% c("survival", "all")) {
    if (distr6 || return_method == "continuous") {
      ret$surv <- surv$distr
    } else {
      ret$surv <- 1 - distr6::gprm(surv$distr, "cdf")
    }
  }

  if (length(ret) == 1) {
    return(ret[[1]])
  } else {
    return(ret)
  }
}

.predict_survreg_continuous <- function(object, newdata, form, basedist,
  fit, lp) {

  # checks and parameterises the chosen model form: proportional hazard (ph), accelerated failure
  # time (aft), odds.
  # PH: h(t) = h0(t)exp(lp)
  # AFT: h(t) = exp(-lp)h0(t/exp(lp))
  # PO: h(t)/h0(t) = {1 + (exp(lp)-1)S0(t)}^-1

  dist <- toproper(fit$dist)

  if (form == "tobit") {
    name = paste(dist, "Tobit Model")
    short_name = paste0(dist, "Tobit")
    description = paste(dist, "Tobit Model with negative log-likelihood",
      -fit$loglik[2])
  } else if (form == "ph") {
    name = paste(dist, "Proportional Hazards Model")
    short_name = paste0(dist, "PH")
    description = paste(dist, "Proportional Hazards Model with negative log-likelihood",
      -fit$loglik[2])
  } else if (form == "aft") {
    name = paste(dist, "Accelerated Failure Time Model")
    short_name = paste0(dist, "AFT")
    description = paste(dist, "Accelerated Failure Time Model with negative log-likelihood",
      -fit$loglik[2])
  } else if (form == "po") {
    name = paste(dist, "Proportional Odds Model")
    short_name = paste0(dist, "PO")
    description = paste(dist, "Proportional Odds Model with negative log-likelihood",
      -fit$loglik[2])
  }

  params = list(list(name = name,
    short_name = short_name,
    type = set6::PosReals$new(),
    support = set6::PosReals$new(),
    valueSupport = "continuous",
    variateForm = "univariate",
    description = description,
    .suppressChecks = TRUE,
    pdf = function() {
    },
    cdf = function() {
    },
    parameters = param6::pset()
  ))

  params = rep(params, length(lp))

  pdf = function(x) {} # nolint
  cdf = function(x) {} # nolint
  quantile = function(p) {} # nolint

  if (form == "tobit") {
    for (i in seq_along(lp)) {
      body(pdf) = substitute(pnorm((x - y) / scale), list(
        y = lp[i] + fit$coefficients[1],
        scale = basedist$stdev()
      ))
      body(cdf) = substitute(pnorm((x - y) / scale), list(
        y = lp[i] + fit$coefficients[1],
        scale = basedist$stdev()
      ))
      body(quantile) = substitute(qnorm(p) * scale + y, list(
        y = lp[i] + fit$coefficients[1],
        scale = basedist$stdev()
      ))
      params[[i]]$pdf = pdf
      params[[i]]$cdf = cdf
      params[[i]]$quantile = quantile
    }
  } else if (form == "ph") {
    for (i in seq_along(lp)) {
      body(pdf) = substitute((exp(y) * basedist$hazard(x)) * (1 - self$cdf(x)), list(y = -lp[i]))
      body(cdf) = substitute(1 - (basedist$survival(x)^exp(y)), list(y = -lp[i]))
      body(quantile) = substitute(
        basedist$quantile(1 - exp(exp(-y) * log(1 - p))), # nolint
        list(y = -lp[i])
      )
      params[[i]]$pdf = pdf
      params[[i]]$cdf = cdf
      params[[i]]$quantile = quantile
    }
  } else if (form == "aft") {
    for (i in seq_along(lp)) {
      body(pdf) = substitute((exp(-y) * basedist$hazard(x / exp(y))) * (1 - self$cdf(x)),
        list(y = lp[i]))
      body(cdf) = substitute(1 - (basedist$survival(x / exp(y))), list(y = lp[i]))
      body(quantile) = substitute(exp(y) * basedist$quantile(p), list(y = lp[i]))
      params[[i]]$pdf = pdf
      params[[i]]$cdf = cdf
      params[[i]]$quantile = quantile
    }
  } else if (form == "po") {
    for (i in seq_along(lp)) {
      body(pdf) = substitute((basedist$hazard(x) *
        (1 - (basedist$survival(x) /
          (((exp(y) - 1)^-1) + basedist$survival(x))))) *
        (1 - self$cdf(x)), list(y = lp[i]))
      body(cdf) = substitute(1 - (basedist$survival(x) *
        (exp(-y) + (1 - exp(-y)) * basedist$survival(x))^-1), # nolint
      list(y = lp[i]))
      body(quantile) = substitute(basedist$quantile(-p / ((exp(-y) * (p - 1)) - p)), # nolint
        list(y = lp[i]))
      params[[i]]$pdf = pdf
      params[[i]]$cdf = cdf
      params[[i]]$quantile = quantile
    }
  }

  distlist = lapply(params, function(.x) do.call(distr6::Distribution$new, .x))
  names(distlist) = paste0(short_name, seq_along(distlist))

  distr = distr6::VectorDistribution$new(distlist,
    decorators = c("CoreStatistics", "ExoticStatistics"))

  lp = lp + fit$coefficients[1]

  list(lp = as.numeric(lp), distr = distr)
}

.predict_survreg_discrete <- function(object, newdata, form, predict_times,
  basedist, fit, lp) {

  # PH: h(t) = h0(t)exp(lp)
  # AFT: h(t) = exp(-lp)h0(t/exp(lp))
  # PO: h(t)/h0(t) = {1 + (exp(lp)-1)S0(t)}^-1
  if (form == "tobit") {
    fun = function(y) stats::pnorm((predict_times - y - fit$coefficients[1]) / basedist$stdev())
  } else if (form == "ph") {
    fun = function(y) 1 - (basedist$survival(predict_times)^exp(-y))
  } else if (form == "aft") {
    fun = function(y) 1 - (basedist$survival(predict_times / exp(y)))
  } else if (form == "po") {
    fun = function(y) {
      surv = basedist$survival(predict_times)
      1 - (surv * (exp(-y) + (1 - exp(-y)) * surv)^-1)
    }
  }

  # linear predictor defined by the fitted cofficients multiplied by the model matrix
  # (i.e. covariates)

  if (length(predict_times) == 1) { # edge case
    mat <- as.matrix(vapply(lp, fun, numeric(1)), ncol = 1)
  } else {
    mat <- t(vapply(lp, fun, numeric(length(predict_times))))
  }
  colnames(mat) <- predict_times

  list(
    lp = as.numeric(lp + fit$coefficients[1]),
    distr = distr6::as.Distribution(mat, fun = "cdf")
  )
}