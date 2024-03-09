#' @title Fully Parametric Survival Model
#' @name parametric
#'
#' @description
#' Fit/predict implementation of [survival::survreg()], which can return
#' absolutely continuous distribution predictions using \pkg{distr6}.
#'
#' @param eps `(numeric(1))` \cr
#' Used when the fitted `scale` parameter is too small. Default `1e-15`.
#' @param ... `ANY` \cr
#' Additional arguments passed to [survival::survreg()].
#'
#' @template param_traindata
#'
#' @return An object inheriting from class `parametric`.
#'
#' @examples
#' if (requireNamespaces(c("distr6", "survival"))) {
#'  library(survival)
#'  parametric(Surv(time, status) ~ ., data = simsurvdata(10))
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

  fit <- survival::survreg(survival::Surv(data$y) ~ data$x, x = TRUE, ...)

  location <- as.numeric(fit$coefficients[1])

  if (is.na(location)) {
    stop("Failed to fit survreg, coefficients all NA")
  }

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
    "weibull" = distr6::Weibull$new(
      shape = 1 / scale, scale = exp(location),
      decorators = "ExoticStatistics"
    ),
    "exponential" = distr6::Exponential$new(
      scale = exp(location),
      decorators = "ExoticStatistics"
    ),
    "gaussian" = distr6::Normal$new(
      mean = location, sd = scale,
      decorators = "ExoticStatistics"
    ),
    "lognormal" = distr6::Lognormal$new(
      meanlog = location, sdlog = scale,
      decorators = "ExoticStatistics"
    ),
    "loglogistic" = distr6::Loglogistic$new(
      scale = exp(location),
      shape = 1 / scale,
      decorators = "ExoticStatistics"
    )
  )

  return(structure(
    list(
      model = fit,
      basedist = basedist,
      call = call,
      xnames = colnames(fit$x),
      ynames = unique(colnames(fit$y))
    ),
    name = "Parametric survival model",
    class = c("parametric", "survivalmodel")
  ))
}

#' @title Predict method for Parametric Model
#'
#' @description Predicted values from a fitted Parametric survival model.
#'
#' @details
#' The `form` parameter determines how the distribution is created.
#' Options are:
#'
#' - Accelerated failure time (`"aft"`) \deqn{h(t) = h_0(\frac{t}{exp(lp)})exp(-lp)}
#' - Proportional Hazards (`"ph"`) \deqn{h(t) = h_0(t)exp(lp)}
#' - Tobit (`"tobit"`) \deqn{h(t) = \Phi(\frac{t - lp}{scale})}
#' - Proportional odds (`"po"`) \deqn{h(t) = \frac{h_0(t)}{1 + (exp(lp)-1)S_0(t)}}
#'
#' where \eqn{h_0,S_0} are the estimated baseline hazard and survival functions
#' (in this case with a given parametric form), \eqn{lp} is the predicted linear
#' predictor calculated using the formula \eqn{lp = \hat{\beta} X_{new}} where
#' \eqn{X_{new}} are the variables in the test data set and \eqn{\hat{\beta}}
#' are the coefficients from the fitted parametric survival model (`object`).
#' \eqn{\Phi} is the cdf of a N(0, 1) distribution, and \eqn{scale} is the
#' fitted scale parameter.
#'
#' @param object (`parametric(1)`)\cr
#' Object of class inheriting from `"parametric"`.
#' @param newdata `(data.frame(1))`\cr
#' Testing data of `data.frame` like object, internally is coerced with [stats::model.matrix()].
#' If missing then training data from fitted object is used.
#' @param form `(character(1))` \cr
#' The form of the predicted distribution, see `details` for options.
#' @param times `(numeric())`\cr
#' Times at which to evaluate the estimator. If `NULL` (default) then evaluated at all unique times
#' in the training set.
#' @param type (`character(1)`)\cr
#' Type of predicted value. Choices are survival probabilities over all time-points in training
#' data (`"survival"`) or a relative risk ranking (`"risk"`), which is the sum of the predicted
#' cumulative hazard function so higher rank implies higher risk of event, or both (`"all"`).
#' @param distr6 (`logical(1)`)\cr
#' If `FALSE` (default) and `type` is `"survival"` or `"all"` returns matrix of survival
#' probabilities, otherwise returns a [distr6::Distribution()].
#' @param ntime `(numeric(1))`\cr
#' Number of unique time-points in the training set, default is 150.
#' @param round_time `(numeric(1))`\cr
#' Number of decimal places to round time-points to, default is 2, set to `FALSE` for no rounding.
#' @param ... `ANY` \cr
#' Currently ignored.
#'
#' @return A `numeric` if `type = "risk"`, a [distr6::Distribution()]
#' (if `distr6 = TRUE`) and `type = "survival"`; a `matrix` if
#' (`distr6 = FALSE`) and `type = "survival"` where entries are survival
#' probabilities with rows of observations and columns are time-points;
#' or a list combining above if `type = "all"`.
#'
#' @examples
#' if (requireNamespaces(c("distr6", "survival"))) {
#'   library(survival)
#'
#'   set.seed(42)
#'   train <- simsurvdata(10)
#'   test <- simsurvdata(5)
#'   fit <- parametric(Surv(time, status) ~ ., data = train)
#'
#'   # Return a discrete distribution survival matrix
#'   predict_distr <- predict(fit, newdata = test)
#'   predict_distr
#'
#'   # Return a relative risk ranking with type = "risk"
#'   predict(fit, newdata = test, type = "risk")
#'
#'   # Or survival probabilities and a rank
#'   predict(fit, newdata = test, type = "all", distr6 = TRUE)
#' }
#' @export
predict.parametric <- function(object, newdata,
  form = c("aft", "ph", "tobit", "po"), times = NULL,
  type = c("survival", "risk", "all"), distr6 = FALSE,
  ntime = 150, round_time = 2, ...) {

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

  if (type %in% c("survival", "all") && distr6) {
    surv <- .predict_survreg_continuous(object, newdata, form,
      basedist, fit, lp)
  } else {
    surv <- .predict_survreg_discrete(object, newdata, form,
      predict_times, basedist, fit, lp)
  }

  ret <- list()

  if (type %in% c("risk", "all")) {
    ret$risk <- -surv$lp
  }

  if (type %in% c("survival", "all")) {
    ret$surv <- surv$distr
  }

  if (length(ret) == 1) {
    return(ret[[1]])
  } else {
    return(ret)
  }
}

.predict_survreg_continuous <- function(object, newdata, form, basedist,
  fit, lp) {

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

  if (length(predict_times) == 1) { # edge case
    mat <- as.matrix(vapply(lp, fun, numeric(1)), ncol = 1)
  } else {
    mat <- t(vapply(lp, fun, numeric(length(predict_times))))
  }
  colnames(mat) <- predict_times

  list(
    lp = as.numeric(lp + fit$coefficients[1]),
    distr = 1 - mat
  )
}
