#' @param formula `(formula(1))`\cr
#' Object specifying the model fit, left-hand-side of formula should describe a [survival::Surv()]
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
