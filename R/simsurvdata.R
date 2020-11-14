#' @title Simulate Survival Data
#' @description Function for simulating survival data.
#' @details Currently limited to three covariates, Weibull survival times, and Type I censoring.
#' This will be expanded to a flexible simulation function in future updates. For now the function
#' is primarily limited to helping function examples.
#' @param n `(integer(1))` \cr Number of samples
#' @param trt,age,sex `(numeric(1))` \cr Coefficients for covariates.
#' @param cutoff `(numeric(1))` \cr Deprecated, in future use `cens`.
#' @param cens `(numeric(1))` \cr Proportion of censoring to be generated, cut-off time is then
#' selected as the quantile that results in `cens`.
#'
#' @return
#' [data.frame()]
#'
#' @examples
#' simsurvdata()
#'
#' @export
simsurvdata <- function(n = 100, trt = 2, age = 2, sex = 1.5, cutoff = NULL, cens = 0.3) {

  if (!is.null(cutoff)) {
    stop("`cutoff` is now deprecated, in the future please use `cens`.")
  }

  covs <- data.frame(
    sexF = stats::rbinom(n, 1, 0.5),
    age = round(stats::runif(n, 20, 50)),
    trt = stats::rbinom(n, 1, 0.7)
  )

  scale <- (covs$trt + 1) * trt + (covs$sex + 1) * sex
  shape <- age * covs$age

  time <- stats::rweibull(nrow(covs), shape = shape, scale = scale)

  if (is.null(cutoff)) {
    cutoff <- as.numeric(stats::quantile(time, 1 - cens))
  }

  status <- as.integer(time <= cutoff)
  time[time > cutoff] <- cutoff
  time <- round(time, 3)

  return(data.frame(covs, time, status))
}
