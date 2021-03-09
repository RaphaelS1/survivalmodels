#' @title Compute Concordance of survivalmodel Risk
#' @description A thin wrapper around [survival::concordance] which essentially
#' just sets `reverse = TRUE`.
#' @param risk (`numeric()`) \cr
#' Vector of risk predictions from a `survivalmodel` model
#' (so high risk implies low survival time prediction).
#' @param truth (`numeric()`) \cr
#' Vector of true survival times, must be same length as `risk`.
#' @param ... (`ANY`) \cr
#' Further parameters passed to [survival::concordance].
#' @examples
#' if (!requireNamespace("survival", quietly = TRUE)) {
#'   set.seed(10)
#'   data <- simsurvdata(20)
#'   fit <- deepsurv(data = data[1:10, ])
#'   p <- predict(fit, type = "risk", newdata = data[11:20, ])
#'   concordance(risk = p, truth = data[11:20, "time"])
#' }
#' @export
cindex <- function(risk, truth, ...) {
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("concordance requires {survival} package")
  }
  stopifnot(length(risk) == length(truth))
  survival::concordance(truth ~ risk, reverse = TRUE, ...)$concordance
}
