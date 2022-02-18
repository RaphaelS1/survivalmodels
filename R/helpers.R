#' @title Vectorised Logical requireNamespace
#' @description Helper function for internal use. Vectorises the [requireNamespace] function and
#' returns `TRUE` if all packages, `x`, are available and `FALSE` otherwise.
#' @param x `(character())` \cr string naming the packages/name spaces to load.
#' @export
requireNamespaces <- function(x) {
  all(vapply(x, requireNamespace, logical(1), quietly = TRUE))
}

#' @title Set seed in R numpy and torch
#' @description To ensure consistent results, a seed has to be set in R
#' using [set.seed] as usual but also in {numpy} and {torch} via {reticulate}.
#' Therefore this function simplifies the process into one funciton.
#' @param seed_R (`integer(1)`) \cr `seed` passed to [set.seed].
#' @param seed_np (`integer(1)`) \cr `seed` passed to `numpy$random$seed`. Default is same as `seed_R`.
#' @param seed_torch (`integer(1)`) \cr `seed` passed to `numpy$random$seed`.
#' Default is same as `seed_R`.
#' @export
set_seed <- function(seed_R, seed_np = seed_R, seed_torch = seed_R) {
  set.seed(seed_R)
  if (reticulate::py_module_available("numpy")) {
    np <- reticulate::import("numpy")
    np$random$seed(as.integer(seed_np))
  }
  if (reticulate::py_module_available("torch")) {
    torch <- reticulate::import("torch")
    torch$manual_seed(as.integer(seed_torch))
  }
  invisible(NULL)
}

#' @title Safely convert a survival matrix prediction to a relative risk
#' @description Many methods can be used to reduce a discrete survival
#' distribution prediction (i.e. matrix) to a relative risk / ranking
#' prediction. Here we define the predicted relative risk as the sum of
#' the predicted cumulative hazard function - which can be loosely interpreted
#' as the expected number of deaths for patients with similar characteristics.
#' @param x (`matrix()`) \cr TxN survival matrix prediction where T is number
#' of time-points and N is number of predicted observations. Colum names
#' correspond to predicted time-points and should therefore be coercable to
#' numeric and increasing. Entries are survival predictions and should
#' be (non-strictly) decreasing in each row.
#' @references
#' Sonabend, R., Bender, A., & Vollmer, S. (2021).
#' Evaluation of survival distribution predictions with discrimination
#' measures. http://arxiv.org/abs/2112.04828.
#' @export
surv_to_risk <- function(x) {
  assert_surv_matrix(x)
  cumH <- -log(x)
  cumH[cumH == Inf] <- 0
  rowSums(cumH)
}
