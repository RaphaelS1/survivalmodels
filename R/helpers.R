setcollapse <- function(x) {
  paste0("{", paste0(x, collapse = ", "), "}")
}

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
#' @param seed_R (`integer(1)`) `seed` passed to [set.seed].
#' @param seed_np (`integer(1)`) `seed` passed to `numpy$random$seed`. Default is same as `seed_R`.
#' @param seed_torch (`integer(1)`) `seed` passed to `numpy$random$seed`.
#' Default is same as `seed_R`.
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
