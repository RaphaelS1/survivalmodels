setcollapse <- function(x) {
  paste0("{", paste0(x, collapse = ", "), "}")
}

#' @title Vectorised Logical requireNamespace
#' @description Vectorises the [requireNamespace] function and returns `TRUE` if all packages, `x`,
#' are available and `FALSE` otherwise.
#' @param x `(character())` \cr string naming the packages/name spaces to load.
#' @examples
#' requireNamespaces("survivalmodels")
#' requireNamespaces(c("survivalmodels", "distr6"))
#'
#' # test for some fake package
#' requireNamespaces(c("survivalmodels", "distr6", "111"))
#'
#' @export
requireNamespaces <- function(x) {
  all(vapply(x, requireNamespace, logical(1), quietly = TRUE))
}
