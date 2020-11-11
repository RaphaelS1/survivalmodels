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
