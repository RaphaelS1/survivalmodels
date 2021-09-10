setcollapse <- function(x) {
  paste0("{", paste0(x, collapse = ", "), "}")
}


surv_to_risk <- function(x) {
  assert_cdf_matrix(x)
  -apply(x, 1, function(.x) sum(c(.x[1], diff(.x)) * as.numeric(colnames(x))))
}


assert_cdf_matrix <- function(x) {
  stopifnot(!is.null(colnames(x)))
  stopifnot(inherits(x, "matrix"))
  apply(x, 1, function(.x) stopifnot(identical(order(.x), seq(ncol(x)))))
}