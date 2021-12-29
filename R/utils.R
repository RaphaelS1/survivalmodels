setcollapse <- function(x) {
  paste0("{", paste0(x, collapse = ", "), "}")
}


surv_to_risk <- function(x) {
  assert_cdf_matrix(1 - x)
  cumH <- -log(x)
  cumH[cumH == Inf] <- 0
  rowSums(cumH)
}


assert_cdf_matrix <- function(x) {
  stopifnot(!is.null(colnames(x)))
  stopifnot(inherits(x, "matrix"))
  apply(x, 1, function(.x) stopifnot(identical(order(.x), seq(ncol(x)))))
}
