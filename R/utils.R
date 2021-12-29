setcollapse <- function(x) {
  paste0("{", paste0(x, collapse = ", "), "}")
}


surv_to_risk <- function(x, method = 1) {
  if (method == 1) {
    assert_surv_matrix(x)
    cumH <- -log(x)
    cumH[cumH == Inf] <- 0
    rowSums(cumH)
  } else {
    assert_surv_matrix(x)
    -apply(1 - x, 1,
            function(.x) sum(c(.x[1], diff(.x)) * as.numeric(colnames(x))))
  }

}


assert_surv_matrix <- function(x) {
  stopifnot(!is.null(colnames(x)))
  stopifnot(inherits(x, "matrix"))
  apply(1 - x, 1, function(.x) stopifnot(identical(order(.x), seq(ncol(x)))))
}
