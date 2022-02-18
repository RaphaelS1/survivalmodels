setcollapse <- function(x) {
  paste0("{", paste0(x, collapse = ", "), "}")
}


surv_to_risk <- function(x) {
  assert_surv_matrix(x)
  cumH <- -log(x)
  cumH[cumH == Inf] <- 0
  rowSums(cumH)
}


assert_surv_matrix <- function(x) {
  stopifnot(all(x <= 1 & x >= 0))
  stopifnot(!is.null(colnames(x)))
  stopifnot(inherits(x, "matrix"))
  apply(1 - x, 1, function(.x) stopifnot(identical(order(.x), seq(ncol(x)))))
}

fill_na <- function(x, along = 1) {
  t(apply(x, along, function(.x) {
    if (!all(is.na(.x))) {
      .x[is.na(.x)] <- .x[!is.na(.x)][findInterval(
        which(is.na(.x)),
        which(!is.na(.x))
      )]
    }
    .x
  }))
}