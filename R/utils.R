setcollapse <- function(x) {
  paste0("{", paste0(x, collapse = ", "), "}")
}

assert_surv_matrix <- function(x) {
  if (!all(x <= 1 & x >= 0)) {
    stop("Survival probabilities, x, must be 0 <= x <= 1")
  }
  if (is.null(colnames(x)) ||
      !identical(order(as.numeric(colnames(x))), seq(ncol(x)))) {
    stop("Survival matrix column names must be increasing numeric")
  }
  apply(1 - x, 1, function(.x) {
    if (!identical(order(.x), seq(ncol(x)))) {
      stop("Survival probabilities must be (non-strictly) decreasing")
    }
  })

  invisible(NULL)
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