setcollapse <- function(x) {
  paste0("{", paste0(x, collapse = ", "), "}")
}

assert_surv_matrix <- function(x) {

  if (is.null(colnames(x)) ||
      !identical(order(as.numeric(colnames(x))), seq(ncol(x)))) {
    stop("Survival matrix column names must be increasing numeric")
  }

  if (!C_assert_increasing_surv(x)) {
    stop("Survival probabilities must be (non-strictly) decreasing and between [0, 1]")
  }

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

toproper <- function(str) {
  paste0(toupper(substr(str, 1, 1)), substr(str, 2, 100))
}
