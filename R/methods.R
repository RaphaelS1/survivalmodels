#' @export
print.survivalmodel <- function(x, ...) {
  cat("\n", attr(x, "name"), "\n\n")
  cat("Call:\n ", deparse(x$call))
  if (is.null(x[["y"]])) {
    ynames <- x$ynames
  } else {
    ynames <- colnames(x$y)
  }
  cat("\n\nResponse:\n  Surv(", paste0(ynames, collapse = ", "), ")\n", sep = "")
  cat("Features:\n ", setcollapse(x$xnames), "\n")
}

#' @export
summary.survivalmodel <- function(object, ...) {
  print.survivalmodel(object, ...)
}
