#' @export
print.survivalmodel <- function(x, ...) {
  cat("\n", attr(x, "name"), "\n\n")
  cat("Call:\n ", deparse(x$call))
  cat("\n\nResponse:\n  Surv(", paste0(colnames(x$y), collapse = ", "), ")\n", sep = "")
  cat("Features:\n ", setcollapse(x$xnames), "\n")
}

#' @export
summary.survivalmodel <- function(object, ...) {
  print.survivalmodel(object, ...)
}
