library(survival)

# copied from https://rstudio.github.io/reticulate/articles/package.html
skip_if_no_pycox <- function() {
  if (!reticulate::py_module_available("torch") || !reticulate::py_module_available("pycox") ||
      !reticulate::py_module_available("numpy"))
    skip("One of torch, numpy, pycox not available for testing.")
}

skip_if_no_keras <- function() {
  if (!requireNamespace("keras", quietly = TRUE))
    skip("keras not available for testing.")
}
