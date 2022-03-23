library(survival)

# copied from https://rstudio.github.io/reticulate/articles/package.html
skip_if_no_pycox <- function() {
  if (!reticulate::py_module_available("torch") ||
      !reticulate::py_module_available("pycox") ||
      !reticulate::py_module_available("numpy"))
    skip("One of torch, numpy, pycox not available for testing.")
}

sanity_check <- function(model, pars) {
  skip_if_not_installed("distr6")

  set.seed(42)
  if (model != "akritas") {
    set_seed(42)
  }

  train <- simsurvdata(500, cens = 0.1)
  test <- simsurvdata(50, cens = 0.1)

  y <- survival::Surv(test$time, test$status)

  fit <- do.call(
    get(model),
    c(list(formula = Surv(time, status) ~ ., data = train), pars)
  )

  p <- predict(fit, newdata = test, type = "all", distr6 = TRUE)

  print(cindex(p$risk, y))
  expect_true(cindex(p$risk, y) >= 0.4)
  expect_equal(length(p$risk), nrow(distr6::gprm(p$surv, "cdf")))

  p <- predict(fit, newdata = test, type = "all", distr6 = FALSE)
  expect_equal(length(p$risk), nrow(p$surv))
}

expect_rounded_equal <- function(actual, expected, n = 4) {
  expect_equal(round(actual, n), round(expected, n))
}
