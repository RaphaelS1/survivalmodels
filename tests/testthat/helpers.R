library(survival)

# copied from https://rstudio.github.io/reticulate/articles/package.html
skip_if_no_pycox <- function() {
  if (!reticulate::py_module_available("torch") || !reticulate::py_module_available("pycox") ||
      !reticulate::py_module_available("numpy"))
    skip("One of torch, numpy, pycox not available for testing.")
}

sanity_check <- function(model, pars) {
  skip_if_not_installed("distr6")

  set.seed(42)
  if (model != "akritas") {
    np <- reticulate::import("numpy")
    np$random$seed(1L)
    torch <- reticulate::import("torch")
    torch$manual_seed(1L)
  }

  train <- simsurvdata(200, cens = 0.2)
  test <- simsurvdata(50, cens = 0.2)

  y <- survival::Surv(test$time, test$status)

  fit <- do.call(get(model), c(list(formula = Surv(time, status) ~ ., data = train), pars))
  p <- predict(fit, newdata = test, type = "all", distr6 = TRUE)

  expect_true(survival::concordance(y ~ p$risk, reverse = TRUE)$concordance >= 0.5)
  expect_equal(length(p$risk), nrow(p$surv$modelTable))

  p <- predict(fit, newdata = test, type = "all", distr6 = FALSE)
  expect_equal(length(p$risk), nrow(p$surv))
}
