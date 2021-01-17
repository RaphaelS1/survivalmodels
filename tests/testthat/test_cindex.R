skip_if_not_installed("survival")

test_that("cindex", {
  set.seed(10)
  data <- simsurvdata(20)
  fit <- deepsurv(data = data[1:10, ])
  p <- predict(fit, type = "risk", newdata = data[11:20, ])
  expect_true(cindex(risk = p, truth = data[11:20, "time"]) > 0.5)
})
