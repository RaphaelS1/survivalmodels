skip_on_os("windows")

test_that("silent", {
  expect_silent({fit <- deepsurv(Surv(time, status) ~ ., data = rats[1:50, ], verbose = FALSE,
                                 frac = 0.3)})
  expect_silent({predict(fit, newdata = rats[51:100, ])})
})
