skip_on_os("windows")

test_that("silent", {
  expect_message({fit <- coxtime(Surv(time, status) ~ ., data = rats[1:50, ], verbose = FALSE,
                                 frac = 0.3)})
  expect_silent({predict(fit, newdata = rats[51:100, ])})
})

test_that("standardize", {
  expect_silent(coxtime(Surv(time, status) ~ ., data = rats[1:50, ], verbose = FALSE,
                        standardize_time = TRUE))
})
