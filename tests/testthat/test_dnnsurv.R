test_that("silent", {
  expect_silent({fit <- dnnsurv(Surv(time, status) ~ ., data = rats[1:50, ], verbose = FALSE)})
  expect_silent({predict(fit, newdata = rats[51:100, ])})
})

test_that("custom", {
  expect_silent(dnnsurv(Surv(time, status) ~ ., data = rats[1:10, 1:4], verbose = FALSE,
                        cuts = 3,
                        custom_model = build_keras_net(5L, 1L)))
})

test_that("early stopping",{
  expect_silent(dnnsurv(Surv(time, status) ~ ., data = rats[1:10, 1:4], verbose = FALSE,
                        early_stopping = TRUE, validation_split = 0.3))
})

test_that("predict", {
  fit <- dnnsurv(Surv(time, status) ~ ., data = rats[1:50, ], verbose = FALSE)
  p <- predict(fit, type = "all", distr6 = FALSE)
  expect_is(p, "list")
  expect_is(p$surv, "matrix")
  expect_is(p$risk, "numeric")
  p <- predict(fit, type = "all", distr6 = TRUE)
  expect_is(p, "list")
  expect_is(p$surv, "VectorDistribution")
  p <- predict(fit, type = "survival")
  expect_is(p, "matrix")
})
