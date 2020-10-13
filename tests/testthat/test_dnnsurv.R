skip_on_os("windows")
skip_if_no_keras()

test_that("silent", {
  expect_silent({fit <- dnnsurv(Surv(time, status) ~ ., data = rats[1:50, ], verbose = FALSE,
                                validation_split = 0.3)})
  expect_silent({predict(fit, newdata = rats[51:100, ])})
})

test_that("errors", {
  expect_error(dnnsurv(Surv(time, status) ~ ., data = rats[1:50, ], cuts = NULL, cutpoints = NULL),
               "One of")
})

test_that("custom", {
  expect_silent(dnnsurv(Surv(time, status) ~ ., data = rats[1:10, 1:4], verbose = FALSE,
                        cuts = 3,
                        custom_model = build_keras_net(5L, 1L)))
})

test_that("early stopping", {
  expect_silent(dnnsurv(Surv(time, status) ~ ., data = rats[1:10, 1:4], verbose = FALSE,
                        early_stopping = TRUE, validation_split = 0.3))
})

test_that("predict", {
  fit <- dnnsurv(Surv(time, status) ~ ., data = rats[1:50, ], verbose = FALSE,
                 cuts = 5)
  p <- predict(fit, type = "all", distr6 = FALSE)
  expect_is(p, "list")
  expect_is(p$surv, "matrix")
  expect_is(p$risk, "numeric")
  expect_equal(length(p$risk), 50)
  expect_equal(dim(p$surv), c(50, 5))
  p <- predict(fit, type = "all", distr6 = TRUE)
  expect_is(p, "list")
  expect_is(p$surv, "VectorDistribution")
  expect_equal(p$surv$properties$support$power, 50)
  p <- predict(fit, type = "survival")
  expect_is(p, "matrix")
})