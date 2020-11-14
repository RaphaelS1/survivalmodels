if (!requireNamespace("distr6", quietly = TRUE)) {
  skip("distr6 not installed.")
}

test_that("silent", {
  expect_error(akritas(Surv(time, status) ~ .))
  expect_silent(akritas(Surv(time, status) ~ ., data = rats[1:10, ]))
  expect_error(akritas(x = "litter"), "Both 'x' and 'y'")
  expect_error(akritas(time_variable = "time"), "'time_variable'")
  expect_error(akritas(
    x = rats[, c("rx", "litter")],
    y = rats$time), "is not TRUE")
  expect_error(akritas(
    x = rats$rx,
    y = Surv(rats$time, rats$status)
  ), "data.frame")
})

test_that("kaplan", {
  fit <- akritas(Surv(time, status) ~ ., data = rats[1:100, ])
  expect_equal(
    as.numeric(predict(fit, newdata = rats[1:100, ], lambda = 1)[1, ]),
    survival::survfit(Surv(time, status) ~ 1, data = rats[1:100, ])$surv)

  fit <- akritas(Surv(time, status) ~ ., data = rats[1:100, ], reverse = TRUE)
  expect_equal(
    as.numeric(predict(fit, newdata = rats[1:100, ], lambda = 1)[1, ]),
    survival::survfit(Surv(time, 1 - status) ~ 1, data = rats[1:100, ])$surv)
})

test_that("univariate", {
  fit <- akritas(Surv(time, status) ~ ., data = rats[1:100, 3:5])
  expect_silent(predict(fit, rats))
})

test_that("cpp", {
  fit <- akritas(Surv(time, status) ~ ., data = rats[1:10, ], lambda = 0.5)
  p <- predict(fit, rats, type = "all")
  expect_is(p, "list")
  expect_length(p, 2)
  expect_is(p$surv, "matrix")
  p <- predict(fit, rats, type = "all", distr6 = TRUE)
  expect_is(p$surv, "VectorDistribution")
  expect_silent(predict(fit, rats, times = 100))
})

test_that("auto sanity", {
  sanity_check(model = "akritas",
               pars = list())
})
