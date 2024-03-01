if (!requireNamespace("distr6", quietly = TRUE)) {
  skip("distr6 not installed.")
}

test_that("silent", {
  expect_error(parametric(Surv(time, status) ~ .))
  expect_silent(parametric(Surv(time, status) ~ ., data = rats[1:10, ]))
  expect_error(parametric(x = "litter"), "Both 'x' and 'y'")
  expect_error(parametric(time_variable = "time"), "'time_variable'")
  expect_error(parametric(
    x = rats[, c("rx", "litter")],
    y = rats$time), "is not TRUE")
  expect_error(parametric(
    x = rats$rx,
    y = Surv(rats$time, rats$status)
  ), "data.frame")
})

test_that("auto sanity", {
  sanity_check(model = "akritas",
               pars = list())
})

test_that("confirm lp and risk directions the same", {
  train <- sample(100, 50)
  test <- setdiff(seq(100), train)

  for (form in c("aft", "ph", "po", "tobit")) {
    fit <- parametric(Surv(time, status) ~ ., data = rats[train, ])
    pred <- predict(fit,
      newdata = rats[test, ], type = "all",
      return_method = "discrete", form = form
    )
    expect_true(all.equal(order(surv_to_risk(pred$surv)), order(pred$risk)))
  }
})
