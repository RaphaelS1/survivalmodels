if (!requireNamespace("distr6", quietly = TRUE)) {
  skip("distr6 not installed.")
}

test_that("silent", {
  expect_error(parametric(Surv(time, status) ~ .))
  expect_silent(parametric(Surv(time, status) ~ ., data = rats[1:10, ]))
  fit <- parametric(Surv(time, status) ~ ., data = rats[1:10, ])
  expect_equal(predict(fit, return_method = "discrete"),
    predict(fit, rats[1:10, ], return_method = "discrete"))
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

form_opts <- c("aft", "ph", "po", "tobit")

test_that("confirm lp and risk directions the same", {

  for (form in form_opts) {
    fit <- parametric(Surv(time, status) ~ ., data = rats)
    pred <- predict(fit,
      newdata = rats, type = "all",
      return_method = "discrete", form = form
    )
    expect_true(all.equal(order(surv_to_risk(pred$surv)), order(pred$risk)))
  }
})

test_that("manualtest - aft", {
  df = simsurvdata(50)
  fit = parametric(Surv(time, status) ~ ., df, dist = "weibull")
  p = predict(fit, df, type = "all")

  expect_equal(-p$risk, unname(predict(fit$model, type = "lp")))
  expect_equal(p$surv[1]$survival(predict(
    fit$model, type = "quantile", p = c(0.2, 0.8)
  )[1, ]), c(0.8, 0.2))
  expect_equal(p$surv[10]$cdf(predict(
    fit$model, type = "quantile", p = seq.int(0, 1, 0.1)
  )[10, ]),
  seq.int(0, 1, 0.1))

  fit = parametric(Surv(time, status) ~ ., df, dist = "lognormal")
  p = predict(fit, df, type = "all")

  expect_equal(p$surv[15]$cdf(predict(
    fit$model, type = "quantile", p = seq.int(0, 1, 0.1)
  )[15, ]), seq.int(0, 1, 0.1))
})

test_that("quantile type", {
  df <- simsurvdata(50)
  fit <- parametric(Surv(time, status) ~ ., df)

  p <- predict(fit, df, type = "all", form = "aft")
  quantile <- p$surv$quantile(c(0.2, 0.8))
  expect_equal(matrix(t(quantile), ncol = 2),
    predict(fit$model, type = "quantile", p = c(0.2, 0.8)))

  for (form in form_opts) {
    p <- predict(fit, df, type = "all", form = form)
    quantile <- p$surv$quantile(0.5)
    expect_equal(unlist(p$surv$cdf(quantile), use.names = FALSE), rep(0.5, 50))
  }
})

dist_opts <- c("weibull", "exponential", "lognormal", "gaussian", "loglogistic")

test_that("quantile dist", {

  df <- simsurvdata(50)

  for (dist in dist_opts) {
    if (dist == "loglogistic") skip_if_not_installed("actuar")
    fit <- parametric(Surv(time, status) ~ ., df, dist = dist)
    form <- ifelse(dist == "gaussian", "tobit", "aft")
    p <- predict(fit, df, form = form)$quantile(c(0.2, 0.8))
    expect_equal(matrix(t(p), ncol = 2),
      predict(fit$model, type = "quantile", p = c(0.2, 0.8)))
  }
})

test_that("cdf dist", {
  df <- simsurvdata(50)

  for (dist in dist_opts) {
    if (dist == "loglogistic") skip_if_not_installed("actuar")
    fit <- parametric(Surv(time, status) ~ ., df, dist = dist)
    form <- ifelse(dist == "gaussian", "tobit", "aft")
    p <- predict(fit, df, form = form)
    cdf <- predict(fit$model, type = "quantile", p = c(0.2, 0.8))
    expect_equal(unname(as.matrix(p$cdf(data = t(cdf)))),
      matrix(c(rep(0.2, 50), rep(0.8, 50)), byrow = TRUE, nrow = 2))
  }
})


test_that("discrete = continuous when expected", {
  fit <- parametric(Surv(time, status) ~ ., rats)

  for (form in form_opts) {
  p_cont <- predict(fit, rats, form = form, type = "all")
  p_disc <- predict(fit, rats, form = form, type = "all",
    return_method = "discrete")
  expect_equal(p_cont$risk, p_disc$risk)
  utimes <- sort(unique(rats$time))
  s_cont <- as.matrix(p_cont$surv$survival(utimes))
  dimnames(s_cont) <- list(utimes, NULL)
  expect_equal(s_cont, t(p_disc$surv))
  }
})
