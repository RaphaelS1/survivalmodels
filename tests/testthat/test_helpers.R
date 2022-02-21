test_that("setcollapse", {
  expect_equal(setcollapse(letters[1:3]), "{a, b, c}")
})

test_that("clean_train_data", {
  expect_equal(clean_train_data(Surv(time, status) ~ ., rats),
               clean_train_data(time_variable = "time", status_variable = "status", data = rats))
  expect_equal(clean_train_data(Surv(time, status) ~ ., rats),
               clean_train_data(Surv(time, status) ~ litter + rx + sex, rats))
  expect_equal(ncol(clean_train_data(Surv(time, status) ~ sex, rats)$x), 1)
  expect_equal(clean_train_data(Surv(time, status) ~ ., rats),
               clean_train_data(x = rats[, c(1:2, 5)], y = Surv(rats$time, rats$status)))
  expect_error(clean_train_data(Surv(rats$time, rats$status) ~ .), "no 'data'")
})

test_that("clean_test_data", {
  if (!requireNamespace("distr6", quietly = TRUE)) {
    skip("distr6 not installed.")
  }
  fit <- akritas(Surv(time, status) ~ ., rats[1:10, ])
  expect_equal(clean_test_data(fit), fit$x)
  expect_error(clean_test_data(fit, rats[, 1:2]), "Names in")
})

test_that("set_seed", {
  skip_if_no_pycox()
  set_seed(1)
  first <- deepsurv(Surv(time, status) ~ ., data = rats[1:50, ], verbose = FALSE,
                          frac = 0.3)
  set_seed(1)
  second <- deepsurv(Surv(time, status) ~ ., data = rats[1:50, ], verbose = FALSE,
                    frac = 0.3)
  expect_equal(first, second)
})


test_that("surv_to_risk", {
  expect_error(surv_to_risk(2), "0 <= x <= 1")
  expect_error(surv_to_risk(matrix(0.5)), "increasing numeric")
  expect_error(surv_to_risk(matrix(0.5, 1, 2, dimnames = list(NULL, 2:1))),
              "increasing numeric")
  expect_error(surv_to_risk(matrix(c(0.5, 0.8), 1, 2, FALSE, list(NULL, 1:2))),
              "decreasing")
})