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
