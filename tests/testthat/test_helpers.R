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
  fit <- akritas(Surv(time, status) ~ ., rats[1:10, ])
  expect_equal(clean_test_data(fit), fit$x)
  expect_error(clean_test_data(fit, rats[, 1:2]), "Names in")
})

test_that("get_keras_optimizer", {
  expect_is(get_keras_optimizer("adadelta"), "keras.optimizer_v2.adadelta.Adadelta")
  expect_is(get_keras_optimizer("adagrad"), "keras.optimizer_v2.adagrad.Adagrad")
  expect_is(get_keras_optimizer("adamax"), "keras.optimizer_v2.adamax.Adamax")
  expect_is(get_keras_optimizer("adam"), "keras.optimizer_v2.adam.Adam")
  expect_is(get_keras_optimizer("nadam"), "keras.optimizer_v2.nadam.Nadam")
  expect_is(get_keras_optimizer("rmsprop"), "keras.optimizer_v2.rmsprop.RMSprop")
  expect_is(get_keras_optimizer("sgd"), "keras.optimizer_v2.gradient_descent.SGD")
})
