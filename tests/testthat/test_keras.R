skip_on_os("windows")
skip_if_not_installed("keras")


test_that("inherits(get_keras_optimizer", {
  expect_equals(class(get_keras_optimizer("adadelta")), "")
  expect_true(inherits(
    get_keras_optimizer("adadelta"),
    "keras.optimizers.optimizer_v2.adadelta.Adadelta"
  ))
  expect_true(inherits(
    get_keras_optimizer("adagrad"),
    "keras.optimizers.optimizer_v2.adagrad.Adagrad"
  ))
  expect_true(inherits(
    get_keras_optimizer("adamax"),
    "keras.optimizers.optimizer_v2.adamax.Adamax"
  ))
  expect_true(inherits(
    get_keras_optimizer("adam"),
    "keras.optimizers.optimizer_v2.adam.Adam"
  ))
  expect_true(inherits(
    get_keras_optimizer("nadam"),
    "keras.optimizers.optimizer_v2.nadam.Nadam"
  ))
  expect_true(inherits(
    get_keras_optimizer("rmsprop"),
    "keras.optimizers.optimizer_v2.rmsprop.RMSprop"
  ))
  expect_true(inherits(
    get_keras_optimizer("sgd"),
    "keras.optimizers.optimizer_v2.gradient_descent.SGD"
  ))
})
