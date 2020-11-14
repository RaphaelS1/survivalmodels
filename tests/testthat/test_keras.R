skip_on_os("windows")
skip_if_not_installed("keras")

test_that("get_keras_optimizer", {
  expect_is(get_keras_optimizer("adadelta"), "keras.optimizer_v2.adadelta.Adadelta")
  expect_is(get_keras_optimizer("adagrad"), "keras.optimizer_v2.adagrad.Adagrad")
  expect_is(get_keras_optimizer("adamax"), "keras.optimizer_v2.adamax.Adamax")
  expect_is(get_keras_optimizer("adam"), "keras.optimizer_v2.adam.Adam")
  expect_is(get_keras_optimizer("nadam"), "keras.optimizer_v2.nadam.Nadam")
  expect_is(get_keras_optimizer("rmsprop"), "keras.optimizer_v2.rmsprop.RMSprop")
  expect_is(get_keras_optimizer("sgd"), "keras.optimizer_v2.gradient_descent.SGD")
})
