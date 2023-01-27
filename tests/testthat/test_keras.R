# skip_on_os("windows")
skip_if_not_installed("keras")

test_that("inherits(get_keras_optimizer", {
  library(keras)
  for (opt in survivalmodels:::keras_optimizers) {
    expect_equal(
      class(get_keras_optimizer(opt)),
      class(do.call(sprintf("optimizer_%s", opt), list()))
    )
  }
})
