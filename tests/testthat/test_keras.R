skip_on_os("windows")
skip_if_not_installed("keras")

test_that("inherits(get_keras_optimizer", {
  for (opt in survivalmodels:::keras_optimizers) {
    expect_equal(
      class(get_keras_optimizer(opt)),
      do.call(sprintf("keras::optimizer_%s", opt), list())
    )
  }
})
