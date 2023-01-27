# skip_on_os("windows")
skip_if_not_installed("keras")

library(keras)
for (opt in survivalmodels:::keras_optimizers) {
  test_that(sprintf("inherits(get_keras_optimizer(%s)", opt), {
    expect_equal(
      class(get_keras_optimizer(opt)),
      class(do.call(sprintf("optimizer_%s", opt), list()))
    )
  })
}
