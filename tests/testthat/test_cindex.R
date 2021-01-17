skip_if_not_installed("survival")

test_that("cindex", {
  expect_equal(cindex(1:10, 10:1), 1)
  expect_equal(cindex(1:10, 1:10), 0)
  expect_error(cindex(1:5, 1:6), "length")
})
