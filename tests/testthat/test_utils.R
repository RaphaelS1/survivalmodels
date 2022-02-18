test_that("fill_na", {
  expect_equal(
    fill_na(matrix(c(1, 4, NA, 6, 2,
                    NA, NA, 5, NA, NA), 2, 5, TRUE)),
    matrix(c(1, 4, 4, 6, 2,
              5, 5, 5, 5, 5), 2, 5, TRUE)
  )

  expect_error(
    fill_na(matrix(c(1, 4, 9, 6, 2,
                    NA, 1, 5, 0, 6), 2, 5, TRUE))
  )

  expect_equal(
    fill_na(matrix(c(1, 4, 9, 6, 2,
                    NA, NA, NA, NA, NA), 2, 5, TRUE)),
    matrix(c(1, 4, 9, 6, 2,
            NA, NA, NA, NA, NA), 2, 5, TRUE)
  )

  expect_equal(
    fill_na(matrix(c(1, NA, NA, 6, NA,
                    NA, NA, NA, NA, NA), 2, 5, TRUE)),
    matrix(c(1, 1, 1, 6, 6,
            NA, NA, NA, NA, NA), 2, 5, TRUE)
  )
})
