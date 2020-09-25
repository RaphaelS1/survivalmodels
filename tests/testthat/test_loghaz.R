skip_on_os("windows")

test_that("silent", {
  expect_silent({fit <- loghaz(Surv(time, status) ~ ., data = rats[1:50, ], verbose = FALSE,
                               frac = 0.3)})
  expect_silent({predict(fit, newdata = rats[51:100, ])})
})

test_that("custom", {
  expect_silent(loghaz(Surv(time, status) ~ ., data = rats[1:10, ],
                         custom_net = build_pytorch_net(3L, 10L),
                         verbose = FALSE))
})

test_that("cutpoints", {
  expect_equal(loghaz(Surv(time, status) ~ ., data = rats[1:50, ], verbose = FALSE,
                       cutpoints = c(20, 50, 70))$model$duration_index,
               array(c(20, 50, 70)))
  expect_silent(loghaz(Surv(time, status) ~ ., data = rats[1:50, ], verbose = FALSE,
                       cuts = 3))
})

test_that("interpolate", {
  fit <- loghaz(Surv(time, status) ~ ., data = rats[1:50, ], verbose = FALSE)
  expect_silent(predict(fit, interpolate = TRUE))
})
