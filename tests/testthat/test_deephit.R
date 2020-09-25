skip_on_os("windows")

test_that("silent", {
  expect_silent({fit <- deephit(Surv(time, status) ~ ., data = rats[1:50, ], verbose = FALSE)})
  expect_silent({predict(fit, newdata = rats[51:100, ])})
})

test_that("custom", {
  expect_silent(deephit(Surv(time, status) ~ ., data = rats[1:10, ],
                       custom_net = build_pytorch_net(3L, 10L),
                       verbose = FALSE))
})
