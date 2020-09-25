skip_on_os("windows")

test_that("silent", {
  expect_silent({fit <- pchazard(Surv(time, status) ~ ., data = rats[1:50, ], verbose = FALSE,
                                 frac = 0.3)})
  expect_silent({predict(fit, newdata = rats[51:100, ])})
})

test_that("custom", {
  expect_silent(pchazard(Surv(time, status) ~ ., data = rats[1:10, ],
                         custom_net = build_pytorch_net(3L, 10L),
                         verbose = FALSE))
})
