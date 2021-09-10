skip_if_no_pycox()

set_seed(1)

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

test_that("auto sanity", {
  sanity_check(
    model = "pchazard",
    pars = list(
      activation = "relu", num_nodes = c(2L, 2L, 2L),
      dropout = 0.1, epochs = 50L, batch_size = 32L
    )
  )
})
