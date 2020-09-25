test_that("get_pycox_optim", {
  net = build_pytorch_net(1L, 1L, 1L)
  expect_is(get_pycox_optim("adadelta", net),  "torch.optim.adadelta.Adadelta")
  expect_is(get_pycox_optim("adagrad", net),  "torch.optim.adagrad.Adagrad")
  expect_is(get_pycox_optim("adamax", net),  "torch.optim.adamax.Adamax")
  expect_is(get_pycox_optim("adam", net),  "torch.optim.adam.Adam")
  expect_is(get_pycox_optim("adamw", net),  "torch.optim.adamw.AdamW")
  expect_is(get_pycox_optim("asgd", net),  "torch.optim.asgd.ASGD")
  expect_is(get_pycox_optim("rmsprop", net),  "torch.optim.rmsprop.RMSprop")
  expect_is(get_pycox_optim("rprop", net),  "torch.optim.rprop.Rprop")
  expect_is(get_pycox_optim("sgd", net),  "torch.optim.sgd.SGD")
  expect_is(get_pycox_optim("sparse_adam", net),  "torch.optim.sparse_adam.SparseAdam")
})

test_that("predict", {
  fit <- coxtime(Surv(time, status) ~ ., data = rats[1:50, ], verbose = FALSE)
  p <- predict(fit, type = "all", distr6 = FALSE)
  expect_is(p, "list")
  expect_is(p$surv, "matrix")
  expect_is(p$risk, "numeric")
  p <- predict(fit, type = "all", distr6 = TRUE)
  expect_is(p, "list")
  expect_is(p$surv, "VectorDistribution")
  p <- predict(fit, type = "survival")
  expect_is(p, "matrix")
})

test_that("build_pytorch_net", {
  expect_silent(build_pytorch_net(2L, 2L, c(2,4,8), activation = c("relu", "elu", "glu"),
                                  dropout = c(0.1, 1, 0.62)))
})
