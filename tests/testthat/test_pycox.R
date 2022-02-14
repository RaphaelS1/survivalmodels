skip_if_no_pycox()

set.seed(1)
np <- reticulate::import("numpy")
np$random$seed(1L)
torch <- reticulate::import("torch")
torch$manual_seed(1L)

test_that("get_pycox_optim", {
  net <- build_pytorch_net(1L, 1L, 1L)
  expect_is(get_pycox_optim("adadelta", net),  "torch.optim.adadelta.Adadelta")
  expect_is(get_pycox_optim("adagrad", net),  "torch.optim.adagrad.Adagrad")
  expect_is(get_pycox_optim("adamax", net),  "torch.optim.adamax.Adamax")
  expect_is(get_pycox_optim("adam", net),  "torch.optim.adam.Adam")
  expect_is(get_pycox_optim("adamw", net),  "torch.optim.adamw.AdamW")
  expect_is(get_pycox_optim("asgd", net),  "torch.optim.asgd.ASGD")
  expect_is(get_pycox_optim("rmsprop", net),  "torch.optim.rmsprop.RMSprop")
  expect_is(get_pycox_optim("rprop", net),  "torch.optim.rprop.Rprop")
  expect_is(get_pycox_optim("sgd", net),  "torch.optim.sgd.SGD")
  # expect_is(get_pycox_optim("sparse_adam", net),  "torch.optim.sparse_adam.SparseAdam")
})

test_that("get_pycox_init", {
  a <- 0; b <- 1; mean <- 0; std <- 1; val <- 0; gain <- 1; mode <- "fan_in"
  non_linearity <- "leaky_relu"

  expect_equal(get_pycox_init("uniform"),
               paste0("torch.nn.init.uniform_(m.weight, ", a, ", ", b, ")"))
  expect_equal(get_pycox_init("normal"),
               paste0("torch.nn.init.normal_(m.weight, ", mean, ", ", std, ")"))
  expect_equal(get_pycox_init("constant", val = val),
               paste0("torch.nn.init.constant_(m.weight, ", val, ")"))
  expect_equal(get_pycox_init("xavier_uniform"),
               paste0("torch.nn.init.xavier_uniform_(m.weight, ", gain, ")"))
  expect_equal(get_pycox_init("xavier_normal"),
               paste0("torch.nn.init.xavier_normal_(m.weight, ", gain, ")"))
  expect_equal(get_pycox_init("kaiming_uniform"),
               paste0("torch.nn.init.kaiming_uniform_(m.weight, ", a, ", '",
                 mode, "', '", non_linearity, "')"))
  expect_equal(get_pycox_init("kaiming_normal"),
    paste0("torch.nn.init.kaiming_normal_(m.weight, ", a, ", '", mode, "', '",
           non_linearity, "')"))
  expect_equal(get_pycox_init("orthogonal"),
               paste0("torch.nn.init.orthogonal_(m.weight, ", gain, ")"))
})

fit <- coxtime(Surv(time, status) ~ ., data = rats[1:50, ], verbose = FALSE)

test_that("predict", {
  p <- predict(fit, type = "all", distr6 = FALSE)
  expect_is(p, "list")
  expect_is(p$surv, "matrix")
  expect_is(p$risk, "numeric")
  expect_equal(length(p$risk), 50)
  expect_equal(dim(p$surv), c(50, 20))
})

test_that("predict distr6", {
  if (!requireNamespace("distr6", quietly = TRUE)) {
    skip("distr6 not installed.")
  }
  p <- predict(fit, type = "all", distr6 = TRUE)
  expect_is(p, "list")
  expect_is(p$surv, "VectorDistribution")
  expect_equal(p$surv$properties$support$power, 50)
  p <- predict(fit, type = "survival")
  expect_is(p, "matrix")
})

test_that("build_pytorch_net", {
  expect_silent(build_pytorch_net(2L, 2L, c(2, 4, 8), activation = c("relu", "elu", "glu"),
                                  dropout = c(0.1, 1, 0.62)))
})
