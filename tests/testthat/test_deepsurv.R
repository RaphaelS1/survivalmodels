skip_if_no_pycox()

set.seed(1)
np <- reticulate::import("numpy")
np$random$seed(1L)
torch <- reticulate::import("torch")
torch$manual_seed(1L)

test_that("silent", {
  expect_silent({fit <- deepsurv(Surv(time, status) ~ ., data = rats[1:50, ], verbose = FALSE,
                                 frac = 0.3)})
  expect_silent({predict(fit, newdata = rats[51:100, ])})
})

test_that("auto sanity", {
  sanity_check(model = "deepsurv",
               pars = list(frac = 0.3, activation = "relu", num_nodes = c(2L, 2L),
                           dropout = 0.1, early_stopping = TRUE, epochs = 100L, batch_size = 32L))
})
