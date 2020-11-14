skip_if_no_pycox()

set.seed(1)
np <- reticulate::import("numpy")
np$random$seed(1L)
torch <- reticulate::import("torch")
torch$manual_seed(1L)

test_that("silent", {
  expect_silent({fit <- coxtime(Surv(time, status) ~ ., data = rats[1:50, ],
                                                  verbose = FALSE,  frac = 0.3)})
  expect_silent({predict(fit, newdata = rats[51:100, ])})
})

test_that("standardize", {
  expect_silent(coxtime(Surv(time, status) ~ ., data = rats[1:50, ], verbose = FALSE,
                        standardize_time = TRUE))
})

test_that("early_stopping", {
  expect_true("EarlyStopping" %in% names(coxtime(Surv(time, status) ~ ., data = rats[1:50, ],
                                                 verbose = FALSE,
                        early_stopping = TRUE)$model$callbacks$callbacks))
  expect_true("BestWeights" %in% names(coxtime(Surv(time, status) ~ ., data = rats[1:50, ],
                                                 verbose = FALSE,
                                                 best_weights = TRUE)$model$callbacks$callbacks))
  expect_true("EarlyStopping" %in% names(coxtime(Surv(time, status) ~ ., data = rats[1:50, ],
                                               verbose = FALSE, early_stopping = TRUE,
                                               best_weights = TRUE)$model$callbacks$callbacks))
})

test_that("auto sanity", {
  sanity_check(model = "coxtime",
               pars = list(frac = 0.3, activation = "relu", num_nodes = c(2L, 2L),
                           dropout = 0.1, early_stopping = TRUE, epochs = 100L, batch_size = 32L))
})
