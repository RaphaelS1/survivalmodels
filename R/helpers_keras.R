keras_optimizers <- c("adadelta", "adagrad", "adam", "adamax", "ftrl", "nadam", "rmsprop", "sgd")

#' @title Get Keras Optimizer
#' @description Utility function to construct optimiser from \CRANpkg{keras}, primarily for
#' internal use.
#' @param optimizer `(character(1))` \cr Optimizer to construct, see details for those available.
#' Default is `"adam"`.
#' @param lr `(numeric(1))` \cr Learning rate passed to all optimizers.
#' @param epsilon `(numeric(1))` \cr Passed to `adadelta`, `adagrad`, `adam`, `adamax`, `nadam`, `rmsprop`
#' @param beta_1,beta_2 `(numeric(1))` \cr Passed to `adamax`, `adam`, and `nadam`.
#' @param decay,clipnorm,clipvalue,global_clipnorm `(numeric(1))` \cr Passed to all optimizers.
#' @param use_ema,jit_compile `(logical(1))` \cr Passed to all optimizers.
#' @param ema_momentum,ema_overwrite_frequency `(numeric(1))` \cr Passed to all optimizers.
#' @param momentum `(numeric(1))` \cr Passed to `rmsprop` and `sgd`.
#' @param nesterov `(logical(1))` \cr Passed to `sgd`.
#' @param rho `(numeric(1))` \cr Passed to `adadelta` and `rmsprop`.
#' @param initial_accumultator_value `(numeric(1))` \cr Passed to `adagrad` and `ftrl`.
#' @param amsgrad `(logical(1))` \cr Passed to `adam` and `sgd`.
#' @param lr_power,l1_regularization_strength,l2_regularization_strength,l2_shrinkage_regularization_strength,beta `(numeric(1))` \cr Passed to `ftrl`.
#' @param centered `(logical(1))` \cr Passed to `rmsprop`.
#' @details Implemented optimizers are
#'
#' * `"adadelta"` \cr [keras::optimizer_adadelta]
#' * `"adagrad"` \cr [keras::optimizer_adagrad]
#' * `"adam"` \cr [keras::optimizer_adam]
#' * `"adamax"` \cr [keras::optimizer_adamax]
#' * `"ftrl"` \cr [keras::optimizer_ftrl]
#' * `"nadam"` \cr [keras::optimizer_nadam]
#' * `"rmsprop"` \cr [keras::optimizer_rmsprop]
#' * `"sgd"` \cr [keras::optimizer_sgd]
#'
#' @examples
#' \donttest{
#' if (requireNamespaces("keras")) {
#'   get_keras_optimizer()
#'
#'   get_keras_optimizer(optimizer = "adamax", decay = 0.1, lr = 0.01)
#' }
#' }
#'
#' @export
get_keras_optimizer <- function(optimizer = "adam", lr = 0.001, beta_1 = 0.9, beta_2 = 0.999,
  epsilon = 1e-7, decay = NULL, clipnorm = NULL, clipvalue = NULL,
  momentum = 0, nesterov = FALSE, rho = 0.95, global_clipnorm = NULL,
  use_ema = FALSE, ema_momentum = 0.99, ema_overwrite_frequency = NULL,
  jit_compile = TRUE, initial_accumultator_value = 0.1, amsgrad = FALSE,
  lr_power = -0.5, l1_regularization_strength = 0, l2_regularization_strength = 0,
  l2_shrinkage_regularization_strength = 0, beta = 0, centered = FALSE) {

  if (!requireNamespace("keras", quietly = TRUE)) {
    stop("Package 'keras' required but not installed.") # nocov
  }

  switch(optimizer,
    adadelta = keras::optimizer_adadelta(lr, rho, epsilon, decay, clipnorm,
    clipvalue, global_clipnorm, use_ema, ema_momentum, ema_overwrite_frequency, jit_compile),
    adagrad = keras::optimizer_adagrad(lr, initial_accumultator_value, epsilon, decay, clipnorm,
    clipvalue, global_clipnorm, use_ema, ema_momentum, ema_overwrite_frequency,
    jit_compile),
    adam = keras::optimizer_adam(lr, beta_1, beta_2, epsilon, amsgrad, decay, clipnorm, clipvalue,
    global_clipnorm, use_ema, ema_momentum, ema_overwrite_frequency, jit_compile),
    adamax = keras::optimizer_adamax(lr, beta_1, beta_2, epsilon, decay, clipnorm, clipvalue,
    global_clipnorm, use_ema, ema_momentum, ema_overwrite_frequency, jit_compile),
    ftrl = keras::optimizer_ftrl(lr, lr_power, initial_accumultator_value,
    l1_regularization_strength, l2_regularization_strength,
    l2_shrinkage_regularization_strength, beta, decay,
    clipnorm, clipvalue, global_clipnorm, use_ema, ema_momentum,
    ema_overwrite_frequency, jit_compile),
    nadam = keras::optimizer_nadam(lr, beta_1, beta_2, epsilon, decay, clipnorm, clipvalue,
    global_clipnorm, use_ema, ema_momentum, ema_overwrite_frequency, jit_compile),
    rmsprop = keras::optimizer_rmsprop(lr, rho, momentum, epsilon, centered,
    decay, clipnorm, clipvalue, global_clipnorm, use_ema, ema_momentum,
    ema_overwrite_frequency, jit_compile),
    sgd = keras::optimizer_sgd(lr, momentum, nesterov, amsgrad, decay, clipnorm, clipvalue,
    global_clipnorm, use_ema, ema_momentum, ema_overwrite_frequency, jit_compile)
  )
}

#' @title Build a Keras Multilayer Perceptron
#' @description Utility function to build a Keras MLP.
#' @details This function is a helper for R users with less Python experience. Currently it is
#' limited to simple MLPs and with identical layers.
#' More advanced networks will require manual creation with \CRANpkg{keras}.
#' @param n_in `(integer(1))`\cr Number of input features.
#' @param n_out `(integer(1))`\cr Number of targets.
#' @param nodes `(numeric())`\cr Hidden nodes in network, each element in vector represents number
#' of hidden nodes in respective layer.
#' @param layer_pars `(list())`\cr Arguments passed to [keras::layer_dense].
#' @param activation `(character(1))`\cr Activation function passed to [keras::layer_activation].
#' Default is linear.
#' @param act_pars `(list())`\cr Parameters for activation function, see
#' [keras::layer_activation].
#' @param dropout `(numeric(1))`\cr Optional dropout layer, if `NULL` then no dropout layer added
#' otherwise either same dropout will be added to all layers.
#' @param batch_norm `(logical(1))`\cr If `TRUE` (default) then batch normalisation is applied
#' to all layers.
#' @param batch_pars `(list())`\cr Parameters for batch normalisation, see
#' [keras::layer_batch_normalization].
#'
#' @examples
#' \donttest{
#' if (requireNamespaces("keras")) {
#'   build_keras_net(4L, 2L)
#'
#'   build_keras_net(n_in = 4L, n_out = 2L, nodes = c(32L, 64L, 32L),
#'     activation = "elu", dropout = 0.4)
#' }
#' }
#'
#' @export
build_keras_net <- function(n_in, n_out, nodes = c(32L, 32L), layer_pars = list(),
                           activation = "linear", act_pars = list(),
                           dropout = 0.1, batch_norm = TRUE,
                           batch_pars = list()) {

  if (!requireNamespace("keras", quietly = TRUE)) {
    stop("Package 'keras' required but not installed.") # nocov
  }

  add_module <- function(net, num_in, num_out) {
    do.call(
      keras::layer_dense,
      c(list(
        object = net,
        units = num_out,
        input_shape = num_in),
        layer_pars)
    )

    do.call(
      keras::layer_activation,
      c(list(
        object = net,
        activation = activation),
        act_pars)
    )

    if (batch_norm) {
      do.call(
        keras::layer_batch_normalization,
        c(list(
          object = net),
          batch_pars)
      )
    }

    if (!is.null(dropout)) {
      keras::layer_dropout(net, dropout)
    }
  }

  net <- keras::keras_model_sequential()

  # input layer
  add_module(net, n_in, nodes[1])

  # hidden layers
  for (i in seq_along(nodes)) {
    if (i < length(nodes)) {
      add_module(net, nodes[i], nodes[i + 1])
    } else {
      # output layer
      add_module(net, nodes[i], n_out)
    }
  }

  return(net)
}

#' @title Install Keras and Tensorflow
#' @description Stripped back version of [keras::install_keras]. Note the
#' default for `pip` is changed to `TRUE`.
#' @param method,conda,pip See [reticulate::py_install].
#' @param install_tensorflow If `TRUE` installs the dependency `tensorflow` package as well.
#' @param ... Passed to [reticulate::py_install].
#' @export
install_keras <- function(method = "auto", conda = "auto", pip = TRUE,
                          install_tensorflow = FALSE, ...) {
  # nocov start
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' required but not installed.") # nocov
  }

  pkg <- "keras"
  if (install_tensorflow) {
    pkg <- c("tensorflow", pkg)
  }
  reticulate::py_install(pkg, method = method, conda = conda, pip = pip, ...)
  # nocov end
}

# Code originally from https://github.com/lilizhaoUM/DNNSurv with minor edits.
# t - Survival time
# d - Censoring indicator
# qt - Vector of time points for dividing time interval
# Returns subject ids, time-points, and pseudo conditional probabilities
.get_pseudo_conditional <- function(t, d, qt) {

  s <- c(0, qt)
  n <- length(t)
  ns <- length(s) - 1 # the number of intervals
  D <- do.call(cbind, lapply(seq_len(ns), function(j) (s[j] < t) * (t <= s[j + 1]) * (d == 1)))
  R <- do.call(cbind, lapply(seq_len(ns), function(j) ifelse(s[j] < t, 1, 0)))
  Delta <- do.call(cbind, lapply(seq_len(ns), function(j) pmin(t, s[j + 1]) - s[j]))

  # long format
  dd_tmp <- cbind.data.frame(id = rep(seq_len(n), ns),
                            s = rep(c(0, qt[-length(qt)]), each = n),
                            y = c(R * Delta),
                            d = c(D))

  dd <- dd_tmp[dd_tmp$y > 0, ]
  pseudost <- rep(NA, nrow(dd))
  for (j in seq_len(ns)) {
    index <- (dd$s == s[j])
    dds <- dd[index, ]
    if (all(dds$d) || !any(dds$d)) {
      pseudost[index] <- sum(index)
    } else {
      pseudost[index] <- pseudo::pseudosurv(time = dds$y, event = dds$d,
                                           tmax = s[j + 1] - s[j])$pseudo
    }
  }
  dd$pseudost <- pseudost

  return(dd[, c(1, 2, 5)])
}
