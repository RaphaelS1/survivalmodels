#' @title Prepare Data for Pycox Model Training
#' @description Utility function to prepare data for training in a Pycox model.
#' Generally used internally only.
#'
#' @param x_train `(matrix(1))` \cr Training covariates.
#' @param y_train `(matrix(1))` \cr Training outcomes.
#' @param frac `(numeric(1))`\cr Fraction of data to use for validation dataset, default is `0`
#' and therefore no separate validation dataset.
#' @param standardize_time `(logical(1))`\cr If `TRUE`, the time outcome to be standardized. For use
#' with [coxtime].
#' @param log_duration `(logical(1))`\cr If `TRUE` and `standardize_time` is `TRUE` then time
#' variable is log transformed.
#' @param with_mean `(logical(1))`\cr If `TRUE` (default) and `standardize_time` is `TRUE` then time
#' variable is centered.
#' @param with_std `(logical(1))`\cr If `TRUE` (default) and `standardize_time` is `TRUE` then time
#' variable is scaled to unit variance.
#' @param discretise `(logical(1))`\cr If `TRUE` then time is discretised. For use with the models
#' [deephit], [pchazard], and [loghaz].
#' @param cuts `(integer(1))`\cr If `discretise` is `TRUE` then determines number of cut-points
#' for discretisation.
#' @param cutpoints `(numeric())` \cr Alternative to `cuts` if `discretise` is true, provide
#' exact cutpoints for discretisation. `cuts` is ignored if `cutpoints` is non-NULL.
#' @param scheme `(character(1))`\cr Method of discretisation, either `"equidistant"` (default)
#' or `"quantiles"`. See `reticulate::py_help(pycox$models$LogisticHazard$label_transform)`.
#' @param cut_min `(integer(1))`\cr Starting duration for discretisation, see
#' `reticulate::py_help(pycox$models$LogisticHazard$label_transform)`.
#' @param model `(character(1))`\cr Corresponding pycox model.
#'
#' @export
pycox_prepare_train_data <- function(x_train, y_train, frac = 0, standardize_time = FALSE,
                              log_duration = FALSE,
  with_mean = TRUE, with_std = TRUE, discretise = FALSE, cuts = 10L,
  cutpoints = NULL, scheme = c("equidistant", "quantiles"),
  cut_min = 0L, model = c("coxtime", "deepsurv", "deephit", "loghaz", "pchazard")) {

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' required but not installed.") # nocov
  }

  torchtuples <- reticulate::import("torchtuples")
  pycox <- reticulate::import("pycox")
  model <- match.arg(model)

  conv <- ifelse(discretise, "int64", "float32")

  x_train <- data.frame(x_train)
  y_train <- data.frame(y_train)
  colnames(y_train) <- c("time", "status")

  if (frac) {
    val <- sample(seq_len(nrow(x_train)), nrow(x_train) * frac)
    x_val <- x_train[val, , drop=FALSE]
    y_val <- y_train[val, , drop=FALSE]
    x_train <- x_train[-val, , drop=FALSE]
    y_train <- y_train[-val, , drop=FALSE]
  }

  y_train <- reticulate::r_to_py(y_train)
  x_train <- reticulate::r_to_py(x_train)$values$astype("float32")
  y_train <- reticulate::tuple(
    y_train["time"]$values$astype(conv),
    y_train["status"]$values$astype(conv))


  if (frac) {
    x_val <- reticulate::r_to_py(x_val)$values$astype("float32")
    y_val <- reticulate::r_to_py(y_val)
    y_val <- reticulate::tuple(
      y_val["time"]$values$astype(conv),
      y_val["status"]$values$astype(conv))
  }

  ret <- list(x_train = x_train, y_train = y_train)

  if (standardize_time || discretise) {
    if (standardize_time) {
      labtrans <- do.call(
        pycox$models$CoxTime$label_transform,
        list(
          log_duration = log_duration,
          with_mean = with_mean,
          with_std = with_std
        )
      )
    } else {
      if (!is.null(cutpoints)) {
        cuts <- reticulate::r_to_py(cutpoints) # nocov - testing causes errors due to py storage
      } else {
        cuts <- as.integer(cuts)
      }
      if (model == "deephit") {
        labtrans <- do.call(
          pycox$models$DeepHitSingle$label_transform,
          list(
            cuts = cuts,
            scheme = match.arg(scheme),
            min_ = as.integer(cut_min)
          )
        )
      } else if (model == "loghaz") {
        labtrans <- do.call(
          pycox$models$LogisticHazard$label_transform,
          list(
            cuts = cuts,
            scheme = match.arg(scheme),
            min_ = as.integer(cut_min)
          )
        )
      } else if (model == "pchazard") {
        labtrans <- do.call(
          pycox$models$PCHazard$label_transform,
          list(
            cuts = cuts,
            scheme = match.arg(scheme),
            min_ = as.integer(cut_min)
          )
        )
      }
    }

    y_train <- reticulate::r_to_py(labtrans$fit_transform(y_train[0], y_train[1]))

    if (model %in% c("coxtime", "deephit")) {
      y_train <- reticulate::tuple(
        y_train[0]$astype(conv),
        y_train[1]$astype(conv)
      )
    } else if (model == "loghaz") {
      y_train <- reticulate::tuple(
        y_train[0]$astype("int64"),
        y_train[1]$astype("float32")
      )
    } else if (model == "pchazard") {
      y_train <- reticulate::tuple(
        y_train[0]$astype("int64"),
        y_train[1]$astype("float32"),
        y_train[2]$astype("float32")
      )
    }

    ret$y_train <- y_train
    ret$labtrans <- labtrans

    if (frac) {
      y_val <- reticulate::r_to_py(labtrans$transform(y_val[0], y_val[1]))

      if (model %in% c("coxtime", "deephit")) {
        y_val <- reticulate::tuple(
          y_val[0]$astype(conv),
          y_val[1]$astype(conv)
        )
      } else if (model == "loghaz") {
        y_val <- reticulate::tuple(
          y_val[0]$astype("int64"),
          y_val[1]$astype("float32")
        )
      } else if (model == "pchazard") {
        y_val <- reticulate::tuple(
          y_val[0]$astype("int64"),
          y_val[1]$astype("float32"),
          y_val[2]$astype("float32")
        )
      }
    }
  }

  if (frac) {
    ret$val <- torchtuples$tuplefy(x_val, y_val)
  }

  return(ret)
}

pycox_activations <- c(
  "celu", "elu", "gelu", "glu", "hardshrink", "hardsigmoid", "hardswish",
  "hardtanh", "relu6", "leakyrelu", "logsigmoid", "logsoftmax",
  "prelu", "rrelu", "relu", "selu", "sigmoid",
  "softmax", "softmax2d", "softmin", "softplus", "softshrink", "softsign",
  "tanh", "tanhshrink", "threshold")

#' @title Get Pytorch Activation Function
#' @description Helper function to return a class or constructed object for pytorch activation
#' function from `torch.nn.modules.activation`.
#' @param activation `(character(1))`\cr Activation function method, see details for list of
#' implemented methods.
#' @param construct `(logical(1))`\cr If `TRUE` (default) returns constructed object, otherwise
#' a class.
#' @param alpha `(numeric(1))`\cr Passed to `celu` and `elu`.
#' @param dim `(integer(1))`\cr Passed to `glu`, `logsoftmax`, `softmax`, and `softmin`.
#' @param lambd `(numeric(1))`\cr Passed to `hardshrink` and `softshrink`.
#' @param min_val,max_val `(numeric(1))`\cr Passed to `hardtanh`.
#' @param negative_slope `(numeric(1))`\cr Passed to `leakyrelu`.
#' @param num_parameters `(integer(1))`\cr Passed to `prelu`.
#' @param init `(numeric(1))`\cr Passed to `prelu`.
#' @param lower,upper `(numeric(1))`\cr Passed to `rrelu`.
#' @param beta `(numeric(1))`\cr Passed to `softplus`.
#' @param threshold `(numeric(1))`\cr Passed to `softplus` and `threshold`.
#' @param value `(numeric(1))`\cr Passed to `threshold`.
#'
#' @details
#' Implemented methods (with help pages) are
#'
#' * `"celu"` \cr `reticulate::py_help(torch$nn$modules$activation$CELU)`
#' * `"elu"` \cr `reticulate::py_help(torch$nn$modules$activation$ELU)`
#' * `"gelu"` \cr `reticulate::py_help(torch$nn$modules$activation$GELU)`
#' * `"glu"` \cr `reticulate::py_help(torch$nn$modules$activation$GLU)`
#' * `"hardshrink"` \cr `reticulate::py_help(torch$nn$modules$activation$Hardshrink)`
#' * `"hardsigmoid"` \cr `reticulate::py_help(torch$nn$modules$activation$Hardsigmoid)`
#' * `"hardswish"` \cr `reticulate::py_help(torch$nn$modules$activation$Hardswish)`
#' * `"hardtanh"` \cr `reticulate::py_help(torch$nn$modules$activation$Hardtanh)`
#' * `"relu6"` \cr `reticulate::py_help(torch$nn$modules$activation$ReLU6)`
#' * `"leakyrelu"` \cr `reticulate::py_help(torch$nn$modules$activation$LeakyReLU)`
#' * `"logsigmoid"` \cr `reticulate::py_help(torch$nn$modules$activation$LogSigmoid)`
#' * `"logsoftmax"` \cr `reticulate::py_help(torch$nn$modules$activation$LogSoftmax)`
#' * `"prelu"` \cr `reticulate::py_help(torch$nn$modules$activation$PReLU)`
#' * `"rrelu"` \cr `reticulate::py_help(torch$nn$modules$activation$RReLU)`
#' * `"relu"` \cr `reticulate::py_help(torch$nn$modules$activation$ReLU)`
#' * `"selu"` \cr `reticulate::py_help(torch$nn$modules$activation$SELU)`
#' * `"sigmoid"` \cr `reticulate::py_help(torch$nn$modules$activation$Sigmoid)`
#' * `"softmax"` \cr `reticulate::py_help(torch$nn$modules$activation$Softmax)`
#' * `"softmax2d"` \cr `reticulate::py_help(torch$nn$modules$activation$Softmax2d)`
#' * `"softmin"` \cr `reticulate::py_help(torch$nn$modules$activation$Softmin)`
#' * `"softplus"` \cr `reticulate::py_help(torch$nn$modules$activation$Softplus)`
#' * `"softshrink"` \cr `reticulate::py_help(torch$nn$modules$activation$Softshrink)`
#' * `"softsign"` \cr `reticulate::py_help(torch$nn$modules$activation$Softsign)`
#' * `"tanh"` \cr `reticulate::py_help(torch$nn$modules$activation$Tanh)`
#' * `"tanhshrink"` \cr `reticulate::py_help(torch$nn$modules$activation$Tanhshrink)`
#' * `"threshold"` \cr `reticulate::py_help(torch$nn$modules$activation$Threshold)`
#'
#' @examples
#' \donttest{
#' if (requireNamespaces("reticulate")) {
#'   #' # returns constructed objects
#'   get_pycox_activation(activation = "relu", construct = TRUE)
#'
#'   # returns class
#'   get_pycox_activation(activation = "selu", construct = FALSE)
#' }
#' }
#'
#' @export
get_pycox_activation <- function(activation = "relu", construct = TRUE, alpha = 1, dim = NULL,
  lambd = 0.5, min_val = -1, max_val = 1, negative_slope = 0.01,
  num_parameters = 1L, init = 0.25, lower = 1 / 8, upper = 1 / 3,
  beta = 1, threshold = 20, value = 20) {

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' required but not installed.") # nocov
  }

  torch <- reticulate::import("torch")
  act <- torch$nn$modules$activation

  if (construct) {
    activation <- switch(activation,
      celu = act$CELU(alpha),
      elu = act$ELU(alpha),
      gelu = act$GELU(),
      glu = act$GLU(as.integer(dim)),
      hardshrink = act$Hardshrink(lambd),
      hardsigmoid = act$Hardsigmoid(),
      hardswish = act$Hardswish(),
      hardtanh = act$Hardtanh(as.integer(min_val), as.integer(max_val)),
      relu6 = act$ReLU6(),
      leakyrelu = act$LeakyReLU(negative_slope),
      logsigmoid = act$LogSigmoid(),
      logsoftmax = act$LogSoftmax(as.integer(dim)),
      prelu = act$PReLU(num_parameters = as.integer(num_parameters), init = init),
      rrelu = act$RReLU(lower, upper),
      relu = act$ReLU(),
      selu = act$SELU(),
      sigmoid = act$Sigmoid(),
      softmax = act$Softmax(as.integer(dim)),
      softmax2d = act$Softmax2d(),
      softmin = act$Softmin(as.integer(dim)),
      softplus = act$Softplus(beta, threshold),
      softshrink = act$Softshrink(lambd),
      softsign = act$Softsign(),
      tanh = act$Tanh(),
      tanhshrink = act$Tanhshrink(),
      threshold = act$Threshold(threshold, value)
    )
  } else {
    activation <- switch(activation,
      celu = act$CELU,
      elu = act$ELU,
      gelu = act$GELU,
      glu = act$GLU,
      hardshrink = act$Hardshrink,
      hardsigmoid = act$Hardsigmoid,
      hardswish = act$Hardswish,
      hardtanh = act$Hardtanh,
      relu6 = act$ReLU6,
      leakyrelu = act$LeakyReLU,
      logsigmoid = act$LogSigmoid,
      logsoftmax = act$LogSoftmax,
      prelu = act$PReLU,
      rrelu = act$RReLU,
      relu = act$ReLU,
      selu = act$SELU,
      sigmoid = act$Sigmoid,
      softmax = act$Softmax,
      softmax2d = act$Softmax2d,
      softmin = act$Softmin,
      softplus = act$Softplus(beta, threshold),
      softshrink = act$Softshrink(lambd),
      softsign = act$Softsign(),
      tanh = act$Tanh(),
      tanhshrink = act$Tanhshrink(),
      threshold = act$Threshold(threshold, value)
    )
  }
}

pycox_optimizers <- c(
  "adadelta", "adagrad", "adam", "adamax", "adamw", "asgd",
  "rmsprop", "rprop", "sgd", "sparse_adam")

#' @title Get Pytorch Optimizer
#' @description Helper function to return a constructed pytorch optimizer from `torch.optim`.
#' @param optimizer `(character(1))`\cr Optimizer, see details for list of implemented methods.
#' @param net `(torch.nn.modules.module.Module)`\cr Network architecture, can be built from
#' [build_pytorch_net].
#' @param rho,lr,lr_decay `(numeric(1))`\cr Passed to `adadelta`.
#' @param eps `(numeric(1))`\cr Passed to all methods except `asgd`, `rprop`, and `sgd`.
#' @param weight_decay `(numeric(1))`\cr Passed to all methods except `rprop` and `sparse_adam`.
#' @param learning_rate `(numeric(1))`\cr Passed to all methods except `adadelta`.
#' @param betas `(numeric(2))`\cr Passed to `adam`, `adamax`, `adamw`, and `sparse_adam`.
#' @param amsgrad `(logical(1))`\cr Passed to `adam` and `adamw`.
#' @param lambd,t0 `(numeric(1))`\cr Passed to `asgd`.
#' @param alpha `(numeric(1))`\cr Passed to `asgd` and `rmsprop`.
#' @param momentum `(numeric(1))`\cr Passed to `rmsprop` and `sgd`.
#' @param centered `(logical(1))`\cr Passed to `rmsprop`.
#' @param etas,step_sizes `(numeric(2))`\cr Passed to `rprop`.
#' @param dampening `(numeric(1))`\cr Passed to `sgd`.
#' @param nesterov `(logical(1))`\cr Passed to `sgd`.
#'
#' @details
#' Implemented methods (with help pages) are
#'
#' * `"adadelta"` \cr `reticulate::py_help(torch$optim$Adadelta)`
#' * `"adagrad"` \cr `reticulate::py_help(torch$optim$Adagrad)`
#' * `"adam"` \cr `reticulate::py_help(torch$optim$Adam)`
#' * `"adamax"` \cr `reticulate::py_help(torch$optim$Adamax)`
#' * `"adamw"` \cr `reticulate::py_help(torch$optim$AdamW)`
#' * `"asgd"` \cr `reticulate::py_help(torch$optim$ASGD)`
#' * `"rmsprop"` \cr `reticulate::py_help(torch$optim$RMSprop)`
#' * `"rprop"` \cr `reticulate::py_help(torch$optim$Rprop)`
#' * `"sgd"` \cr `reticulate::py_help(torch$optim$SGD)`
#' * `"sparse_adam"` \cr `reticulate::py_help(torch$optim$SparseAdam)`
#'
#' @export
get_pycox_optim <- function(optimizer = "adam", net, rho = 0.9, eps = 1e-8, lr = 1,
  weight_decay = 0, learning_rate = 1e-2, lr_decay = 0,
  betas = c(0.9, 0.999), amsgrad = FALSE,
  lambd = 1e-4, alpha = 0.75, t0 = 1e6,
  momentum = 0, centered = TRUE, etas = c(0.5, 1.2),
  step_sizes = c(1e-6, 50), dampening = 0,
  nesterov = FALSE) {

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' required but not installed.") # nocov
  }

  torch <- reticulate::import("torch")
  opt <- torch$optim
  params <- net$parameters()

  switch(optimizer,
    adadelta = opt$Adadelta(params, rho, eps, lr, weight_decay),
    adagrad = opt$Adagrad(params, learning_rate, lr_decay, weight_decay, eps),
    adam = opt$Adam(params, learning_rate, betas, eps, weight_decay, amsgrad),
    adamax = opt$Adamax(params, learning_rate, betas, eps, weight_decay),
    adamw = opt$AdamW(params, learning_rate, betas, eps, weight_decay, amsgrad),
    asgd = opt$ASGD(params, learning_rate, lambd, alpha, t0, weight_decay),
    rmsprop = opt$RMSprop(
      params, learning_rate, momentum, alpha, eps, centered,
      weight_decay),
    rprop = opt$Rprop(params, learning_rate, etas, step_sizes),
    sgd = opt$SGD(params, learning_rate, momentum, weight_decay, dampening, nesterov),
    sparse_adam = opt$SparseAdam(params, learning_rate, betas, eps)
  )
}

pycox_initializers <- c(
  "uniform", "normal", "constant", "xavier_uniform", "xavier_normal",
  "kaiming_uniform", "kaiming_normal", "orthogonal")

#' @title Get Pytorch Weight Initialization Method
#' @description Helper function to return a character string with a populated pytorch weight
#' initializer method from `torch.nn.init`. Used in [build_pytorch_net] to define a weighting
#' function.
#' @param init `(character(1))`\cr Initialization method, see details for list of implemented
#' methods.
#' @param a `(numeric(1))`\cr Passed to `uniform`, `kaiming_uniform`, and `kaiming_normal`.
#' @param b `(numeric(1))`\cr Passed to `uniform`.
#' @param mean,std `(numeric(1))`\cr Passed to `normal`.
#' @param val `(numeric(1))`\cr Passed to `constant`.
#' @param gain `(numeric(1))`\cr Passed to `xavier_uniform`, `xavier_normal`, and `orthogonal`.
#' @param mode `(character(1))`\cr Passed to `kaiming_uniform` and `kaiming_normal`, one of
#' `fan_in` (default) and `fan_out`.
#' @param non_linearity `(character(1))`\cr Passed to `kaiming_uniform` and `kaiming_normal`, one of
#' `leaky_relu` (default) and `relu`.
#' @details
#' Implemented methods (with help pages) are
#'
#' * `"uniform"` \cr `reticulate::py_help(torch$nn$init$uniform_)`
#' * `"normal"` \cr `reticulate::py_help(torch$nn$init$normal_)`
#' * `"constant"` \cr `reticulate::py_help(torch$nn$init$constant_)`
#' * `"xavier_uniform"` \cr `reticulate::py_help(torch$nn$init$xavier_uniform_)`
#' * `"xavier_normal"` \cr `reticulate::py_help(torch$nn$init$xavier_normal_)`
#' * `"kaiming_uniform"` \cr `reticulate::py_help(torch$nn$init$kaiming_uniform_)`
#' * `"kaiming_normal"` \cr `reticulate::py_help(torch$nn$init$kaiming_normal_)`
#' * `"orthogonal"` \cr `reticulate::py_help(torch$nn$init$orthogonal_)`
#'
#' @examples
#' \donttest{
#' if (requireNamespaces("reticulate")) {
#'   get_pycox_init(init = "uniform")
#'
#'   get_pycox_init(init = "kaiming_uniform", a = 0, mode = "fan_out")
#' }
#' }
#'
#' @export
get_pycox_init <- function(init = "uniform", a = 0, b = 1, mean = 0, std = 1, val, gain = 1,
  mode = c("fan_in", "fan_out"), non_linearity = c("leaky_relu", "relu")) {

  switch(init,
    uniform = paste0("torch.nn.init.uniform_(m.weight, ", a, ", ", b, ")"),
    normal = paste0("torch.nn.init.normal_(m.weight, ", mean, ", ", std, ")"),
    constant = paste0("torch.nn.init.constant_(m.weight, ", val, ")"),
    xavier_uniform = paste0("torch.nn.init.xavier_uniform_(m.weight, ", gain, ")"),
    xavier_normal = paste0("torch.nn.init.xavier_normal_(m.weight, ", gain, ")"),
    kaiming_uniform = paste0(
      "torch.nn.init.kaiming_uniform_(m.weight, ", a, ", '",
      match.arg(mode), "', '", match.arg(non_linearity), "')"),
    kaiming_normal = paste0(
      "torch.nn.init.kaiming_normal_(m.weight, ", a, ", '",
      match.arg(mode), "', '", match.arg(non_linearity), "')"),
    orthogonal = paste0("torch.nn.init.orthogonal_(m.weight, ", gain, ")")
  )
}

#' @title Get Torchtuples Callbacks
#' @description Helper function to return torchtuples callbacks from `torchtuples.callbacks`.
#' @param early_stopping `(logical(1))` \cr
#' If `TRUE` then constructs `torchtuples.callbacks,EarlyStopping`.
#' @param best_weights `(logical(1))`\cr
#' If `TRUE` then returns `torchtuples.callbacks.BestWeights`. Ignored if `early_stopping`
#' is `TRUE`.
#' @param min_delta `(numeric(1))`\cr
#' Passed to `torchtuples.callbacks.EarlyStopping`.
#' @param patience `(integer(1))`\cr
#' Passed to `torchtuples.callbacks.EarlyStopping`.
#'
#' @examples
#' \donttest{
#' if (requireNamespaces("reticulate")) {
#'   get_pycox_callbacks(early_stopping = TRUE)
#'
#'   get_pycox_callbacks(best_weights = TRUE)
#' }
#' }
#'
#' @export
get_pycox_callbacks <- function(early_stopping = FALSE, best_weights = FALSE,
                               min_delta = 0, patience = 10L) {

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' required but not installed.") # nocov
  }

  torchtuples <- reticulate::import("torchtuples")

  if (early_stopping) {
    return(reticulate::r_to_py(list(
      torchtuples$callbacks$EarlyStopping(min_delta = min_delta, patience = as.integer(patience))
    )))
  } else if (best_weights) {
    return(reticulate::r_to_py(list(torchtuples$callbacks$BestWeights())))
  } else {
    return(NULL)
  }

}

#' @title Install Pycox With Reticulate
#' @description Installs the python 'pycox' package via reticulate.
#' @param method,conda,pip See [reticulate::py_install].
#' @param install_torch If `TRUE` installs the dependency `torch` package as well.
#' @export
install_pycox <- function(method = "auto", conda = "auto", pip = FALSE, install_torch = FALSE) {
  # nocov start
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' required but not installed.") # nocov
  }

  pkg <- "pycox"
  if (install_torch) {
    pkg <- c("torch", pkg)
  }
  reticulate::py_install(pkg, method = method, conda = conda, pip = pip)
  # nocov end
}

#' @title Install Torch With Reticulate
#' @description Installs the python 'torch' package via reticulate.
#' @param method,conda,pip See [reticulate::py_install]
#' @export
install_torch <- function(method = "auto", conda = "auto", pip = FALSE) {
  # nocov start
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' required but not installed.")
  }
  reticulate::py_install("torch", method = method, conda = conda, pip = pip)
  # nocov end
}

#' @title Build a Pytorch Multilayer Perceptron
#' @description Utility function to build an MLP with a choice of activation function and weight
#' initialization with optional dropout and batch normalization.
#' @details This function is a helper for R users with less Python experience. Currently it is
#' limited to simple MLPs. More advanced networks will require manual creation with
#' \CRANpkg{reticulate}.
#'
#' @param n_in `(integer(1))`\cr Number of input features.
#' @param n_out `(integer(1))`\cr Number of targets.
#' @param nodes `(numeric())`\cr Hidden nodes in network, each element in vector represents number
#' of hidden nodes in respective layer.
#' @param activation `(character(1)|list())`\cr Activation function, can either be a single
#' character and the same function is used in all layers, or a list of length `length(nodes)`. See
#' [get_pycox_activation] for options.
#' @param act_pars `(list())`\cr Passed to [get_pycox_activation].
#' @param dropout `(numeric())`\cr Optional dropout layer, if `NULL` then no dropout layer added
#' otherwise either a single numeric which will be added to all layers or a vector of differing
#' drop-out amounts.
#' @param bias `(logical(1))`\cr If `TRUE` (default) then a bias parameter is added to all linear
#' layers.
#' @param batch_norm `(logical(1))`\cr If `TRUE` (default) then batch normalisation is applied
#' to all layers.
#' @param batch_pars `(list())`\cr Parameters for batch normalisation, see
#' `reticulate::py_help(torch$nn$BatchNorm1d)`.
#' @param init `(character(1))`\cr Weight initialization method. See
#' [get_pycox_init] for options.
#' @param init_pars `(list())`\cr Passed to [get_pycox_init].
#'
#' @examples
#' \donttest{
#' if (requireNamespaces("reticulate")) {
#'   build_pytorch_net(4L, 2L, nodes = c(32, 64, 32), activation = "selu")
#'
#'   # pass parameters to activation and initializer functions
#'   build_pytorch_net(4L, 2L, activation = "elu", act_pars = list(alpha = 0.1),
#'   init  = "kaiming_uniform", init_pars = list(mode = "fan_out"))
#' }
#' }
#'
#' @export
build_pytorch_net <- function(n_in, n_out, nodes = c(32, 32), activation = "relu",
                             act_pars = list(), dropout = 0.1,  bias = TRUE, batch_norm = TRUE,
                             batch_pars = list(eps = 1e-5, momentum = 0.1, affine = TRUE),
                             init = "uniform", init_pars = list()) {

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' required but not installed.") # nocov
  }

  torch <- reticulate::import("torch")
  nodes <- as.integer(nodes)
  n_in <- as.integer(n_in)
  n_out <- as.integer(n_out)
  nn <- torch$nn
  lng <- length(nodes)

  if (length(activation) == 1) {
    stopifnot(inherits(activation, "character"))
    activation <- rep(list(do.call(
      get_pycox_activation,
      c(list(
        activation = activation,
        construct = TRUE
      ), act_pars))), lng)
  } else {
    stopifnot(inherits(activation, "character") && length(activation) == lng)
    activation <- lapply(activation, function(x) {
      do.call(
      get_pycox_activation,
      c(list(
        activation = x,
        construct = TRUE), act_pars)
      )
    })
  }


  if (is.null(dropout) || length(dropout) == 1) {
    dropout <- rep(list(dropout), lng)
  } else {
    stopifnot(inherits(dropout, "numeric") && length(dropout) == lng)
  }

  add_module <- function(net, id, num_in, num_out, act, dropout) {
    # linear trafo

    net$add_module(paste0("L", id), nn$Linear(num_in, num_out, bias))
    # activation
    net$add_module(paste0("A", id), act)
    # batch normalisation
    if (batch_norm) {
      net$add_module(paste0("BN", id), do.call(nn$BatchNorm1d, c(list(num_features = num_out),
                                                                 batch_pars)))
    }
    # dropout layer
    if (!is.null(dropout)) {
      net$add_module(paste0("D", id), nn$Dropout(dropout))
    }

    return(net)
  }

  # input layer
  net <- nn$Sequential()
  add_module(net, 0, n_in, nodes[1], activation[[1]], dropout[[1]])

  # hidden layers
  for (i in seq_along(nodes)) {
    if (i < length(nodes)) {
      add_module(net, i, nodes[i], nodes[i + 1], activation[[i + 1]], dropout[[i + 1]])
    } else {
      # output layer
      net$add_module(as.character(length(nodes)), nn$Linear(nodes[i], n_out, bias))
    }
  }

  init <- do.call(get_pycox_init, c(list(init = init), init_pars))
  reticulate::py_run_string(
    paste0(
      "import torch
def init_weights(m):
      if type(m) == torch.nn.Linear:",
      init))
  net$apply(reticulate::py$init_weights)

  return(net)
}

#' @title Predict Method for pycox Neural Networks
#'
#' @description Predicted values from a fitted pycox ANN.
#'
#' @template return_predict
#'
#' @param object `(pycox(1))`\cr
#' Object of class inheriting from `"pycox"`.
#' @param newdata `(data.frame(1))`\cr
#' Testing data of `data.frame` like object, internally is coerced with [stats::model.matrix()].
#' If missing then training data from fitted object is used.
#' @param batch_size `(integer(1))`\cr
#' Passed to `pycox.models.X.fit`, elements in each batch.
#' @param num_workers `(integer(1))`\cr
#' Passed to `pycox.models.X.fit`, number of workers used in the dataloader.
#' @param interpolate `(logical(1))`\cr
#' For models `deephit` and `loghaz`, should predictions be linearly interpolated? Ignored
#' for other models.
#' @param inter_scheme `(character(1))`\cr
#' If `interpolate` is `TRUE` then the scheme for interpolation, see
#' `reticulate::py_help(py_help(pycox$models$DeepHitSingle$interpolate))` for further
#' details.
#' @param sub `(integer(1))`\cr
#' If `interpolate` is `TRUE` or model is `loghaz`, number of sub-divisions for interpolation.
#' See reticulate::py_help(py_help(pycox$models$DeepHitSingle$interpolate))` for further
#' details.
#' @param type (`character(1)`)\cr
#' Type of predicted value. Choices are survival probabilities over all time-points in training
#' data (`"survival"`) or a relative risk ranking (`"risk"`), which is the negative mean survival
#' time so higher rank implies higher risk of event, or both (`"all"`).
#' @param distr6 `(logical(1))`\cr
#' If `FALSE` (default) and `type` is `"survival"` or `"all"` returns matrix of survival
#' probabilities, otherwise returns a [distr6::VectorDistribution()].
#' @param ... `ANY` \cr
#' Currently ignored.
#'
#' @examples
#' \donttest{
#' if (requireNamespaces("reticulate")) {
#'   fit <- coxtime(data = simsurvdata(50))
#'
#'   # predict survival matrix and relative risks
#'   predict(fit, simsurvdata(10), type = "all")
#'
#'   # return as distribution
#'   if (requireNamespaces("distr6")) {
#'     predict(fit, simsurvdata(10), distr6 = TRUE)
#'   }
#' }
#' }
#'
#' @export
predict.pycox <- function(object, newdata, batch_size = 256L, num_workers = 0L,
                         interpolate = FALSE, inter_scheme = c("const_hazard", "const_pdf"),
                         sub = 10L, type = c("survival", "risk", "all"), distr6 = FALSE,
                         ...) {

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' required but not installed.") # nocov
  }

  # clean and convert data to float32
  newdata <- data.frame(clean_test_data(object, newdata))


  if (inherits(object, "coxtime") || inherits(object, "deepsurv")) {
    interpolate <- FALSE
    object$model$compute_baseline_hazards()
  } else if (inherits(object, "pchazard")) {
    interpolate <- FALSE
    try({object$model$sub <- as.integer(sub)}, silent = TRUE)
  }

  if (interpolate) {
    surv <- object$model$interpolate(
      sub = as.integer(sub),
      scheme = match.arg(inter_scheme)
    )

    surv <- surv$predict_surv_df(
      reticulate::r_to_py(newdata)$values$astype("float32"),
      batch_size = as.integer(batch_size),
      num_workers = as.integer(num_workers)
    )
  } else {
    surv <- object$model$predict_surv_df(
      reticulate::r_to_py(newdata)$values$astype("float32"),
      batch_size = as.integer(batch_size),
      num_workers = as.integer(num_workers)
    )
  }

  surv <- as.matrix(surv)
  ret <- list()
  stopifnot(nrow(newdata) == ncol(surv))

  type <- match.arg(type)
  if (type %in% c("survival", "all")) {
    if (!distr6 || !requireNamespace("distr6", quietly = TRUE)) {
      if (distr6) {
        warning("'distr6' not installed, returning 'surv' as matrix.") # nocov
      }
      ret$surv <- t(surv)
    } else {
      # cast to distr6
      times <- as.numeric(rownames(surv))
      surv <- rbind(1, surv, 0)
      rownames(surv) <- round(c(0, times, max(times) + 1e-3), 5)

      x <- rep(list(list(cdf = 0)), nrow(newdata))
      for (i in seq_len(nrow(newdata))) {
        # fix for infinite hazards - invalidate results for NaNs
        if (any(is.nan(surv[, i]))) {
          x[[i]]$cdf <- rep(1, numeric(length(x[[i]]$x))) # nocov - can't force this error
        } else {
          # fix rounding error bug
          x[[i]]$cdf <- sort(round(1 - surv[, i], 6))
        }
      }

      ret$surv <- distr6::VectorDistribution$new(
        distribution = "WeightedDiscrete",
        params = x,
        shared_params = list(x = round(as.numeric(rownames(surv)), 5)),
        decorators = c("CoreStatistics", "ExoticStatistics"))
    }
  }

  if (type %in% c("risk", "all")) {
    ret$risk <- -apply(1 - surv, 2, function(.x) sum(c(.x[1], diff(.x)) * as.numeric(rownames(surv))))
  }

  if (length(ret) == 1) {
    return(ret[[1]])
  } else {
    return(ret)
  }

}

.pycox_prep <- function(formula, data, time_variable, status_variable, x, y, reverse,
                      activation, frac, ...) {

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' required but not installed.") # nocov
  }

  pycox <- reticulate::import("pycox")
  torch <- reticulate::import("torch")
  torchtuples <- reticulate::import("torchtuples")

  data <- clean_train_data(formula, data, time_variable, status_variable, x, y, reverse)
  data$activation <- get_pycox_activation(activation, construct = FALSE)

  data <- c(data, pycox_prepare_train_data(data$x, data$y, frac, ...))

  return(data)
}
