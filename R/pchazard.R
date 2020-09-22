#' @description Logistic-Hazard fits a discrete neural network based on a cross-entropy loss
#' and predictions of a discrete hazard function, also known as Nnet-Survival.
#'
#' @template pycox
#' @templateVar call PCHazard
#' @templateVar name PC-Hazard
#' @templateVar fun pchazard
#'
#' @template param_customnet
#' @template param_discretise
#'
#' @param reduction `(character(1))`\cr
#' How to reduce the loss, see to `reticulate::py_help(pycox$models$loss$NLLPCHazardLoss)`.
#'
#' @references
#' Kvamme, H., & Borgan, Ø. (2019).
#' Continuous and discrete-time survival prediction with neural networks.
#' https://doi.org/arXiv:1910.06724.
#'
#' @export
pchazard <- function(formula = NULL, data = NULL, reverse = FALSE,
                   time_variable = NULL, status_variable = NULL,
                   x = NULL, y = NULL, frac = 0, cuts = 10, cutpoints = NULL,
                   scheme = c("equidistant", "quantiles"), cut_min = 0,
                   activation = "relu", custom_net = NULL,
                   num_nodes = c(32L, 32L), batch_norm = TRUE,
                   reduction = c("mean", "none", "sum"),
                   dropout = NULL, device = NULL, early_stopping = FALSE,
                   best_weights = FALSE,  min_delta = 0, patience = 10L, batch_size = 256L,
                   epochs = 1L, verbose = TRUE, num_workers = 0L, shuffle = TRUE, ...) {

  call <- match.call()

  data <- .pycox_prep(formula, data, time_variable, status_variable, x, y, reverse, activation,
                      frac = frac, discretise = TRUE, model = "pchazard", cuts = cuts,
                      cutpoints = cutpoints, scheme = match.arg(scheme), cut_min = cut_min)

  if (!is.null(custom_net)) {
    net <- custom_net
  } else {
    net <- torchtuples$practical$MLPVanilla(
      in_features = data$x_train$shape[1],
      num_nodes = reticulate::r_to_py(as.integer(num_nodes)),
      activation = data$activation,
      out_features = data$labtrans$out_features,
      batch_norm = batch_norm,
      dropout = dropout
    )
  }

  # Get optimizer and set-up model
  model = pycox$models$PCHazard(
    net = net,
    duration_index = data$labtrans$cuts,
    loss = pycox$models$loss$NLLPCHazardLoss(match.arg(reduction)),
    optimizer = get_pycox_optim(net = net, ...),
    device = device
  )

  model$fit(
    input = data$x_train,
    target = data$y_train,
    callbacks = get_pycox_callbacks(early_stopping, best_weights, min_delta, patience),
    val_data = data$val,
    batch_size = as.integer(batch_size),
    epochs = as.integer(epochs),
    verbose = verbose,
    num_workers = as.integer(num_workers),
    shuffle = shuffle
  )

  structure(list(y = data$y_train, x = data$x_train,
                 xnames = colnames(data$x_train),
                 model = model,
                 call = call),
            name = "PC-Hazard Neural Network",
            class = c("pchazard", "pycox", "survivalmodel")
  )
}
