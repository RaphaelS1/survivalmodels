#' @description DeepHit fits a neural network based on the PMF of a discrete Cox model. This is
#' the single (non-competing) event implementation.
#'
#' @template pycox
#' @templateVar call DeepHitSingle
#' @templateVar name DeepHit
#' @templateVar fun deephit
#'
#' @template param_customnet
#' @template param_discretise
#' @template param_traindata
#' @template return_train
#'
#' @param mod_alpha `(numeric(1))`\cr
#' Weighting in (0,1) for combining likelihood (L1) and rank loss (L2). See reference and
#' `py_help(pycox$models$DeepHitSingle)` for more detail.
#' @param sigma `(numeric(1))`\cr
#' From eta in rank loss (L2) of ref. See reference and
#' `py_help(pycox$models$DeepHitSingle)` for more detail.
#'
#' @references
#' Changhee Lee, William R Zame, Jinsung Yoon, and Mihaela van der Schaar.
#' Deephit: A deep learning approach to survival analysis with competing risks.
#' In Thirty-Second AAAI Conference on Artificial Intelligence, 2018.
#' http://medianetlab.ee.ucla.edu/papers/AAAI_2018_DeepHit
#'
#'
#' @examples
#' \donttest{
#' if (requireNamespaces("reticulate")) {
#'   # all defaults
#'   deephit(data = simsurvdata(50))
#'
#'   # common parameters
#'   deephit(data = simsurvdata(50), frac = 0.3, activation = "relu",
#'     num_nodes = c(4L, 8L, 4L, 2L), dropout = 0.1, early_stopping = TRUE, epochs = 100L,
#'     batch_size = 32L)
#' }
#' }
#'
#' @export
deephit <- function(formula = NULL, data = NULL, reverse = FALSE,
                    time_variable = "time", status_variable = "status",
                    x = NULL, y = NULL, frac = 0, cuts = 10, cutpoints = NULL,
                    scheme = c("equidistant", "quantiles"), cut_min = 0,
                    activation = "relu", custom_net = NULL,
                    num_nodes = c(32L, 32L), batch_norm = TRUE,
                    dropout = NULL, device = NULL, mod_alpha = 0.2,
                    sigma = 0.1, early_stopping = FALSE,
                    best_weights = FALSE,  min_delta = 0, patience = 10L, batch_size = 256L,
                    epochs = 1L, verbose = FALSE, num_workers = 0L, shuffle = TRUE, ...) {

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' required but not installed.") # nocov
  }

  call <- match.call()

  data <- .pycox_prep(formula, data, time_variable, status_variable, x, y, reverse, activation,
                      frac = frac, discretise = TRUE, model = "deephit", cuts = cuts,
                      cutpoints = cutpoints, scheme = match.arg(scheme), cut_min = cut_min)

  pycox <- reticulate::import("pycox")
  torchtuples <- reticulate::import("torchtuples")

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
  model <- pycox$models$DeepHitSingle(
    net = net,
    duration_index = data$labtrans$cuts,
    optimizer = get_pycox_optim(net = net, ...),
    sigma = sigma,
    device = device,
    alpha = mod_alpha
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

  structure(list(y = data$y, x = data$x,
                 xnames = colnames(data$x),
                 model = model,
                 call = call),
            name = "DeepHit Neural Network",
            class = c("deephit", "pycox", "survivalmodel")
  )
}
