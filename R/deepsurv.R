#' @description DeepSurv neural fits a neural network based on the partial likelihood from
#' a Cox PH.
#'
#' @template pycox
#' @templateVar call CoxPH
#' @templateVar name DeepSurv
#' @templateVar fun deepsurv
#'
#' @template param_traindata
#' @template return_train
#'
#' @references
#' Katzman, J. L., Shaham, U., Cloninger, A., Bates, J., Jiang, T., & Kluger, Y. (2018).
#' DeepSurv: personalized treatment recommender system using a Cox proportional hazards deep neural
#' network.
#' BMC Medical Research Methodology, 18(1), 24. https://doi.org/10.1186/s12874-018-0482-1
#'
#'
#' @examples
#' \donttest{
#' if (requireNamespaces("reticulate")) {
#'   # all defaults
#'   deepsurv(data = simsurvdata(50))
#'
#'   # common parameters
#'   deepsurv(data = simsurvdata(50), frac = 0.3, activation = "relu",
#'     num_nodes = c(4L, 8L, 4L, 2L), dropout = 0.1, early_stopping = TRUE, epochs = 100L,
#'     batch_size = 32L)
#' }
#' }
#'
#' @export
deepsurv <- function(formula = NULL, data = NULL, reverse = FALSE,
                    time_variable = "time", status_variable = "status",
                    x = NULL, y = NULL, frac = 0,
                    activation = "relu", num_nodes = c(32L, 32L), batch_norm = TRUE,
                    dropout = NULL, device = NULL, early_stopping = FALSE,
                    best_weights = FALSE,  min_delta = 0, patience = 10L, batch_size = 256L,
                    epochs = 1L, verbose = FALSE, num_workers = 0L, shuffle = TRUE, ...) {

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' required but not installed.") # nocov
  }

  call <- match.call()

  data <- .pycox_prep(formula, data, time_variable, status_variable, x, y, reverse, activation,
                     frac)

  pycox <- reticulate::import("pycox")
  torchtuples <- reticulate::import("torchtuples")

  net <- torchtuples$practical$MLPVanilla(
    in_features = data$x_train$shape[1],
    num_nodes = reticulate::r_to_py(as.integer(num_nodes)),
    out_features = 1L,
    activation = data$activation,
    output_bias = FALSE,
    batch_norm = batch_norm,
    dropout = dropout
  )

  # Get optimizer and set-up model
  model <- pycox$models$CoxPH(
    net = net,
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

  structure(list(y = data$y, x = data$x,
                 xnames = colnames(data$x),
                 model = model,
                 call = call),
            name = "DeepSurv Neural Network",
            class = c("deepsurv", "pycox", "survivalmodel")
  )
}
