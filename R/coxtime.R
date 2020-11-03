#' @description Cox-Time fits a neural network based on the Cox PH with
#'  time-varying effects.
#'
#' @template pycox
#' @templateVar call Coxtime
#' @templateVar name Cox-Time
#' @templateVar fun coxtime
#'
#' @param standardize_time `(logical(1))`\cr
#' If `TRUE`, the time outcome is standardized.
#' @param log_duration `(logical(1))`\cr
#' If `TRUE` and `standardize_time` is `TRUE` then time variable is log transformed.
#' @param with_mean `(logical(1))`\cr
#' If `TRUE` (default) and `standardize_time` is `TRUE` then time variable is centered.
#' @param with_std `(logical(1))`\cr
#' If `TRUE` (default) and `standardize_time` is `TRUE` then time variable is scaled to unit
#' variance.
#' @param shrink `(numeric(1))`\cr
#' Passed to `pycox.models.Coxtime`, shrinkage parameter for regularization.
#'
#' @template param_traindata
#' @template return_train
#'
#' @references
#' Kvamme, H., Borgan, Ø., & Scheel, I. (2019).
#' Time-to-event prediction with neural networks and Cox regression.
#' Journal of Machine Learning Research, 20(129), 1–30.
#'
#'
#' @examples
#' \donttest{
#' if (requireNamespaces("reticulate")) {
#'   # all defaults
#'   coxtime(data = simsurvdata(50))
#'
#'   # common parameters
#'   coxtime(data = simsurvdata(50), frac = 0.3, activation = "relu",
#'     num_nodes = c(4L, 8L, 4L, 2L), dropout = 0.1, early_stopping = TRUE, epochs = 100L,
#'     batch_size = 32L)
#' }
#' }
#'
#' @export
coxtime <- function(formula = NULL, data = NULL, reverse = FALSE,
                    time_variable = "time", status_variable = "status",
                    x = NULL, y = NULL, frac = 0, standardize_time = FALSE,
                    log_duration = FALSE, with_mean = TRUE, with_std = TRUE,
                    activation = "relu", num_nodes = c(32L, 32L), batch_norm = TRUE,
                    dropout = NULL, device = NULL, shrink = 0, early_stopping = FALSE,
                    best_weights  = FALSE,  min_delta = 0, patience = 10L, batch_size = 256L,
                    epochs = 1L, verbose = FALSE, num_workers = 0L, shuffle = TRUE, ...) {

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' required but not installed.") # nocov
  }

  call <- match.call()

  data <- .pycox_prep(formula = formula, data = data, time_variable = time_variable,
                      status_variable = status_variable, x = x, y = y, reverse = reverse,
                      activation = activation, frac = frac, standardize_time = standardize_time,
                      log_duration = log_duration, with_mean = with_mean, with_std = with_std)

  pycox <- reticulate::import("pycox")

  net <- pycox$models$cox_time$MLPVanillaCoxTime(
    in_features = data$x_train$shape[1],
    num_nodes = reticulate::r_to_py(as.integer(num_nodes)),
    activation = data$activation,
    batch_norm = batch_norm,
    dropout = dropout
  )

  # Get optimizer and set-up model
  model <- pycox$models$CoxTime(
    net = net,
    labtrans = data$labtrans,
    optimizer = get_pycox_optim(net = net, ...),
    device = device,
    shrink = shrink
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
            name = "Cox-Time Neural Network",
            class = c("coxtime", "pycox", "survivalmodel")
  )
}
