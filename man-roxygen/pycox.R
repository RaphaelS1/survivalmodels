#' @title <%=name%> Survival Neural Network
#' @details Implemented from the `pycox` Python package via \CRANpkg{reticulate}.
#' Calls `<%=paste0("pycox.models.", call) %>`.
#'
#' @param formula `(formula(1))`\cr
#' Object specifying the model fit, left-hand-side of formula should describe a [survival::Surv()]
#' object.
#' @param data `(data.frame(1))`\cr
#' Training data of `data.frame` like object, internally is coerced with [stats::model.matrix()].
#' @param reverse `(logical(1))`\cr
#' If `TRUE` fits estimator on censoring distribution, otherwise (default) survival distribution.
#' @param time_variable `(character(1))`\cr
#' Alternative method to call the function. Name of the 'time' variable, required if `formula`.
#' or `x` and `Y` not given.
#' @param status_variable `(character(1))`\cr
#' Alternative method to call the function. Name of the 'status' variable, required if `formula`
#' or `x` and `Y` not given.
#' @param x `(data.frame(1))`\cr
#' Alternative method to call the function. Required if `formula, time_variable` and
#' `status_variable` not given. Data frame like object of features which is internally
#' coerced with `model.matrix`.
#' @param y `([survival::Surv()])`\cr
#' Alternative method to call the function. Required if `formula, time_variable` and
#' `status_variable` not given. Survival outcome of right-censored observations.
#' @param frac \cr
#' See [pycox_prepare_train_data].
#' @param activation \cr
#' See [get_pycox_activation].
#' @param num_nodes,batch_norm,dropout \cr
#' See [build_pytorch_net].
#' @param early_stopping,best_weights,min_delta,patience \cr
#' See [get_pycox_callbacks].
#' @param device `(integer(1)/character(1))`\cr
#' Passed to `<%=paste0("pycox.models.", call) %>`, specifies device to compute models on.
#' @param batch_size `(integer(1))`\cr
#' Passed to `<%=paste0("pycox.models.", call, ".fit") %>`, elements in each batch.
#' @param epochs `(integer(1))`\cr
#' Passed to `<%=paste0("pycox.models.", call, ".fit") %>`, number of epochs.
#' @param verbose `(logical(1))`\cr
#' Passed to `<%=paste0("pycox.models.", call, ".fit") %>`, should information be displayed during
#' fitting.
#' @param num_workers `(integer(1))`\cr
#' Passed to `<%=paste0("pycox.models.", call, ".fit") %>`, number of workers used in the
#' dataloader.
#' @param shuffle `(logical(1))`\cr
#' Passed to `<%=paste0("pycox.models.", call, ".fit") %>`, should order of dataset be shuffled?
#' @param ... `ANY` \cr
#' Passed to [get_pycox_optim].
#'
#' @return An object inheriting from class `<%=fun%>`.
