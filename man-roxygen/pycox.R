#' @title <%=name%> Survival Neural Network
#' @details Implemented from the `pycox` Python package via \CRANpkg{reticulate}.
#' Calls `<%=paste0("pycox.models.", call) %>`.
#'
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
