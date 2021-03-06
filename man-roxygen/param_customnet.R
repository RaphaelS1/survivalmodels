#' @param custom_net `(torch.nn.modules.module.Module(1))`\cr
#' Optional custom network built with [build_pytorch_net], otherwise default architecture used.
#' Note that if building a custom network the number of output channels depends on `cuts` or
#' `cutpoints`.
