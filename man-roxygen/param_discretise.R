#' @param cuts `(integer(1))`\cr
#' If `discretise` is `TRUE` then determines number of cut-points for discretisation.
#' @param cutpoints `(numeric())` \cr
#' Alternative to `cuts` if `discretise` is true, provide exact cutpoints for discretisation.
#' `cuts` is ignored if `cutpoints` is non-NULL.
#' @param scheme `(character(1))`\cr
#' Method of discretisation, either `"equidistant"` (default) or `"quantiles"`.
#' See `reticulate::py_help(pycox$models$LogisticHazard$label_transform)` for more detail.
#' @param cut_min `(integer(1))`\cr
#' Starting duration for discretisation, see
#' `reticulate::py_help(pycox$models$LogisticHazard$label_transform)` for more detail.
