#' @return A `numeric` if `type = "risk"`, a [distr6::VectorDistribution()] (if `distr6 = TRUE`)
#' and `type = "survival"`; a `matrix` if (`distr6 = FALSE`) and `type = "survival"` where
#' entries are survival probabilities with rows of observations and columns are time-points;
#' or a list combining above if `type = "all"`.
