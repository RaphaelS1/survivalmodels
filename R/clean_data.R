clean_train_data <- function(formula = NULL, data = NULL, time_variable = NULL,
                             status_variable = NULL, x = NULL, y = NULL, reverse = FALSE) {

  if (!is.null(x) | !is.null(y)) {
    if (is.null(x) | is.null(y)) {
      stop("Both 'x' and 'y' must be provided if either non-NULL.")
    } else {
      if (is.null(ncol(x))) {
        stop("'x' should be a data.frame like object.")
      }
    }
    stopifnot(inherits(y, "Surv"))
  } else if (!is.null(formula)) {
    f <- stats::as.formula(formula, env = data)
    y <- eval(f[[2]], envir = data)
    stopifnot(inherits(y, "Surv"))

    if (deparse(f[[3]]) == ".") {
      if (is.null(data)) {
        stop("'.' in formula and no 'data' argument")
      } else {
        x <- data[, setdiff(colnames(data), c(deparse(f[[2]][[2]]), deparse(f[[2]][[3]]))),
                  drop = FALSE
        ]
      }
    } else {
      x <- data[, strsplit(deparse(f[[3]]), " + ", TRUE)[[1]], drop = FALSE]
    }
  } else if (!is.null(time_variable) | !is.null(status_variable)) {
    if (is.null(time_variable) | is.null(status_variable) | is.null(data)) {
      stop("'time_variable', 'status_variable', and 'data' must be provided if either 'time_variable' or 'status_variable' non-NULL.") # nolint
    } else {
      stopifnot(time_variable %in% colnames(data))
      stopifnot(status_variable %in% colnames(data))
      x <- data[, setdiff(colnames(data), c(time_variable, status_variable)), drop = FALSE]
      y <- data.frame(time = data[, time_variable], status = data[, status_variable])
    }
  }

  y <- as.matrix(y)
  x <- stats::model.matrix(~., x)[, -1, drop = FALSE]

  if (reverse) {
    y[, 2] <- 1 - y[, 2]
  }

  return(list(x = x, y = y))
}

clean_test_data <- function(object, newdata) {
  if (missing(newdata)) {
    newdata <- object$x
  } else {
    newdata <- stats::model.matrix(~., newdata)[, -1, drop = FALSE]
  }

  ord <- match(colnames(newdata), colnames(object$x), nomatch = NULL)
  newdata <- newdata[, !is.na(ord), drop = FALSE]
  newdata <- newdata[, ord[!is.na(ord)], drop = FALSE]
  if (!all(suppressWarnings(colnames(newdata) == colnames(object$x)))) {
    stop(sprintf(
      "Names in newdata should be identical to {%s}.",
      paste0(colnames(object$x), collapse = ", ")
    ))
  }

  return(newdata)
}
