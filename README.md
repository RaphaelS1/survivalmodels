survivalmodels
================

[![CRAN Status
Badge](https://www.r-pkg.org/badges/version-ago/survivalmodels)](https://cran.r-project.org/package=survivalmodels)
[![CRAN
Checks](https://cranchecks.info/badges/worst/survivalmodels)](https://cran.r-project.org/web/checks/check_results_survivalmodels.html)
[![tic](https://github.com/RaphaelS1/survivalmodels/workflows/tic/badge.svg)](https://github.com/RaphaelS1/survivalmodels/actions)
[![CodeFactor](https://www.codefactor.io/repository/github/raphaels1/survivalmodels/badge)](https://www.codefactor.io/repository/github/raphaels1/survivalmodels)

[![Repo
Status](https://www.repostatus.org/badges/latest/active.svg)](https://github.com/RaphaelS1/survivalmodels)
[![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://github.com/RaphaelS1/survivalmodels)

[![CRAN
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/survivalmodels)](https://cran.r-project.org/package=survivalmodels)
[![codecov](https://codecov.io/gh/RaphaelS1/survivalmodels/branch/master/graph/badge.svg)](https://codecov.io/gh/RaphaelS1/survivalmodels)
[![dependencies](https://tinyverse.netlify.com/badge/survivalmodels)](https://CRAN.R-project.org/package=survivalmodels)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## What is survivalmodels?

`survivalmodels` implements models for survival analysis that are either
not already implemented in R, or novel implementations for speed
improvements. Currently implemented are five neural networks from the
Python packages [pycox](https://github.com/havakv/pycox), DNNSurv, and
the Akritas non-parametric conditional estimator. Further updates will
include implementations of novel survival models.

## Python Models

`survivalmodels` implements models from Python using
[reticulate](https://cran.r-project.org/package=reticulate). In order to
use these models, the required Python packages must be installed
following with
[reticulate::py\_install](https://rstudio.github.io/reticulate/reference/py_install.html).
`survivalmodels` includes a helper function to install the required
`pycox` function (with pytorch if also required). Before running any
models in this package, if you have not already installed `pycox` please
run

``` r
install_pycox(pip = TRUE, install_torch = FALSE)
```

With the arguments changed as you require, see
[?install\_pycox](https://raphaels1.github.io/survivalmodels/reference/install_pycox.html)
for more.

For `DNNSurv` the model depends on `keras` and `tensorflow`, which
require installation via:

``` r
install_keras(pip = TRUE, install_tensorflow = FALSE)
```

## Installation

Install the latest release from CRAN:

``` r
install.packages("survivalmodels")
```

Install the development version from GitHub:

``` r
remotes::install_github("RaphaelS1/survivalmodels")
```
