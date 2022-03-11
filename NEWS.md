# survivalmodels 0.1.12

* Exported `surv_to_risk` for reducing survival matrix distributions to relative risks / rankings

# survivalmodels 0.1.11

* Better handling of improper distributions
* Fixes bug in Python conversion

# survivalmodels 0.1.10

* Removed deprecated `cutoff` parameter from `simsurvdata`
* Update risk calculations to follow https://arxiv.org/abs/2112.04828
* Minor internal changes

# survivalmodels 0.1.9

* Added tutorial with survivalmodels and mlr3proba
* Bugfix for distr6 1.6.0

# survivalmodels 0.1.8

* Add example of custom model to `DNNSurv`
* Change default of `pip` to `TRUE` in `install_` functions

# survivalmodels 0.1.7

* Fix export of `set_seed` and `cindex`

# survivalmodels 0.1.6

* Add `set_seed` for easier setting of seeds within R and Python environments

# survivalmodels 0.1.5

* Fixed bug in `risk` return type when `distr6 = FALSE`
* Added `cindex` for evaluating concordance of risk

# survivalmodels 0.1.4

* Improved `risk` return type for discrimination

# survivalmodels 0.1.3

* Previous `risk` predictions returned values where lower ranks implied higher risk, now higher rank implies higher risk. Calculated as the negative mean survival time.

# survivalmodels 0.1.2

* Minor patch for CRAN

# survivalmodels 0.1.1

* Patch for clang-ASAN, gcc-ASAN, valgrind

# survivalmodels 0.1.0

- Initial release
