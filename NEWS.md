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
