#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
bool C_assert_increasing_surv(NumericMatrix x) {

  for (int i = 0; i < x.nrow(); i++) {
    if (x(i, 0) < 0 || x(i, 0) > 1) {
      return false;
    }

    for (int j = 1; j < x.ncol(); j++) {
      if (x(i, j) < 0 || x(i, j) > 1) {
        return false;
      }

      if (x(i, j) > x(i, j - 1)) {
        return false;
      }
    }
  }

  return true;
}
