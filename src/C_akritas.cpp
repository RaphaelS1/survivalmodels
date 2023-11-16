#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector C_Akritas(NumericMatrix truth, NumericVector predict_times,
                        NumericVector unique_times, NumericVector FX_train,
                        NumericVector FX_predict, double lambda) {

  double prod;
  double num;
  double den;
  double FXn;
  int pred_length = FX_predict.size();
  int train_length = FX_train.size();
  int new_times_length = predict_times.size();
  int unique_times_length = unique_times.size();

  NumericMatrix surv(pred_length, predict_times.size());

  for (int n = 0; n < pred_length; n++) {
    FXn = FX_predict[n];
    for (int i = 0; i < new_times_length; i++) {
      prod = 1;
      double current_time = predict_times[i];

      for (int t = 0; t < unique_times_length; t++) {
        num = 0;
        den = 0;
        double unique_time = unique_times[t];
        if (unique_time > current_time) {
          break;
        }
        for (int l = 0; l < train_length; l++) {
          double true_time = truth(l, 0);
          int true_status = truth(l, 1);
          double FX_train_l = FX_train[l];
          if (true_time < unique_time) {
            break;
          } else if (fabs(FXn - FX_train_l) <= lambda) {
            num += (true_time == unique_time) * true_status;
            den += 1;
          }
        }
        if (den != 0) {
          prod *= (1 - num/den);
        }
      };

      surv(n, i) = prod;
    }
  }

  return surv;
}
