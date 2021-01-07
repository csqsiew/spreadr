#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector create_mat_t(NumericMatrix &mat, NumericVector &a_tm1, NumericVector &d, NumericVector &retention) {
  int n_row = mat.nrow(), n_col = mat.ncol();
  NumericMatrix mat_t(n_row, n_col);

  // The (i, j) entry represents the activation going from i to j.
  for (int i = 0; i < n_row; ++i) {
    for (int j = 0; j < n_col; ++j) {
      if (i == j) {
        mat_t(i,j) = d[i] == 0 ? a_tm1[i] : retention[i] * a_tm1[i];
      } else if (d[i] == 0) {
        mat_t(i,j) = 0;
      } else {
        mat_t(i,j) = (1 - retention[i]) * mat(i,j) * (a_tm1[i]/d[i]);
      }
    }
  }
  return mat_t;
}
