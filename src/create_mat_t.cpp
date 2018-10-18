#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector create_mat_t(NumericMatrix &mat, NumericVector &a_tm1, NumericVector &d, double retention) {
  int n_row = mat.nrow(), n_col = mat.ncol();
  NumericMatrix mat_t(n_row, n_col);
  for(int i = 0; i < n_row; ++i){
    NumericVector row = mat(i,_);
    for(int j = 0; j < n_col; ++j){
      if(i == j){
        mat_t(i,j) = retention * a_tm1[j];
        } else {
        if(d[j] == 0) {
          mat_t(i,j) = 0;
          } else {
          mat_t(j,i) = (1 - retention) * row[j] * (a_tm1[j]/d[j]);
          }
        }
      }
    }
  return mat_t;
  }


