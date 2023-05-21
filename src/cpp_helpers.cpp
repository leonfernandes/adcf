// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;

//  [[Rcpp::export]]
arma::vec cppColSum_sqrt(arma::mat A)
{
    return (arma::sqrt(arma::sum(A, 1)));
}

//  [[Rcpp::export]]
double dist_to_dcov(arma::mat x, arma::mat y) {
    int n = size(x)[0];
    arma::vec col_sum_x = arma::sum(x, 1) / n;
    arma::vec col_sum_y = arma::sum(y, 1) / n;
    double sum_x = arma::sum(col_sum_x) / n;
    double sum_y = arma::sum(col_sum_y) / n;
    double sum_xy = arma::dot(col_sum_x, col_sum_y) / n;
    double tr_xy = arma::accu(x % y) / (n * n);
    return(tr_xy + sum_x*sum_y - 2*sum_xy);
}