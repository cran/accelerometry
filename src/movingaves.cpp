#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector movingaves(NumericVector x, int window) {
  int n = x.size();
  NumericVector out(n-window+1);
  double sum = 0;
  NumericVector current(window);
  for (int a = 0; a < window; ++a) {
    current[a] = x[a];
    sum += x[a];
  }
  double ave = sum/window;
  out[0]=ave;
  for (int b = window; b < n; ++b) {
    sum = sum + x[b]-x[b-window];
    ave = sum/window;
    out[b-window+1]=ave;
  }
  return(out);
}
