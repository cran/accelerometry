#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
float movingaves_max(NumericVector x, int window) {
  int n = x.size();
  float sum = 0;
  NumericVector current(window);
  for (int a = 0; a < window; ++a) {
    current[a] = x[a];
    sum += x[a];
  }
  float max=sum;
  for (int b = window; b < n; ++b) {
    sum = sum + x[b] - x[b-window];
    if (sum>max) max=sum;
  }
  max = max/window;
  return(max);
}
