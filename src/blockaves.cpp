#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector blockaves(NumericVector x, int window) {
  int n = x.size();
  NumericVector out(n/window);
  float sum = 0;
  int index = 0;
  for (int a = 0; a < n; ++a) {
    sum += x[a];
    if ((a+1)%window==0) {
      out[index] = sum/window;
      index += 1;
      sum = 0;
    }
  }              
  return(out);
}
