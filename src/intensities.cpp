#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector intensities(NumericVector counts, IntegerVector thresh) {
  int n = counts.size();
  NumericVector out(16);
  for (int a = 0; a <n; ++a) {
    if (counts[a]<thresh[0]) {
      out[0] += 1;
      out[8] += counts[a];
    }
    else if (counts[a]>=thresh[0] && counts[a]<thresh[1]) {
      out[1] += 1;
      out[5] += 1;
      out[7] += 1;
      out[9] += counts[a];
      out[13] += counts[a];
      out[15] += counts[a];
    }
    else if (counts[a]>=thresh[1] && counts[a]<thresh[2]) {
      out[2] += 1;
      out[5] += 1;
      out[7] += 1;
      out[10] += counts[a];
      out[13] += counts[a];
      out[15] += counts[a];
    }
    else if (counts[a]>=thresh[2] && counts[a]<thresh[3]) {
      out[3] += 1;
      out[6] += 1;
      out[7] += 1;
      out[11] += counts[a];
      out[14] += counts[a];
      out[15] += counts[a];
    }
    else if (counts[a]>=thresh[3]) {
      out[4] += 1;
      out[6] += 1;
      out[7] += 1;
      out[12] += counts[a];
      out[14] += counts[a];
      out[15] += counts[a];
    }
  }
  return(out);
}
