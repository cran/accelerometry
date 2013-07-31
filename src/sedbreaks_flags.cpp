#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector sedbreaks_flags(IntegerVector counts, IntegerVector weartime, int thresh) {
  int n = counts.size();
  IntegerVector out(n);
  for (int a = 0; a < n-1; ++a)
    if (weartime[a]==1 && weartime[a+1]==1 && counts[a]<thresh && counts[a+1]>=thresh) out[a+1]=1;
  return(out);
}
