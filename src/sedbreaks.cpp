#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int sedbreaks(IntegerVector counts, IntegerVector weartime, int thresh) {
  int n = counts.size();
  int sedbreaks = 0;
  for (int a = 0; a < n-1; ++a)
    if (weartime[a]==1 && weartime[a+1]==1 && counts[a]<thresh && counts[a+1]>=thresh) sedbreaks += 1;
  return(sedbreaks);
}
