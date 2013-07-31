#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector artifacts(IntegerVector counts, int thresh) {
  int n = counts.size();
  IntegerVector out(n);
  int before = -1;
  int after = -1;
  if (counts[0]>=thresh) {
    for (int a = 1; a < n; ++a) {
      if (counts[a]<thresh) {
        out[0] = counts[a];
        break;
      }
    }
  }
  else out[0] = counts[0];
  for (int b = 1; b < n; ++b) {
    before = -1;
    after = -1;
    if (counts[b]>=thresh) {
      for (int c = b-1; c >= 0; --c) {
        if (counts[c]<thresh) {
          before = counts[c];
          break;
        }
      }
      for (int d = b+1; d < n; ++d) {
        if (counts[d]<thresh) {
          after = counts[d];
          break;
        }
      }
      if (before>-1 && after>-1) {
        if ((before+after)%2==0) out[b] = (before+after)/2;
        else out[b] = (before+after+1)/2;
      }
      else if (before==-1) out[b] = after;
      else if (after==-1) out[b] = before;
    }
    else out[b] = counts[b];
  }
  return(out);
}
