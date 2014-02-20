#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix personvars(NumericMatrix dayvars, int rows, int days, int wk, int we) {
  NumericMatrix out(rows,200);
  int n = dayvars.nrow();
  int index = 0;
  out(0,0) = dayvars(0,0);
  if (dayvars(0,2)==1) {
    out(0,1) += 1;
    for (int a = 3; a < 68; ++a) 
      out(0,a+2) += dayvars(0,a);
    if (dayvars(0,1)==1 || dayvars(0,1)==7) {
      out(0,3) += 1;
      for (int a = 3; a < 68; ++a)
        out(0,a+132) += dayvars(0,a);
    }
    else {
      out(0,2) += 1;
      for (int a = 3; a < 68; ++a)
        out(0,a+67) += dayvars(0,a);
    }
  }
  for (int b = 1; b < n; ++b) {
    if (dayvars(b,0)!=dayvars(b-1,0)) {
      for (int a = 5; a < 70; ++a)
        out(index,a) = out(index,a)/out(index,1);
      for (int a = 70; a < 135; ++a)
        out(index,a) = out(index,a)/out(index,2);
      for (int a = 135; a < 200; ++a)
        out(index,a) = out(index,a)/out(index,3);
      if (out(index,1)>=days && out(index,2)>=wk && out(index,3)>=we) out(index,4) = 1;
      index +=  1;
      out(index,0) = dayvars(b,0);
    }
    if (dayvars(b,2)==1) {
      out(index,1) += 1;
      for (int a = 3; a < 68; ++a)
        out(index,a+2) += dayvars(b,a);
      if (dayvars(b,1)==1 || dayvars(b,1)==7) {
        out(index,3) += 1;
        for (int a = 3; a < 68; ++a)
          out(index,a+132) += dayvars(b,a);
      }
      else {
        out(index,2) += 1;
        for (int a = 3; a < 68; ++a)
          out(index,a+67) += dayvars(b,a);
      }
    }
  }
  if (out(index,1)>=days && out(index,2)>=wk && out(index,3)>=we) out(index,4) = 1;
  for (int a = 5; a < 70; ++a)
    out(index,a) = out(index,a)/out(index,1);
  for (int a = 70; a < 135; ++a)
    out(index,a) = out(index,a)/out(index,2);
  for (int a = 135; a < 200; ++a)
    out(index,a) = out(index,a)/out(index,3);
  return(out);
}
