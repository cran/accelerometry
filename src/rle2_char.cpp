#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
CharacterMatrix rle2_char(CharacterVector x, int nmax) {
  int n = x.size();
      
  if (nmax==-1) {

    int rows = 1;
    for (int a = 1; a < n; ++a)
      if (x(a)!=x(a-1)) rows+=1;
    NumericMatrix diffs(rows,3);
    CharacterMatrix out(rows,4);
    int counter = 0;
    diffs(counter,0) = 1;
    out(counter,0) = x(0);
    out(counter,1) = 1;

    if (rows==1) {
      out(counter,2) = n;
      out(counter,3) = n;
    }

    else {
      for (int b = 1; b < n-1; ++b) {
        if (x(b)!=x(b-1)) {
          diffs(counter,1) = b;
          diffs(counter,2) = diffs(counter,1)-diffs(counter,0)+1;
          out(counter,2) = b;
          out(counter,3) = diffs(counter,2);
          counter += 1;
          diffs(counter,0) = b+1;
          out(counter,0) = x(b);
          out(counter,1) = b+1;
        }
      }

      if (x(n-1)==x(n-2)) {
        diffs(counter,1) = n;
        diffs(counter,2) = diffs(counter,1)-diffs(counter,0)+1;
        out(counter,2) = n;
        out(counter,3) = diffs(counter,2);
      }
      else {
        diffs(counter,1) = n-1;
        diffs(counter,2) = diffs(counter,1)-diffs(counter,0)+1;
        out(counter,2) = n-1;
        out(counter,3) = diffs(counter,2);
        counter += 1;
        out(counter,0) = x(n-1);
        out(counter,1) = n;
        out(counter,2) = n;
        out(counter,3) = 1;
      }
    }
              
    return(out);
  
  }

  else {
            
    NumericMatrix diffs(nmax,3);
    CharacterMatrix out1(nmax,4);
    int counter = 0;
    diffs(counter,0) = 1;
    out1(counter,0) = x(0);
    out1(counter,1) = 1;
        
    for (int b = 1; b < n-1; ++b) {
      if (x(b)!=x(b-1)) {
        diffs(counter,1) = b;
        diffs(counter,2) = diffs(counter,1)-diffs(counter,0)+1;
        out1(counter,2) = b;
        out1(counter,3) = diffs(counter,2);
        counter += 1;
        if (counter==nmax) break;
        diffs(counter,0) = b+1;
        out1(counter,0) = x(b);
        out1(counter,1) = b+1;
      }
    }
            
    if (counter<nmax) {
            
      if (x(n-1)==x(n-2)) {
        diffs(counter,1) = n;
        diffs(counter,2) = diffs(counter,1)-diffs(counter,0)+1;
        out1(counter,2) = n;
        out1(counter,3) = diffs(counter,2);
      }
      else {
        diffs(counter,1) = n-1;
        diffs(counter,2) = diffs(counter,1)-diffs(counter,0)+1;
        out1(counter,2) = n-1;
        out1(counter,3) = diffs(counter,2);
        counter += 1;
        if (counter!=nmax) {
          out1(counter,0) = x(n-1);
          out1(counter,1) = n;
          out1(counter,2) = n;
          out1(counter,3) = 1;
        }
      }
    }
            
    if (counter>=nmax-1) return(out1);
            
    else {
            
      CharacterMatrix out2(counter+1,4);
      for (int c = 0; c < counter+1; ++c) {
        out2(c,0) = out1(c,0);
        out2(c,1) = out1(c,1);
        out2(c,2) = out1(c,2);
        out2(c,3) = out1(c,3);
      }
  
      return(out2);
      
    }       
  }
}
