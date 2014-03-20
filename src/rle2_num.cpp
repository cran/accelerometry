#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix rle2_num(NumericVector x, int n, int nmax, int indices) {
              
  if (indices==0) {
    
    if (nmax==-1) {
      
      int rows = 1;
      for (int a = 1; a < n; ++a)
        if (x(a)!=x(a-1)) rows += 1;
      NumericMatrix out(rows,2);
      int presval = x(0);
      int prespos = 0;
      int index = -1;
      for (int b = 1; b < n; ++b) {
        if (x(b)!=presval) {
          index += 1;
          out(index,0) = presval;
          out(index,1) = b-prespos;
          presval = x(b);
          prespos = b;
        }
      }
      index += 1;
      out(index,0) = presval;
      out(index,1) = n-prespos;
      return(out);
      
    }
    
    else {
      
      NumericMatrix out(nmax,2);
      int presval = x(0);
      int prespos = 0;
      int index = -1;
      for (int b = 1; b < n; ++b) {
        if (x(b)!=presval) {
          index += 1;
          if (index==nmax) break;
          out(index,0) = presval;
          out(index,1) = b-prespos;
          presval = x(b);
          prespos = b;
        }
      }
      if (index<nmax-1) {
        index += 1;
        out(index,0) = presval;
        out(index,1) = n-prespos;
      }
      
      if (index>=nmax-1) return(out);
    
      else {
            
        NumericMatrix out2(index+1,2);
        for (int c = 0; c < index+1; ++c) {
          out2(c,0) = out(c,0);
          out2(c,1) = out(c,1);
        }
        return(out2);
      }
    }
  }
  
  else {
    
    if (nmax==-1) {
      
      int rows = 1;
      for (int a = 1; a < n; ++a)
        if (x(a)!=x(a-1)) rows += 1;
      NumericMatrix out(rows,4);
      int presval = x(0);
      int prespos = 0;
      int index = -1;
      for (int b = 1; b < n; ++b) {
        if (x(b)!=presval) {
          index += 1;
          
          out(index,0) = presval;
          out(index,1) = prespos+1;
          out(index,2) = b;
          out(index,3) = b-prespos;
          presval = x(b);
          prespos = b;
        }
      }
      index += 1;
      out(index,0) = presval;
      out(index,1) = prespos+1;
      out(index,2) = n;
      out(index,3) = n-prespos;
      return(out);
      
    }
    
    else {
      
      NumericMatrix out(nmax,4);
      int presval = x(0);
      int prespos = 0;
      int index = -1;
      for (int b = 1; b < n; ++b) {
        if (x(b)!=presval) {
          index += 1;
          if (index==nmax) break;
          out(index,0) = presval;
          out(index,1) = prespos+1;
          out(index,2) = b;
          out(index,3) = b-prespos;
          presval = x(b);
          prespos = b;
        }
      }
      if (index<nmax-1) {
        index += 1;
        out(index,0) = presval;
        out(index,1) = prespos+1;
        out(index,2) = n;
        out(index,3) = n-prespos;
      }
      
      if (index>=nmax-1) return(out);
    
      else {
            
        NumericMatrix out2(index+1,4);
        for (int c = 0; c < index+1; ++c) {
          out2(c,0) = out(c,0);
          out2(c,1) = out(c,1);
          out2(c,2) = out(c,2);
          out2(c,3) = out(c,3);
        }
        return(out2);
      }
    }
  }
}
