#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector weartime(NumericVector counts, int window, int tol, int tol_upper, int nci, int days_distinct) {
  int n = counts.size();
  IntegerVector out(n);
  for (int a = 0; a < n; ++a) out[a] = 1;
  
  if (days_distinct==0) {
    
    if (tol==0) {
      
      int zeros = 0;
      for (int b = 0; b < n; ++b) {
        if (counts[b]==0) zeros +=1;
        else {
          if (zeros>=window)
            for (int c = b-zeros; c < b; ++c) out[c] = 0;
          zeros = 0;
        }
        if (b==n-1 && zeros>=window)
          for (int d = b-zeros+1; d < b+1; ++d) out[d] = 0;
      }
      
    }
    else if (tol>0) {
      
      if (nci==0) {
        
        IntegerVector status(n);
        for (int b = 0; b < n; ++b) {
          if (counts[b]==0) status[b] = 0;
          else if (counts[b]<=tol_upper) status[b] = 1;
          else if (counts[b]>tol_upper) status[b] = tol+1;
        }
        int sum = 0;
        for (int c = 0; c < window; ++c)
          sum += status[c];
        if (sum<=tol)
          for (int d = 0; d < window; ++d) out[d] = 0;
        for (int e = window; e < n; ++e) {
          sum = sum-status[e-window]+status[e];
          if (sum<=tol)
            for (int f = e-window+1; f <=e ; ++f) out[f] = 0;
        }
        
      }            
      else if (nci==1) {
        
        int zeros=0;
        int tolcount=0;
        int flag=0;
        for (int b = 0; b < n; ++b) {
          if (zeros==0 && counts[b]!=0) continue;
          if (counts[b]==0) {
            zeros +=1;
            tolcount = 0;
          }
          else if (counts[b]>0 && counts[b]<=tol_upper) {
            zeros += 1;
            tolcount += 1;
          }
          else if (counts[b]>tol_upper) {
            zeros += 1;
            tolcount += 1;
            flag = 1;
          }
          if (tolcount>tol || flag==1 || b==n-1) {
            if (zeros-tolcount>=window)
              for (int c = b-zeros+1; c < b-tolcount+1; ++c) out[c] = 0;
            zeros = 0;
            tolcount = 0;
            flag = 0;
          }
        }
        
      }
    }
  }
  else if (days_distinct==1) {
    
    if (tol==0) {
      
      int zeros = 0;
      for (int b = 0; b < n; ++b) {
        if (counts[b]==0) zeros +=1;
        else {
          if (zeros>=window)
            for (int c = b-zeros; c < b; ++c) out[c] = 0;
          zeros = 0;
        }
        if ((b==n-1 || (b+1)%1440==0) && zeros>=window)
          for (int d = b-zeros+1; d < b+1; ++d) out[d] = 0;
        if ((b+1)%1440==0) zeros = 0;
      }
      
    }                
    else if (tol>0) {
      
      if (nci==0) {
        
        IntegerVector status(n);
        for (int b = 0; b < n; ++b) {
          if (counts[b]==0) status[b] = 0;
          else if (counts[b]<=tol_upper) status[b] = 1;
          else if (counts[b]>tol_upper) status[b] = tol+1;
        }
        int sum = 0;
        for (int c = 0; c < window; ++c)
          sum += status[c];
        if (sum<=tol)
          for (int d = 0; d < window; ++d) out[d] = 0;
        for (int e = window; e < n; ++e) {
          sum = sum-status[e-window]+status[e];
          if (sum<=tol && e%1440>window-2)
            for (int f = e-window+1; f <=e; ++f) out[f] = 0;
        }
        
      }                
      else if (nci==1) {
        
        int zeros=0;
        int tolcount=0;
        int flag=0;
        for (int b = 0; b < n; ++b) {
          if (zeros==0 && counts[b]!=0) continue;
          if (counts[b]==0) {
            zeros +=1;
            tolcount = 0;
          }
          else if (counts[b]>0 && counts[b]<=tol_upper) {
            zeros += 1;
            tolcount += 1;
          }
          else if (counts[b]>tol_upper) {
            zeros += 1;
            tolcount += 1;
            flag = 1;
          }
          if (tolcount>tol || flag==1 || b==n-1 || (b+1)%1440==0) {
            if (zeros-tolcount>=window)
              for (int c = b-zeros+1; c < b-tolcount+1; ++c) out[c] = 0;
            zeros = 0;
            tolcount = 0;
            flag = 0;
          }
        }
        
      }
    }
  }
  return(out);
}
