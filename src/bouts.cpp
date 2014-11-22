#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector bouts(NumericVector counts, IntegerVector weartime, int bout_length, int thresh_lower, int thresh_upper, int tol, int tol_lower, int tol_upper, int nci, int days_distinct) {
  int n = counts.size();
  IntegerVector out(n);
  
  if (days_distinct==0) {
    
    if (tol==0) {
      
      int counter = 0;
      for (int a = 0; a < n; ++a) {
        if (weartime[a]==1 && counts[a]>=thresh_lower && counts[a]<=thresh_upper) counter +=1;
        else {
          if (counter>=bout_length)
            for (int b = a-counter; b < a; ++b) out[b] = 1;
          counter = 0;
        }
        if (a==n-1 && counter>=bout_length)
          for (int c = a-counter+1; c < a+1; ++c) out[c] = 1;
      }
      
    }            
    else if (tol>0) {
      
      if (nci==0) {
        
        IntegerVector status(n);
        for (int b = 0; b < n; ++b) {
          if (weartime[b]==1 && counts[b]>=thresh_lower && counts[b]<=thresh_upper) status[b] = 0;
          else if (weartime[b]==1 && counts[b]>=tol_lower && counts[b]<=tol_upper) status[b] = 1;
          else status[b] = tol+1;
        }
        int sum = 0;
        for (int c = 0; c < bout_length; ++c)
          sum += status[c];
        if (sum<=tol)
          for (int d = 0; d < bout_length; ++d) out[d] = 1;
        for (int e = bout_length; e < n; ++e) {
          sum = sum-status[e-bout_length]+status[e];
          if (sum<=tol)
            for (int f = e-bout_length+1; f <=e ; ++f) out[f] = 1;
        }
        
      }            
      else if (nci==1) {
        
        for (int a = 0; a < n-bout_length+1; ++a) {
          if (counts[a]<thresh_lower || counts[a]>thresh_upper) continue;
          int tolcount = 0;
          int tolcount2 = 0;
          int last = 0;
          for (int b = a; b < a+bout_length; ++b) {
            if (counts[b]>=thresh_lower && counts[b]<=thresh_upper) {
              tolcount2 = 0;
              last = b;
            }
            else {
              tolcount += 1;
              tolcount2 += 1;
            }
            if (tolcount==tol+1) break;
          }
          if (tolcount<tol+1) {
            if (a+bout_length==n) {
              for (int c = a; c <= last; ++c) out[c] = 1;
              break;
            }
            for (int c = a+bout_length; c < n; ++c) {
              if (counts[c]>=thresh_lower && counts[c]<=thresh_upper) {
                tolcount2 = 0;
                last = c;
              }
              else tolcount2 += 1;
              if (tolcount2==tol+1 || c==n-1) {
                for (int d = a; d <= last; ++d) out[d] = 1;
                break;
              }
            }
          }
        }
        
      }
    }
  }
  else if (days_distinct==1) {
    
    if (tol==0) {
      
      int counter = 0;
      for (int a = 0; a < n; ++a) {
        if (weartime[a]==1 && counts[a]>=thresh_lower && counts[a]<=thresh_upper) counter +=1;
        else {
          if (counter>=bout_length)
            for (int b = a-counter; b < a; ++b) out[b] = 1;
          counter = 0;
        }
        if ((a==n-1 || (a+1)%1440==0) && counter>=bout_length)
          for (int c = a-counter+1; c < a+1; ++c) out[c] = 1;
        if ((a+1)%1440==0) counter = 0;
      }
      
    }              
    else if (tol>0) {
      
      if (nci==0) {
        
        IntegerVector status(n);
        for (int b = 0; b < n; ++b) {
          if (weartime[b]==1 && counts[b]>=thresh_lower && counts[b]<=thresh_upper) status[b] = 0;
          else if (weartime[b]==1 && counts[b]>=tol_lower && counts[b]<=tol_upper) status[b] = 1;
          else status[b] = tol+1;
        }
        int sum = 0;
        for (int c = 0; c < bout_length; ++c)
          sum += status[c];
        if (sum<=tol)
          for (int d = 0; d < bout_length; ++d) out[d] = 1;
        for (int e = bout_length; e < n; ++e) {
          sum = sum-status[e-bout_length]+status[e];
          if (sum<=tol && e%1440>bout_length-2)
            for (int f = e-bout_length+1; f <=e ; ++f) out[f] = 1;
        }
        
      }
      else if (nci==1) {
        
        for (int a = 0; a < n-bout_length+1; ++a) {
          if (counts[a]<thresh_lower || counts[a]>thresh_upper) continue;
          int counter = 0;
          int tolcount = 0;
          int tolcount2 = 0;
          int last = 0;
          for (int b = a; b < a+bout_length; ++b) {
            if (counts[b]>=thresh_lower && counts[b]<=thresh_upper) {
              counter += 1;
              tolcount2 = 0;
              last = b;
            }
            else {
              counter += 1;
              tolcount += 1;
              tolcount2 += 1;
            }
            if (tolcount==tol+1 || (b+1)%1440==0) break;
          }
          if (counter<bout_length || tolcount==tol+1) continue;
          else {
            if ((a+bout_length)%1440==0)
              for (int c = a; c <= last; ++c) out[c] = 1;
            else {
              for (int d = a+bout_length; d < n; ++d) {
                if (counts[d]>=thresh_lower && counts[d]<=thresh_upper) {
                  tolcount2 = 0;
                  last = d;
                }
                else tolcount2 += 1;
                if (tolcount2==tol+1 || d==n-1 || (d+1)%1440==0) {
                  for (int e = a; e <= last; ++e) out[e] = 1;
                  break;
                }
              }
            }
          }
        }
        
      }
    }
  }
  return(out);
}
