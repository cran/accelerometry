#' Functions for Processing Accelerometer Data
#'
#' A collection of functions that perform operations on time-series 
#' accelerometer data, such as identify non-wear time, flag minutes that are 
#' part of an activity bout, and find the maximum 10-minute average count value. 
#' The functions are generally very flexible, allowing for a variety of 
#' algorithms to be implemented. Most of the functions are written in C++ for 
#' efficiency.
#'
#' \tabular{ll}{
#' Package: \tab accelerometry \cr
#' Type: \tab Package \cr
#' Version: \tab 3.1.2 \cr
#' Date: \tab 2018-08-23 \cr
#' License: \tab GPL-3 \cr
#' }
#'
#' See 
#' \href{https://cran.r-project.org/package=accelerometry}{CRAN documentation} 
#' for full list of functions.
#'
#' @author Dane R. Van Domelen \cr \email{vandomed@@gmail.com}
#'
#' @references
#' Centers for Disease Control and Prevention (CDC). National Center for Health 
#' Statistics (NCHS). National Health and Nutrition Examination Survey Data. 
#' Hyattsville, MD: US Department of Health and Human Services, Centers for 
#' Disease Control and Prevention, 2003-6. Available at: 
#' \url{https://wwwn.cdc.gov/nchs/nhanes/Default.aspx}. Accessed Aug. 19, 2018.
#' 
#' Eddelbuettel, D. and Francois, R. (2011) Rcpp: Seamless R and C++ 
#' Integration. Journal of Statistical Software, 40(8), 1-18. 
#' \url{http://www.jstatsoft.org/v40/i08/}.
#' 
#' Eddelbuettel, D. (2013) Seamless R and C++ Integration with Rcpp. Springer, 
#' New York. ISBN 978-1-4614-6867-7.
#' 
#' Eddelbuettel, D. and Balamuta, J.J. (2017). Extending R with C++: A Brief 
#' Introduction to Rcpp. PeerJ Preprints 5:e3188v1. 
#' \url{https://doi.org/10.7287/peerj.preprints.3188v1}.
#' 
#' National Cancer Institute. Risk factor monitoring and methods: SAS programs 
#' for analyzing NHANES 2003-2004 accelerometer data. Available at: 
#' \url{http://riskfactor.cancer.gov/tools/nhanes_pam}. Accessed Aug. 19, 2018.
#' 
#' Van Domelen, D.R., Pittard, W.S. and Harris, T.B. (2018) 
#' nhanesaccel: Process accelerometer data from NHANES 2003-2006. R package 
#' version 3.1.1. \url{https://github.com/vandomed/accelerometry}. 
#' 
#' Acknowledgment: This material is based upon work supported by the National 
#' Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
#'
#'
#' @docType package
#' @importFrom Rcpp evalCpp
#' @importFrom dvmisc inside
#' @useDynLib accelerometry, .registration=TRUE
#' @name accelerometry
NULL