\name{accelerometry-package}
\alias{accelerometry-package}
\alias{accelerometry}
\docType{package}
\title{
Functions for processing minute-to-minute accelerometer data
}
\description{
This package contains a collection of functions that perform operations on time-series accelerometer data, such as identify non-wear time, flag minutes that are part of an activity bout, and find the maximum 10-minute average count value. The functions are generally very flexible, allowing for a variety of algorithms to be implemented. Most of the functions are written in C++ and therefore execute very quickly.
}
\details{
\tabular{ll}{
Package: \tab accelerometry\cr
Type: \tab Package\cr
Version: \tab 2.1\cr
Date: \tab 2014-02-01\cr
License: \tab GPL-2\cr
}
The following functions are contained in the package accelerometry:

\code{\link{accel.artifacts}},
\code{\link{accel.bouts}},
\code{\link{accel.process.uni}},
\code{\link{accel.process.tri}},
\code{\link{accel.intensities}},
\code{\link{accel.sedbreaks}},
\code{\link{accel.weartime}},
\code{\link{blockaves}},
\code{\link{movingaves}},
\code{\link{rle2}}

Two datasets are included for the Examples: unidata and tridata. unidata contains uniaxial data on the first five participants in NHANES 2003-2004 [2], and tridata contains seven days of triaxial data from a volunteer, provided by Ei Ei Khaing Nang from the Department of Epidemiology and Public Health, Yong Loo Lin School of Medicine,  National University of Singapore, Singapore, Republic of Singapore.

Some additional information on the package accelerometry and its functions can be found on the author's website, \url{https://sites.google.com/site/danevandomelen/}
}
\author{
Dane R. Van Domelen

Maintainer: Dane R. Van Domelen <vandomed@gmail.com>
}
\references{
1. Centers for Disease Control and Prevention (CDC). National Center for Health Statistics (NCHS). National Health and Nutrition Examination Survey Data. Hyattsville, MD: US Department of Health and Human Services, Centers for Disease Control and Prevention, 2003-6 \url{http://www.cdc.gov/nchs/nhanes/nhanes_questionnaires.htm}. Accessed February 1, 2014.

2. National Cancer Institute. Risk factor monitoring and methods: SAS programs for analyzing NHANES 2003-2004 accelerometer data. Available at: \url{http://riskfactor.cancer.gov/tools/nhanes_pam}. Accessed February 1, 2014.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\keyword{ package }
\seealso{
NA
}
\examples{
# Load in sample data from NHANES 2003-2004 [1]
data(unidata)

# Get data from ID number 21005
id.part1 <- unidata[unidata[,"seqn"] == 21005,"seqn"]
days.part1 <- unidata[unidata[,"seqn"] == 21005,"paxday"]
counts.part1 <- unidata[unidata[,"seqn"] == 21005,"paxinten"]

# Identify periods of valid wear time
weartime.flag <- accel.weartime(counts = counts.part1)

# Flag minutes that are part of a moderate-to-vigorous activity bout
mvpa.bouts.flag <- accel.bouts(counts = counts.part1, weartime = weartime.flag, 
                              thresh.lower = 2020)
                              
# Obtain maximum 10-minute count average
max.10min.movingave <- movingaves(x=counts.part1,window=10,return.max=TRUE)

# Process data from ID 21005 and request per-day variables and daily averages
accel.list <- accel.process.uni(counts = counts.part1,  id = id.part1, return.form = 3)
                  
# Process data according to methods used in NCI's SAS programs [2]
accel.nci <- accel.process.uni(counts = counts.part1, id = id.part1, brevity = 2, 
                               valid.days = 4, cpm.nci = TRUE, days.distinct = TRUE, 
                               nonwear.tol = 2, nonwear.tol.upper = 100, 
                               nonwear.nci = TRUE, weartime.maximum = 1440, 
                               active.bout.tol = 2, active.bout.nci = TRUE, 
                               artifact.thresh = 32767, artifact.action = 3, 
                               return.form = 3)
                               
# Load in triaxial sample data
data(tridata)

# Process data and request per-day variables
accel.days <- accel.process.tri(counts = tridata[,1:3], steps = tridata[,4])

# Process data, but for non-wear detection use triaxial vector magnitude with 90-
# minute window and two-minute tolerance for nonzero counts up to 200
accel.days <- accel.process.tri(counts = tridata[,1:3], steps = tridata[,4],
                                nonwear.axis = "mag", nonwear.window = 90,
                                nonwear.tol = 2, nonwear.tol.upper = 200)
}