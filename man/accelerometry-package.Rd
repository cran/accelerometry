\name{accelerometry-package}
\alias{accelerometry-package}
\alias{accelerometry}
\docType{package}
\title{
Functions for processing uniaxial minute-to-minute accelerometer data
}
\description{
This package contains a collection of functions that perform operations on time-series accelerometer data, such as identify non-wear time, flag minutes that are part of an activity bout, and find the maximum 10-minute average count value. The functions are generally very flexible, allowing for a variety of algorithms to be implemented. Most of the functions are written in C++ and therefore execute very quickly.
}
\details{
\tabular{ll}{
Package: \tab accelerometry\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2013-08-31\cr
License: \tab GPL-2\cr
}
The following functions are contained in the package accelerometry:

\code{\link{accel.artifacts}},
\code{\link{accel.bouts}},
\code{\link{accel.process}},
\code{\link{accel.intensities}},
\code{\link{accel.sedbreaks}},
\code{\link{accel.weartime}},
\code{\link{blockaves}},
\code{\link{movingaves}},
\code{\link{rle2}}

Some additional information on the package accelerometry and its functions can be found on the author's website, \url{https://sites.google.com/site/danevandomelen/}
}
\author{
Dane R. Van Domelen

Maintainer: Dane R. Van Domelen <vandomed@gmail.com>
}
\references{
1. Centers for Disease Control and Prevention (CDC). National Center for Health Statistics (NCHS). National Health and Nutrition Examination Survey Data. Hyattsville, MD: US Department of Health and Human Services, Centers for Disease Control and Prevention, 2003-6 \url{http://www.cdc.gov/nchs/nhanes/nhanes_questionnaires.htm}. Accessed August 12, 2013.

2. National Cancer Institute. Risk factor monitoring and methods: SAS programs for analyzing NHANES 2003-2004 accelerometer data. Available at: \url{http://riskfactor.cancer.gov/tools/nhanes_pam}. Accessed August 12, 2013.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\keyword{ package }
\seealso{
NA
}
\examples{
data(testdata)
counts.part1 = testdata[testdata[,"seqn"]==21005,"paxinten"]
days.part1 = testdata[testdata[,"seqn"]==21005,"paxday"]
id.part1 = testdata[testdata[,"seqn"]==21005,"seqn"]
weartime.flag = accel.weartime(counts=counts.part1)
mvpa.bouts.flag = accel.bouts(counts=counts.part1,weartime=weartime.flag,thresh.lower=2020)
max.10min.movingave = movingaves(x=counts.part1,window=10,return.max=TRUE)
accel.days = accel.process(counts=counts.part1,days=days.part1,id=id.part1)
accel.averages = accel.process(counts=counts.part1,days=days.part1,id=id.part1,return.averages=TRUE)
}