\name{accel.bouts}
\alias{accel.bouts}
\title{
Activity bout detection
}
\description{
This function identifies bouts of physical activity in uniaxial minute-to-minute accelerometer data.
}
\usage{
accel.bouts(counts, weartime=NULL, bout.length=10, thresh.lower=0, thresh.upper=Inf, 
tol=0, tol.lower=0, tol.upper=Inf, nci=FALSE, days.distinct=FALSE, skipchecks=FALSE)
}
\arguments{
  \item{counts}{
Time series accelerometer counts vector.
}
  \item{weartime}{
Accelerometer wear time vector; must be same length as counts and consist of 1's and 0's (if specified).
}
  \item{bout.length}{
Minimum length of an activity bout.
}
  \item{thresh.lower}{
Lower cut-off for count values in intensity range.
}
  \item{thresh.upper}{
Upper cut-off for count values in intensity range.
}
  \item{tol}{
Number of minutes with count values outside of intensity range allowed during an activity bout.
}
  \item{tol.lower}{
Lower cut-off for count values outside of intensity range during an activity bout.
}
  \item{tol.upper}{
Upper cut-off for count values outside of intensity range during an activity bout.
}
  \item{nci}{
If TRUE, use activity bouts algorithm from the NCI SAS programs (see reference); if FALSE, use regular algorithm.
}
  \item{days.distinct}{
If TRUE, treat each day of data as distinct, i.e. identify activity bouts in day 1, then day 2, etc.; if FALSE, apply algorithm on continuous basis for full monitoring period. Function assumes that first 1440 data points are day 1, next 1440 are day 2, and so on.
}
  \item{skipchecks}{
If TRUE, function skips error checking code and runs slightly faster.
}
}
\details{
If nci is set to FALSE, the algorithm uses a moving window to go through every possible interval of length bout.length in input vector counts. Any interval in which all counts are greater than or equal to tol.lower and less than or equal to tol.upper, and no more than tol counts are less than thresh.lower or greater than thresh.upper, is classified as an activity bout. 

If nci is set to TRUE, activity bouts are classified according to the algorithm used in the NCI SAS programs (see reference). Briefly, this algorithm defines an activity bout as an interval of length bout.length that starts with a count value between thresh.lower and thresh.upper and has no more than tol counts outside of that range. If these criteria are met, the bout continues until there are (tol+1) consecutive minutes outside of the range defined by thresh.lower and thresh.upper. The parameters tol.lower and tol.upper are not used.
}
\value{
Integer vector of same length as counts and weartime, with 1's indicating minutes that are part of an activity bout, and 0's indicating minutes that are not part of an activity bout.
}
\references{
National Cancer Institute. Risk factor monitoring and methods: SAS programs for analyzing NHANES 2003-2004 accelerometer data. Available at: \url{http://riskfactor.cancer.gov/tools/nhanes_pam}. Accessed August 12, 2013.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
Specifying thresh.lower but leaving the default (Inf) for thresh.upper is generally recommended. Specifying both of these parameters can be overly restrictive in that the algorithm may miss bouts of activity in which counts are consistently high, but not exclusively in one intensity range. 

If the user allows for a tolerance (e.g. tol=2) and does not use the NCI algorithm (nci=FALSE), specifying a non-zero value for tol.lower is highly recommended. Otherwise the algorithm will tend to classify minutes immediately before and after an activity bout as being part of the bout.

Leaving days.distinct set to FALSE is strongly encouraged. If set to TRUE, activity bouts that span the end of one day and the beginning of the next might be missed. This option is included because it is necessary to reproduce the algorithm used in the NCI SAS programs (see reference).

Some additional information on the package accelerometry and its functions can be found on the author's website, \url{https://sites.google.com/site/danevandomelen/}
}
\seealso{
\code{\link{accel.weartime}}, \code{\link{accel.process}}
}
\examples{
data(testdata)
counts.part1 = testdata[testdata[,"seqn"]==21005,"paxinten"]
weartime.flag = accel.weartime(counts=counts.part1)
mvpa.bouts.flag = accel.bouts(counts=counts.part1,weartime=weartime.flag,thresh.lower=2020)
}
\keyword{ accelerometry }
\keyword{ bouts }