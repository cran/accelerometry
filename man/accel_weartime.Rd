\name{accel.weartime}
\alias{accel.weartime}
\title{
Accelerometer non-wear detection
}
\description{
This function identifies periods of non-wear time in minute-to-minute accelerometer data.
}
\usage{
accel.weartime(counts, window = 60, tol = 0, tol.upper = 99, nci = FALSE, 
               days.distinct = FALSE, skipchecks = FALSE)
}
\arguments{
  \item{counts}{
Time series accelerometer counts vector.
}
  \item{window}{
Minimum length of a non-wear interval.
}
  \item{tol}{
Number of minutes with non-zero counts allowed during a non-wear interval.
}
  \item{tol.upper}{
Maximum count value for a minute with non-zero counts during a non-wear interval.
}
  \item{nci}{
If TRUE, use non-wear algorithm from the NCI's SAS programs [1]; if FALSE, use regular algorithm (see Details).
}
  \item{days.distinct}{
if TRUE, treat each day of data as distinct, i.e. identify wear time in day 1, then day 2, etc.; if FALSE, apply algorithm on continuous basis for full monitoring period. Function assumes that first 1440 data points are day 1, next 1440 are day 2, and so on. 
}
  \item{skipchecks}{
If TRUE, function skips error checking code and runs slightly faster.
}
}
\details{
If nci is set to FALSE, the algorithm uses a moving window to go through every possible interval of length window in input vector counts. Any interval in which no more than tol counts are non-zero, and those counts are less than tol.upper, is classified as non-wear time. 

If nci is set to TRUE, non-wear time is classified according to the algorithm used in the NCI's SAS programs [1]. Briefly, this algorithm defines a non-wear period as an interval of length window that starts with a count value of 0, does not contain any periods with (tol + 1) consecutive non-zero count values, and does not contain any counts greater than tol.upper. If these criteria are met, the bout continues until there are (tol + 1) consecutive non-zero count values or a single count value greater than tol.upper.
}
\value{
Integer vector of same length as counts and weartime, with 1's indicating minutes of valid wear time, and 0's indicating minutes of non-wear time.
}
\references{
1. National Cancer Institute. Risk factor monitoring and methods: SAS programs for analyzing NHANES 2003-2004 accelerometer data. Available at: \url{http://riskfactor.cancer.gov/tools/nhanes_pam}. Accessed July 31, 2014.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
Leaving days.distinct set to FALSE is strongly encouraged. If set to TRUE, non-wear periods that span the end of one day and the beginning of the next might be missed. This option is included because it is necessary to reproduce the algorithm used in the NCI's SAS programs [1].

Some additional information on the package accelerometry and its functions can be found on the author's website, \url{https://sites.google.com/site/danevandomelen/}
}
\seealso{
\code{\link{accel.process.uni}}, \code{\link{accel.process.tri}}
}
\examples{
# Load in sample data frame
data(unidata)

# Get data from ID number 21005
counts.part1 <- unidata[unidata[, "seqn"] == 21005, "paxinten"]

# Identify periods of valid wear time
weartime.flag <- accel.weartime(counts = counts.part1)
}
\keyword{ accelerometry }
\keyword{ non-wear time }
\keyword{ wear time }