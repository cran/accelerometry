\name{accel.sedbreaks}
\alias{accel.sedbreaks}
\title{
Sedentary Breaks Detection
}
\description{
This function identifies sedentary breaks in minute-to-minute accelerometer data.
}
\usage{
accel.sedbreaks(counts, weartime = NULL, thresh = 100, return.flags = FALSE, 
                skipchecks = FALSE)
}
\arguments{
  \item{counts}{
Time series accelerometer counts vector.
}
  \item{weartime}{
Accelerometer wear time vector; must be same length as counts and consist of 1's and 0's (if specified).
}
  \item{thresh}{
Minimum count value that must be achieved to record a sedentary break.
}
  \item{return.flags}{
If TRUE, function returns vector of 0's and 1's in which 1's indicate minutes in which a sedentary break has occurred; if FALSE, function returns total number of sedentary breaks during monitoring period.
}
  \item{skipchecks}{
If TRUE, function skips error checking code and runs slightly faster.
}
}
\value{
Either a single numeric value indicating the total number of sedentary breaks during the monitoring period, or a vector of same length as counts where 1's indicate minutes in which a sedentary break has occurred.
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
If a wear time vector is not provided, algorithm may consider the first minute after a non-wear period as a sedentary break, which is not correct because the preceeding minute was non-wear time and may or may not have been sedentary time. This could result in a slight over-estimate of sedentary breaks.

Some additional information on the package accelerometry and its functions can be found on the author's website, \url{https://sites.google.com/site/danevandomelen/}
}
\seealso{
\code{\link{accel.weartime}}, \code{\link{accel.process.uni}}, \code{\link{accel.process.tri}}
}
\examples{
# Load in sample data frame
data(unidata)

# Get data from ID number 21005
id.part1 <- unidata[unidata[, "seqn"] == 21005, "seqn"]
counts.part1 <- unidata[unidata[, "seqn"] == 21005, "paxinten"]

# Identify periods of valid wear time
weartime.flag <- accel.weartime(counts = counts.part1)

# Count number of sedentary breaks (over full week)
num.sedbreaks <- accel.sedbreaks(counts = counts.part1, weartime = weartime.flag)

# Flag minutes that represent sedentary breaks
flag.sedbreaks <- accel.sedbreaks(counts = counts.part1, weartime = weartime.flag, 
                                  return.flags = TRUE)
}
\keyword{ accelerometry }
\keyword{ sedentary breaks}