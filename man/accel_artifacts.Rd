\name{accel.artifacts}
\alias{accel.artifacts}
\title{
Accelerometer Artifact Correction
}
\description{
This function corrects abnormally high count values in minute-to-minute accelerometer data by replacing such values with the average of the neighboring count values.
}
\usage{
accel.artifacts(counts, thresh = 32767, skipchecks = FALSE)
}
\arguments{
  \item{counts}{
Time series accelerometer counts vector.
}
  \item{thresh}{
Minimum count value that is considered an artifact.
}
  \item{skipchecks}{
If TRUE, function skips error checking code and runs slightly faster.
}
}
\value{
An integer vector identical to the input vector counts but with artifacts corrected.
}
\references{
1. National Cancer Institute. Risk factor monitoring and methods: SAS programs for analyzing NHANES 2003-2004 accelerometer data. Available at: \url{http://riskfactor.cancer.gov/tools/nhanes_pam}. Accessed July 31, 2014.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
An integer vector is returned despite the average calculation often producing a decimal. This follows the convention used by the NCI's SAS programs [1].

Some additional information on the package accelerometry and its functions can be found on the author's website, \url{https://sites.google.com/site/danevandomelen/}
}
\seealso{
\code{\link{accel.process.uni}}, \code{\link{accel.process.tri}}
}
\examples{
# Load in sample data frame
data(unidata)

# Get data from ID number 21007
counts.part3 <- unidata[unidata[, "seqn"] == 21007, "paxinten"]

# Replace artifacts (defined as 10000+ counts) with average of neighboring values
counts.part3.corrected <- accel.artifacts(counts = counts.part3, thresh = 10000)
}
\keyword{ accelerometry }
\keyword{ artifacts }