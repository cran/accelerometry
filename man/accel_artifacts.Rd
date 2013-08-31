\name{accel.artifacts}
\alias{accel.artifacts}
\title{
Accelerometer artifact correction
}
\description{
This function corrects abnormally high count values in uniaxial minute-to-minute accelerometer data by replacing such values with the average of the neighboring count values.
}
\usage{
accel.artifacts(counts, thresh=32767, skipchecks=FALSE)
}
\arguments{
  \item{counts}{
Time series accelerometer counts vector.
}
  \item{thresh}{
Minimum count value that is considered an artifact.
}
  \item{skipchecks}{If TRUE, function skips error checking code and runs slightly faster.
}
}
\details{
NA
}
\value{
An integer vector identical to the input vector counts but with artifacts corrected.
}
\references{
National Cancer Institute. Risk factor monitoring and methods: SAS programs for analyzing NHANES 2003-2004 accelerometer data. Available at: \url{http://riskfactor.cancer.gov/tools/nhanes_pam}. Accessed August 12, 2013.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
An integer vector is returned despite the average calculation often producing a decimal. This follows the convention used by the NCI SAS programs (see reference).

Some additional information on the package accelerometry and its functions can be found on the author's website, \url{https://sites.google.com/site/danevandomelen/}
}
\seealso{
\code{\link{accel.process}}
}
\examples{
data(testdata)
counts.part3 = testdata[testdata[,"seqn"]==21007,"paxinten"]
counts.part3.corrected = accel.artifacts(counts=counts.part3, thresh=10000)
}
\keyword{ accelerometry }
\keyword{ artifacts }