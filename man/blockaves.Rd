\name{blockaves}
\alias{blockaves}
\title{
Block average calculator
}
\description{
This function returns averages for non-overlapping segments of data.
}
\usage{
blockaves(x, window, skipchecks = FALSE)
}
\arguments{
  \item{x}{
Input vector.
}
  \item{window}{
Window length.
}
  \item{skipchecks}{
If TRUE, function skips error checking code and runs slightly faster.
}
}
\details{
NA
}
\value{
Vector of length floor(length(x)/window)
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
If length(x) is not an exact multiple of window, the average for the last partial segment of data is dropped.

Some additional information on the package accelerometry and its functions can be found on the author's website, \url{https://sites.google.com/site/danevandomelen/}
}
\seealso{
\code{\link{movingaves}}
}
\examples{
data(testdata)
counts.part1.saturday = testdata[testdata[,"seqn"]==21005 & testdata[,"paxday"]==7,"paxinten"]
hourly.averages = blockaves(x=counts.part1.saturday, window=60)
plot(hourly.averages)
}
\keyword{ accelerometry }
\keyword{ block average }