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
# Load in sample data frame
data(unidata)

# Get data from ID number 21005, Saturday only
counts.saturday <- unidata[unidata[,"seqn"]==21005 & unidata[,"paxday"]==7,"paxinten"]

# Calculate and plot hourly count averages
hourly.averages <- blockaves(x = counts.saturday, window = 60)
plot(hourly.averages)
}
\keyword{ accelerometry }
\keyword{ block average }