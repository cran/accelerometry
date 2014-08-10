\name{movingaves}
\alias{movingaves}
\title{
Moving averages
}
\description{
This function returns either a vector of moving averages or the maximum moving average for some input vector x.
}
\usage{
movingaves(x, window, return.max = FALSE, skipchecks = FALSE)
}
\arguments{
  \item{x}{
Input vector.
}
  \item{window}{
Window length for moving averages.
}
  \item{return.max}{
If TRUE, function returns the maximum moving average; if FALSE, function returns a vector of moving averages.
}
  \item{skipchecks}{
If TRUE, function skips error checking code and runs slightly faster.}
}
\value{
Either a single numeric value indicating the maximum moving average, or a vector of length (length(x) - window + 1) with moving averages.
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
Some additional information on the package accelerometry and its functions can be found on the author's website, \url{https://sites.google.com/site/danevandomelen/}
}
\seealso{
\code{\link{blockaves}}
}
\examples{
# Load in sample data frame
data(unidata)

# Get data from ID number 21005
counts.part1 <- unidata[unidata[, "seqn"] == 21005, "paxinten"]

# Create vector of all 10-minute moving averages
all.10min.averages <- movingaves(x = counts.part1, window = 10)

# Calculate maximum 10-minute moving average
max.10min.average <- movingaves(x = counts.part1, window = 10, return.max = TRUE)
}
\keyword{ accelerometry }
\keyword{ moving average }