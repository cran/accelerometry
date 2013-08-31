\name{rle2}
\alias{rle2}
\title{
Run length encoding (alternate implementation)
}
\description{
Summarize vector of numeric or character values containing "runs" of repeated values. This function is very similar to the base function rle, but sometimes much faster, and different in that it returns the start/end indices for each run in addition to the run length.
}
\usage{
rle2(x)
}
\arguments{
  \item{x}{
Input vector containing either numeric or character data.
}
}
\details{
NA
}
\value{
The function returns a 4-column object, where each row contains information on a particular "run" of repeated values. The columns are value, start index, end index, run length.

A matrix is returned if the input vector contains numeric data, and a data frame if it contains character data.
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
For numeric data, rle2 runs 2-10 times faster than rle. In general, the longer the input vector and the longer the runs, the greater the speed advantage of rle2 over rle. 

For character data, rle2 is often slower than rle, sometimes by an order of magnitude or more. However, for very long vectors (e.g. length > 10,000) with long runs (e.g. average run length > 100), rle2 can be up to 5 times faster than rle.

Some additional information on the package accelerometry and its functions can be found on the author's website, \url{https://sites.google.com/site/danevandomelen/}
}
\seealso{
\code{\link{rle}}
}
\examples{
data(testdata)
id.numbers = testdata[,1]
id.summarized = rle2(x=id.numbers)
}
\keyword{ accelerometry }
\keyword{ rle }
\keyword{ consecutive }