\name{rle2.num}
\alias{rle2.num}
\title{
C++ function for rle2 (numeric)
}
\description{
This function is called by rle2 to identify runs of repeated numeric values. Not intended for direct use.
}
\usage{
rle2.num(x, nmax)
}
\arguments{
  \item{x}{
Input vector containing numeric data.
}
  \item{nmax}{
Maximum number of distinct runs.
}
}
\details{
NA
}
\value{
The function returns a 4-column matrix, where each row contains information on a particular "run" of repeated values. The columns are value, start index, end index, run length.
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
NA
}
\seealso{
NA
}
\examples{
NA
}
\keyword{ accelerometry }