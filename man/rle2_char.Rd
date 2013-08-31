\name{rle2.char}
\alias{rle2.char}
\title{
C++ function for rle2 (character)
}
\description{
This function is called by rle2 to identify runs of repeated numeric values. Not intended for direct use.
}
\usage{
rle2.char(x, nmax)
}
\arguments{
  \item{x}{
Input vector containing character data.
}
  \item{nmax}{
Maximum number of distinct runs.
}
}
\details{
NA
}
\value{
The function returns a 4-column data frame, where each row contains information on a particular "run" of repeated values. The columns are value, start index, end index, run length.
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