\name{rle2.char}
\alias{rle2.char}
\title{
C++ Function for rle2 (Character)
}
\description{
This function is called by rle2 to identify runs of repeated numeric values. Not intended for direct use.
}
\usage{
rle2.char(x, n, nmax, indices)
}
\arguments{
  \item{x}{
Input vector containing character data.
}
  \item{n}{
Length of x.
}
  \item{nmax}{
Maximum number of distinct runs.
}
  \item{indices}{
If set to 0, only run values and lengths are recorded; if set to 1, start/stop positions of runs are also recorded. 
}
}
\value{
The function returns a 2- or 4-column matrix, where each row contains information on a particular run of repeated values.
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\keyword{ accelerometry }