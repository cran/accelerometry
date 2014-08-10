\name{personvars}
\alias{personvars}
\title{
Function for calculating daily averages for physical activity variables
}
\description{
Not intended for direct use.
}
\usage{
personvars(dayvars, rows, days, wk, we)
}
\arguments{
  \item{dayvars}{
Matrix that contains daily physical activity variables.
}
  \item{rows}{
Number of rows in the matrix to be returned.
}
  \item{days}{
Minimum number of valid days required to be considered valid data for analysis.
}
  \item{wk}{
Minimum number of valid weekdays required to be considered valid data for analysis.
}
  \item{we}{
Minimum number of valid weekend days required to be considered valid data for analysis.
}
}
\value{
Matrix with averages for physical activity variables.
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\keyword{ accelerometry }