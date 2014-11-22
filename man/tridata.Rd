\name{tridata}
\alias{tridata}
\docType{data}
\title{
Triaxial Sample Data
}
\description{
This dataset is used to illustrate the accel.process.tri function in the accelerometry package.
}
\usage{data(tridata)}
\format{
  The format is:
 int [1:10080, 1:4] 118 0 0 2 0 0 0 20 0 0 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:10080] "526" "527" "528" "529" ...
  ..$ : chr [1:4] "vert" "ap" "ml" "steps"
}
\details{
This dataset contains a four-column matrix. The first three columns are counts in the vertical, anteroposterior, and mediolateral axes, and the fourth column is steps. There are seven days worth of data from a volunteer.  
}
\source{
This data was graciously provided by Ei Ei Khaing Nang from the Department of Epidemiology and Public Health, Yong Loo Lin School of Medicine,  National University of Singapore, Singapore, Republic of Singapore. 
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\examples{
# Load in sample data
data(tridata)

# Plot one day of step data
plot(tridata[1:1440, 4])
}
\keyword{datasets}