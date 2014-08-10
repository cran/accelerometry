\name{accel.intensities}
\alias{accel.intensities}
\title{
Classification of physical activity intensities
}
\description{
This function computes the number of minutes with counts in user-defined intensity levels, and the number of counts accumulated during time spent in each intensity level. It is intended for use with minute-to-minute accelerometer data.
}
\usage{
accel.intensities(counts, thresh = c(100, 760, 2020, 5999), skipchecks = FALSE)
}
\arguments{
  \item{counts}{
Time series accelerometer counts vector.
}
  \item{thresh}{
Vector of four cut-points from which five intensity ranges are derived. For example, if thresh = c(100, 760, 2020, 5999), minutes with 0-99 counts are classified as intensity level 1, minutes with 100-759 counts are classified as intensity level 2, ... , and minuts with 5999 or greater counts are classified as intensity level 5. By default the function also returns time in intensities 2-3, 4-5, and 2-5.
}
  \item{skipchecks}{
If TRUE, function skips error checking code and runs slightly faster.
}
}
\value{
An integer vector in which the first eight values are minutes in intensity 1, 2, 3, 4, 5, 2-3, 4-5, and 2-5, respectively, and the next eight values are counts accumulated during time spent in intensity 1, 2, 3, 4, 5, 2-3, 4-5, and 2-5. Intensities 1-5 typically correspond to sedentary, light, lifestyle, moderate, and vigorous.
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
Users should generally input a vector of counts that occur during wear time only. Otherwise the estimate for intensity range 1 (sedentary) will include non-wear time and be vastly overestimated.

Some additional information on the package accelerometry and its functions can be found on the author's website, \url{https://sites.google.com/site/danevandomelen/}
}
\seealso{
\code{\link{accel.weartime}}, \code{\link{accel.process.uni}}, \code{\link{accel.process.tri}}
}
\examples{
# Load in sample data frame
data(unidata)

# Get data from ID number 21005
counts.part1 <- unidata[unidata[, "seqn"] == 21005, "paxinten"]

# Create vector of counts during valid wear time only
counts.part1.weartime <- counts.part1[accel.weartime(counts = counts.part1) == 1]

# Calculate physical activity intensity variables
intensity.variables <- accel.intensities(counts = counts.part1.weartime)
}
\keyword{ accelerometry }
\keyword{ intensity }