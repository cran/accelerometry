\name{accel.process.tri}
\alias{accel.process.tri}
\title{
Process triaxial minute-to-minute accelerometer data
}
\description{
This function calculates a variety of physical activity variables based on triaxial minute-to-minute accelerometer data for individual participants. A data dictionary for the variables returned is available on the author's website, \url{https://sites.google.com/site/danevandomelen/}.
}
\usage{
accel.process.tri(counts.tri, steps = NULL, start.date = as.Date("2014/1/5"), 
                  start.time = "00:00:00", id = NULL, brevity = 1, valid.days = 1, 
                  valid.week.days = 0, valid.weekend.days = 0, int.axis = "vert", 
                  int.cuts = c(100, 760, 2020, 5999), cpm.nci = FALSE, 
                  hourly.axis = "vert", days.distinct = FALSE, nonwear.axis = "vert", 
                  nonwear.window = 60, nonwear.tol = 0, nonwear.tol.upper = 99, 
                  nonwear.nci = FALSE, weartime.minimum = 600, weartime.maximum = 1440, 
                  use.partialdays = FALSE, active.bout.length = 10, active.bout.tol = 0, 
                  mvpa.bout.tol.lower = 0, vig.bout.tol.lower = 0, 
                  active.bout.nci = FALSE, sed.bout.tol = 0, 
                  sed.bout.tol.maximum = int.cuts[2] - 1, artifact.axis = "vert", 
                  artifact.thresh = 25000, artifact.action = 1, weekday.weekend = FALSE, 
                  return.form = 2)
}
\arguments{
  \item{counts.tri}{
Three-column accelerometer counts matrix or data frame, where columns 1-3 represent vertical, anteroposterior (AP), and mediolateral (ML) counts, respectively.
}
  \item{steps}{
Steps vector; must be same length as counts.tri (if specified).
}
  \item{start.date}{
Date of first day of monitoring (must be of class 'date'). Only used to extract day of week, so if day of week is known but date is not, user can enter any date that corresponds to that day of the week. The default date corresonds to the first Sunday in January 2014. 
}
  \item{start.time}{
Optional character vector indicating the start time for monitoring. If not specified it is assumed to be 00:00:00, i.e. the very beginning of the first day.
}
  \item{id}{
Either a single value or a vector indicating the ID number for the participant whose accelerometer data was entered.
}
  \item{brevity}{
Controls the number of physical activity variables returned. If 1, returns basic indicators of physical activity volume; if 2, also returns indicators of activity intensities, activity bouts, sedentary behavior, and peak activity; if 3, also returns hourly count averages.
}
  \item{valid.days}{
Minimum number of valid days to be considered valid for analysis.
}
  \item{valid.week.days}{
Minimum number of valid weekdays to be considered valid for analysis.
}
  \item{valid.weekend.days}{
Minimum number of valid weekend days to be considered valid for analysis.
}
  \item{int.axis}{
Axis that should be used to classify intensities. Should be one of "vert", "ap", "ml", "sum" (for triaxial sum), or "mag" (for triaxial vector magnitude).
}
  \item{int.cuts}{
Vector of four cut-points from which five intensity ranges are derived using the accelerometer axis specified by int.axis. For example, if int.axis="vert" and thresh=c(100,760,2020,5999), minutes with 0-99 vertical axis counts are classified as intensity level 1, minutes with 100-759 counts are classified as intensity level 2, ... , and minutes with 5999 or greater counts are classified as intensity level 5. Intensities 1-5 typically correspond to sedentary, light, lifestyle, moderate, and vigorous.
}
  \item{cpm.nci}{
If TRUE, average counts per minute is calculated by dividing average daily counts by average daily weartime, as opposed to averaging each day's counts per minute value. In general, leave as FALSE unless you want to replicate the NCI's SAS programs [1].
}
  \item{hourly.axis}{
Axis that should be used for hourly counts per minute variables. Should be one of "vert", "ap", "ml", "sum" (for triaxial sum), or "mag" (for triaxial vector magnitude).
}
  \item{days.distinct}{
If TRUE, treat each day of data as distinct, i.e. identify non-wear time and activity bouts in day 1, then day 2, etc.; If FALSE, apply algorithms on continuous basis for full monitoring period.
}
  \item{nonwear.axis}{
Axis that should be used for non-wear algorithm. Should be one of "vert", "ap", "ml", "sum" (for triaxial sum), or "mag" (for triaxial vector magnitude).
}
  \item{nonwear.window}{
Minimum length of a non-wear interval.
}
  \item{nonwear.tol}{
Number of minutes with non-zero counts allowed during a non-wear interval.
}
  \item{nonwear.tol.upper}{
Maximum count value for a minute with non-zero counts during a non-wear interval.
}
  \item{nonwear.nci}{
If TRUE, use non-wear algorithm from the NCI's SAS programs [1]; if FALSE, use regular algorithm.
}
  \item{weartime.minimum}{
Minimum number of wear time minutes for a day of monitoring to be considered valid.
}
  \item{weartime.maximum}{
Maximum number of wear time minutes for a day of monitoring to be considered valid.
}
  \item{use.partialdays}{
If TRUE, function will consider a day of data for analysis even if it contains fewer than 1440 minutes; if FALSE, function will ignore days of data that contain fewer than 1440 minutes.
}
  \item{active.bout.length}{
Minimum length of moderate-to-vigorous physical activity (MVPA) and vigorous physical activity (VPA) bouts.
}
  \item{active.bout.tol}{
Number of minutes with counts below the required intensity level allowed during MVPA and VPA bouts.
}
  \item{mvpa.bout.tol.lower}{
Lower cut-off for count values outside of MVPA intensity range during an MVPA bout.
}
  \item{vig.bout.tol.lower}{
Lower cut-off for count values outside of VPA intensity range during a VPA bout.
}
  \item{active.bout.nci}{
If TRUE, use activity bouts algorithm from the NCI's SAS programs [1]; if FALSE, use regular algorithm.
}
  \item{sed.bout.tol}{
Number of minutes with counts outside sedentary range allowed during sedentary bouts.
}
  \item{sed.bout.tol.maximum}{
Upper cut-off for count values outside sedentary range during a sedentary bout.
}
  \item{artifact.axis}{
Axis that should be used to detect artifacts. Should be one of "vert", "ap", "ml", "sum" (for triaxial sum), or "mag" (for triaxial vector magnitude).
}
  \item{artifact.thresh}{
Lower cut-off for counts that are abnormally high and should be considered artifacts.
}
  \item{artifact.action}{
If 1, exclude days that have one or more artifacts; if 2, consider artifacts as non-wear time; if 3, replace artifacts with average of neighboring count values (for all axes of minutes identified as artifacts); if 4, take no action.
}
  \item{weekday.weekend}{
If TRUE, function computes physical activity averages for weekdays and weekend days separately (in addition to daily averages for all valid days, which are computed regardless). If FALSE, function only computes averages for all valid days.
}
  \item{return.form}{
If 1, function returns physical activity variables on per-person basis, i.e. daily averages for each participant; if 2, function returns variables on per-day basis; if 3, function returns both via a list.
}
}
\details{
NA
}
\value{
A single matrix or a list of two matrices, depending on return.form.
}
\references{
1. National Cancer Institute. Risk factor monitoring and methods: SAS programs for analyzing NHANES 2003-2004 accelerometer data. Available at: \url{http://riskfactor.cancer.gov/tools/nhanes_pam}. Accessed February 1, 2014.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
The first 1440 minutes of data must represent a full day of monitoring, i.e. midnight to midnight. If a partial day of data is present, it must be removed before calling accel.process.tri.

This function is similar to \code{\link{accel.process.uni}}, but works with triaxial accelerometer data. For example, rather than just calculating a single counts variable for each day of monitoring, accel.process.tri calculates a daily counts variable for the vertical axis, anteroposterior (AP) axis, and mediolateral (ML) axis, and also for the triaxial sum and triaxial vector magnitude (defined as the square root of the sum of the squares of the three axes). Also, the user can choose which of the five signals to use for artifact correction, non-wear detection, activity intensity classification, and hourly counts per minute averaging. 

In the future, a sample triaxial dataset will be included in the package and some Examples will be provided below. For now, researchers should see the Examples for \code{\link{accel.process.uni}}. This function has very similar inputs. 

Some additional information on the package accelerometry and its functions can be found on the author's website, \url{https://sites.google.com/site/danevandomelen/}.
}
\seealso{
\code{\link{accel.artifacts}}, \code{\link{accel.bouts}}, \code{\link{accel.process.uni}}, \code{\link{accel.intensities}}, \code{\link{accel.sedbreaks}}, \code{\link{accel.weartime}}, \code{\link{blockaves}}, \code{\link{movingaves}}, \code{\link{rle2}}
}
\examples{
# Load in sample data frame
data(tridata)

# Process data and request per-day variables
accel.days1 <- accel.process.tri(counts = tridata[,1:3], steps = tridata[,4])

# Process data, but for non-wear detection use triaxial vector magnitude with 90-
# minute window and two-minute tolerance for nonzero counts up to 200
accel.days2 <- accel.process.tri(counts = tridata[,1:3], steps = tridata[,4],
                                 nonwear.axis = "mag", nonwear.window = 90,
                                 nonwear.tol = 2, nonwear.tol.upper = 200)
}
\keyword{ accelerometry }
\keyword{ physical activity }
\keyword{ triaxial }