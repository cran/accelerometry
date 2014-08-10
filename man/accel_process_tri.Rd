\name{accel.process.tri}
\alias{accel.process.tri}
\title{
Process triaxial minute-to-minute accelerometer data
}
\description{
This function calculates a variety of physical activity variables based on triaxial minute-to-minute accelerometer data for individual participants. A data dictionary for the variables returned is available here: \url{https://sites.google.com/site/danevandomelen/r-package-accelerometry/data-dictionary}.
}
\usage{
accel.process.tri(counts.tri, steps = NULL, nci.methods = FALSE, 
                  start.date = as.Date("2014/1/5"), start.time = "00:00:00", id = NULL, 
                  brevity = 1, valid.days = 1, valid.week.days = 0, 
                  valid.weekend.days = 0, int.axis = "vert", 
                  int.cuts = c(100, 760, 2020, 5999), cpm.nci = FALSE, 
                  hourly.axis = "vert", days.distinct = FALSE, nonwear.axis = "vert", 
                  nonwear.window = 60, nonwear.tol = 0, nonwear.tol.upper = 99, 
                  nonwear.nci = FALSE, weartime.minimum = 600, weartime.maximum = 1440, 
                  partialday.minimum = 1440, active.bout.length = 10, 
                  active.bout.tol = 0, mvpa.bout.tol.lower = 0, vig.bout.tol.lower = 0, 
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
Steps vector. If specified, must be same length as counts.tri.
}
\item{nci.methods}{
If TRUE, inputs are set to replicate the data processing methods used by the NCI's SAS programs [1]. More specifically: 

valid.days = 4; valid.week.days = 0; valid.weekend.days = 0; int.axis = "vert"; int.cuts = c(100, 760, 2020, 5999); cpm.nci = TRUE; hourly.axis = "vert"; days.distinct = TRUE; nonwear.axis = "vert"; nonwear.window = 60; nonwear.tol = 2; nonwear.tol.upper = 100; nonwear.nci = TRUE; weartime.minimum = 600; weartime.maximum = 1440; partialday.minimum = 1440; active.bout.length = 10; active.bout.tol = 2; mvpa.bout.tol.lower = 0; vig.bout.tol.lower = 0; active.bout.nci = TRUE; sed.bout.tol = 0; sed.bout.tol.maximum = 759; artifact.axis = "vert"; artifact.thresh = 32767; artifact.action = 3

Even if nci.methods is set to TRUE, the user can specify non-default values for brevity, weekday.weekend, and return.form.
}
  \item{start.date}{
Date of first day of monitoring (must be of class 'date'). Only used to extract day of week, so if day of week is known but date is not, user can enter any date that corresponds to that day of the week. The default date corresponds to the first Sunday in January 2014. 
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
Vector of four cut-points from which five intensity ranges are derived using the accelerometer axis specified by int.axis. For example, if int.axis = "vert" and thresh = c(100, 760, 2020, 5999), minutes with 0-99 vertical axis counts are classified as intensity level 1, minutes with 100-759 counts are classified as intensity level 2, ... , and minutes with 5999 or greater counts are classified as intensity level 5. Intensities 1-5 typically correspond to sedentary, light, lifestyle, moderate, and vigorous.
}
  \item{cpm.nci}{
If TRUE, average counts per minute is calculated by dividing average daily counts by average daily weartime, as opposed to averaging each day's counts per minute value. In general, leave as FALSE unless you want to replicate the NCI's SAS programs [1].
}
  \item{hourly.axis}{
Axis that should be used for hourly counts per minute variables. Should be one of "vert", "ap", "ml", "sum" (for triaxial sum), or "mag" (for triaxial vector magnitude).
}
  \item{days.distinct}{
If TRUE, treat each day of data as distinct, i.e. identify non-wear time and activity bouts in day 1, then day 2, etc.; If FALSE, apply algorithms on continuous basis for full monitoring period. If protocol has participants remove accelerometer for sleep, strongly recommend setting to FALSE to capture non-wear periods that start between 11 pm and midnight.
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
If TRUE, use non-wear algorithm from the NCI's SAS programs [1]; if FALSE, use regular algorithm (see Details).
}
  \item{weartime.minimum}{
Minimum number of wear time minutes for a day of monitoring to be considered valid.
}
  \item{weartime.maximum}{
Maximum number of wear time minutes for a day of monitoring to be considered valid.
}
  \item{partialday.minimum}{
Minimum number of minutes for a partial day of monitoring to be processed and potentially considered valid for analysis (generally applies only to the first and last days of monitoring, which may not cover full 24-hour periods). This input is included because some researchers may prefer to exclude a day that only has data from, say, 1 pm to midnight. Even though there may be sufficient wear time during that period to be classified as a valid day, the missing chunk of data prior to 1 pm may result in the day not being representative of the participant's usual physical activity.
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
If TRUE, use activity bouts algorithm from the NCI's SAS programs [1]; if FALSE, use regular algorithm (see Details).
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
The algorithm used to identify non-wear time is defined by function inputs nonwear.axis, nonwear.window, nonwear.tol, nonwear.tol.upper, and nonwear.nci. If nonwear.nci is set to FALSE, a `regular' non-wear algorithm is used. This algorithm classifies as non-wear time any interval of length nonwear.window in which no more than nonwear.tol counts in the nonwear.axis axis are non-zero, and those counts are all less than nonwear.tol.upper. If nonwear.nci is set to TRUE, the non-wear algorithm from the NCI's SAS programs [2] is used. This algorithm classifies as non-wear time any interval of length nonwear.window that starts with a count value of 0, does not contain any periods with (nonwear.tol + 1) consecutive non-zero count values, and does not contain any counts greater than nonwear.tol.upper. Once a non-wear bout is established, it continues until there are (nonwear.tol + 1) consecutive non-zero count values or a single count value greater than nonwear.tol.upper.

The activity bout algorithm operates similarly to the non-wear algorithm. If active.bout.nci is set to FALSE, a `regular' algorithm is used. To illustrate, any interval of length active.bout.length where no more than active.bout.tol minutes have counts less than int.cuts[3], and the counts below int.cuts[3] are all mvpa.bout.tol.lower or greater, is considered an MVPA bout. If active.bout.nci is set to TRUE, the NCI's algorithm is used. This algorithm defines an MVPA bout as an interval that starts with ten consecutive count values greater than or equal to int.cuts[3], allowing for up to active.bout.tol minutes with counts below int.cuts[3]. The first minute of the bout cannot be below int.cuts[3]. Once the MVPA bout is established, it continues until there are (active.bout.tol + 1) consecutive minutes with counts less than int.cuts[3]. The parameters mvpa.bout.tol.lower and vig.bout.tol.lower are not used in the NCI bout algorithm.
}
\value{
A single matrix or a list of two matrices, depending on return.form.
}
\references{
1. National Cancer Institute. Risk factor monitoring and methods: SAS programs for analyzing NHANES 2003-2004 accelerometer data. Available at: \url{http://riskfactor.cancer.gov/tools/nhanes_pam}. Accessed July 31, 2014.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
This function is similar to \code{\link{accel.process.uni}}, but works with triaxial accelerometer data. For example, rather than just calculating a single counts variable for each day of monitoring, accel.process.tri calculates a daily counts variable for the vertical axis, anteroposterior (AP) axis, and mediolateral (ML) axis, and also for the triaxial sum and triaxial vector magnitude (defined as the square root of the sum of the squares of the three axes). Also, the user can choose which of the five signals to use for artifact correction, non-wear detection, activity intensity classification, and hourly counts per minute averaging. 

Some additional information on the package accelerometry and its functions can be found on the author's website, \url{https://sites.google.com/site/danevandomelen/}.
}
\seealso{
\code{\link{accel.artifacts}}, \code{\link{accel.bouts}}, \code{\link{accel.process.uni}}, \code{\link{accel.intensities}}, \code{\link{accel.sedbreaks}}, \code{\link{accel.weartime}}, \code{\link{blockaves}}, \code{\link{movingaves}}, \code{\link{rle2}}, \code{\link{inverse.rle2}}
}
\examples{
# Load in sample data frame
data(tridata)

# Process data and request per-day variables
accel.days1 <- accel.process.tri(counts = tridata[, 1:3], steps = tridata[, 4])

# Process data, but for non-wear detection use triaxial vector magnitude with 90-
# minute window and two-minute tolerance for nonzero counts up to 200
accel.days2 <- accel.process.tri(counts = tridata[, 1:3], steps = tridata[, 4],
                                 nonwear.axis = "mag", nonwear.window = 90,
                                 nonwear.tol = 2, nonwear.tol.upper = 200)
}
\keyword{ accelerometry }
\keyword{ physical activity }
\keyword{ triaxial }