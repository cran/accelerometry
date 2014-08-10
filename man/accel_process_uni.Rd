\name{accel.process.uni}
\alias{accel.process.uni}
\title{
Process uniaxial minute-to-minute accelerometer data
}
\description{
This function calculates a variety of physical activity variables based on uniaxial minute-to-minute accelerometer data for individual participants. A data dictionary for the variables returned is available here: \url{https://sites.google.com/site/danevandomelen/r-package-accelerometry/data-dictionary}.
}
\usage{
accel.process.uni(counts, steps = NULL, nci.methods = FALSE, 
                  start.date = as.Date("2014/1/5"), start.time = "00:00:00", id = NULL, 
                  brevity = 1, valid.days = 1, valid.week.days = 0, 
                  valid.weekend.days = 0, int.cuts = c(100, 760, 2020, 5999), 
                  cpm.nci = FALSE, days.distinct = FALSE, nonwear.window = 60, 
                  nonwear.tol = 0, nonwear.tol.upper = 99, nonwear.nci = FALSE, 
                  weartime.minimum = 600, weartime.maximum = 1440, 
                  partialday.minimum = 1440, active.bout.length = 10, 
                  active.bout.tol = 0, mvpa.bout.tol.lower = 0, vig.bout.tol.lower = 0, 
                  active.bout.nci = FALSE, sed.bout.tol = 0, 
                  sed.bout.tol.maximum = int.cuts[2] - 1, artifact.thresh = 25000, 
                  artifact.action = 1, weekday.weekend = FALSE, return.form = 2)
}
\arguments{
  \item{counts}{
Time series accelerometer counts vector
}
  \item{steps}{
Steps vector. If specified, must be same length as counts.
}
  \item{nci.methods}{
If TRUE, inputs are set to replicate the data processing methods used by the NCI's SAS programs [2]. More specifically: 

valid.days = 4; valid.week.days = 0; valid.weekend.days = 0; int.cuts = c(100, 760, 2020, 5999); cpm.nci = TRUE; days.distinct = TRUE; nonwear.window = 60; nonwear.tol = 2; nonwear.tol.upper = 100; nonwear.nci = TRUE; weartime.minimum = 600; weartime.maximum = 1440; partialday.minimum = 1440; active.bout.length = 10; active.bout.tol = 2; mvpa.bout.tol.lower = 0; vig.bout.tol.lower = 0; active.bout.nci = TRUE; sed.bout.tol = 0; sed.bout.tol.maximum = 759; artifact.thresh = 32767; artifact.action = 3

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
  \item{int.cuts}{
Vector of four cut-points from which five intensity ranges are derived. For example, if thresh = c(100, 760, 2020, 5999), minutes with 0-99 counts are classified as intensity level 1, minutes with 100-759 counts are classified as intensity level 2, ... , and minutes with 5999 or greater counts are classified as intensity level 5. Intensities 1-5 typically correspond to sedentary, light, lifestyle, moderate, and vigorous.
}
  \item{cpm.nci}{
If TRUE, average counts per minute is calculated by dividing average daily counts by average daily weartime, as opposed to averaging each day's counts per minute value. In general, leave as FALSE unless you want to replicate the NCI's SAS programs [2].
}
  \item{days.distinct}{
If TRUE, treat each day of data as distinct, i.e. identify non-wear time and activity bouts in day 1, then day 2, etc.; If FALSE, apply algorithms on continuous basis for full monitoring period. If protocol has participants remove accelerometer for sleep, strongly recommend setting to FALSE to capture non-wear periods that start between 11 pm and midnight.
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
If TRUE, use non-wear algorithm from the NCI's SAS programs [2]; if FALSE, use regular algorithm (see Details).
}
  \item{weartime.minimum}{
Minimum number of wear time minutes for a day of monitoring to be considered valid.
}
  \item{weartime.maximum}{
Maximum number of wear time minutes for a day of monitoring to be considered valid. The default is 1440, but it may be better to set it to a lower value (e.g. 1200) if participants were instructed to remove their accelerometers for sleeping. Daily wear time greater than 1200 minutes corresponds to less than 4 hours of sleep. In these cases it seems more likely that the participant slept while wearing the device, and as a result had small movements overnight show up as wear time. This could inflate estimates of sedentary time and shrink estimates of physical activity, e.g. counts per minute. 
}
  \item{partialday.minimum}{
Minimum number of minutes for a partial day of monitoring to be processed and potentially considered valid for analysis (generally applies only to the first and last days of monitoring, which may not cover full 24-hour periods). This input is included because some researchers may prefer to exclude a day that only has data from, say, 1 pm to midnight. Even though there may be sufficient wear time during that period to be classified as a valid day, the missing chunk of data prior to 1 pm may result in the day not being representative of the participant's usual physical activity.
}
  \item{active.bout.length}{
Minimum length of moderate-to-vigorous physical activity (MVPA) and vigorous physical activity (VPA) bouts.
}
  \item{active.bout.tol}{
Number of minutes with counts below the required intensity level allowed during MVPA and VPA bouts. If set to a non-zero value, and active.bout.nci is FALSE, specifying non-zero values for mvpa.bout.tol.lower and vig.bout.tol.lower is highly recommended. Otherwise the algorithm will tend to classify minutes immediately before and after an activity bout as being part of the bout.
}
  \item{mvpa.bout.tol.lower}{
Lower cut-off for count values outside of MVPA intensity range during an MVPA bout.
}
  \item{vig.bout.tol.lower}{
Lower cut-off for count values outside of VPA intensity range during a VPA bout.
}
  \item{active.bout.nci}{
If TRUE, use activity bouts algorithm from the NCI's SAS programs [2]; if FALSE, use regular algorithm (see Details).
}
  \item{sed.bout.tol}{
Number of minutes with counts outside sedentary range allowed during sedentary bouts.
}
  \item{sed.bout.tol.maximum}{
Upper cut-off for count values outside sedentary range during a sedentary bout.
}
  \item{artifact.thresh}{
Lower cut-off for counts that are abnormally high and should be considered artifacts (see Note).
}
  \item{artifact.action}{
If 1, exclude days that have one or more artifacts; if 2, consider artifacts as non-wear time; if 3, replace artifacts with average of neighboring count values; if 4, take no action (see Note).
}
  \item{weekday.weekend}{
If TRUE, function computes physical activity averages for weekdays and weekend days separately (in addition to daily averages for all valid days, which are computed regardless). If FALSE, function only computes averages for all valid days.
}
  \item{return.form}{
If 1, function returns physical activity variables on per-person basis, i.e. daily averages for each participant; if 2, function returns variables on per-day basis; if 3, function returns both via a list.
}
}
\details{
The algorithm used to identify non-wear time is defined by function inputs nonwear.window, nonwear.tol, nonwear.tol.upper, and nonwear.nci. If nonwear.nci is set to FALSE, a `regular' non-wear algorithm is used. This algorithm classifies as non-wear time any interval of length nonwear.window in which no more than nonwear.tol counts are non-zero, and those counts are all less than nonwear.tol.upper. If nonwear.nci is set to TRUE, the non-wear algorithm from the NCI's SAS programs [2] is used. This algorithm classifies as non-wear time any interval of length nonwear.window that starts with a count value of 0, does not contain any periods with (nonwear.tol + 1) consecutive non-zero count values, and does not contain any counts greater than nonwear.tol.upper. Once a non-wear bout is established, it continues until there are (nonwear.tol + 1) consecutive non-zero count values or a single count value greater than nonwear.tol.upper. 

The activity bout algorithm operates similarly to the non-wear algorithm. If active.bout.nci is set to FALSE, a `regular' algorithm is used. To illustrate, any interval of length active.bout.length where no more than active.bout.tol minutes have counts less than int.cuts[3], and the counts below int.cuts[3] are all mvpa.bout.tol.lower or greater, is considered an MVPA bout. If active.bout.nci is set to TRUE, the NCI's algorithm is used. This algorithm defines an MVPA bout as an interval that starts with ten consecutive count values greater than or equal to int.cuts[3], allowing for up to active.bout.tol minutes with counts below int.cuts[3]. The first minute of the bout cannot be below int.cuts[3]. Once the MVPA bout is established, it continues until there are (active.bout.tol + 1) consecutive minutes with counts less than int.cuts[3]. The parameters mvpa.bout.tol.lower and vig.bout.tol.lower are not used in the NCI bout algorithm.
}
\value{
A single matrix or a list of two matrices, depending on return.form.
}
\references{
1. Centers for Disease Control and Prevention (CDC). National Center for Health Statistics (NCHS). National Health and Nutrition Examination Survey Data. Hyattsville, MD: US Department of Health and Human Services, Centers for Disease Control and Prevention, 2003-6 \url{http://www.cdc.gov/nchs/nhanes/nhanes_questionnaires.htm}. Accessed July 31, 2014.

2. National Cancer Institute. Risk factor monitoring and methods: SAS programs for analyzing NHANES 2003-2004 accelerometer data. Available at: \url{http://riskfactor.cancer.gov/tools/nhanes_pam}. Accessed July 31, 2014.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
There is no perfect solution for dealing with abnormally high count values, also known as artifacts. The NCI's SAS programs replace artifacts (which they define as the ActiGraph AM-7164 maximum, 32767) with the mean of neighboring count values [2]. This can be done by specifying artifact.thresh = 32767 and artifact.action = 3. This method may work well, but in many cases count values that are artifacts are surrounded by count values that are only slightly lower, bringing into question whether the entire group of counts is plausible or implausible. Count values at or around the cut-point defined by artifact.thresh can contribute greatly to daily counts. Therefore the default settings, artifact.thresh = 25000 and artifact.action = 1, simply excludes days of monitoring with one or more count values of 25000 or greater. As this solution is clearly not ideal, users are welcome to choose their own preferred setting for artifact.thresh and artifact.action.

Some additional information on the package accelerometry and its functions can be found on the author's website, \url{https://sites.google.com/site/danevandomelen/}.
}
\seealso{
\code{\link{accel.artifacts}}, \code{\link{accel.bouts}}, \code{\link{accel.process.tri}}, \code{\link{accel.intensities}}, \code{\link{accel.sedbreaks}}, \code{\link{accel.weartime}}, \code{\link{blockaves}}, \code{\link{movingaves}}, \code{\link{rle2}}, \code{\link{inverse.rle2}}
}
\examples{
# Load in sample data frame
data(unidata)

# Get data from ID number 21005
id.part1 <- unidata[unidata[, "seqn"] == 21005, "seqn"]
counts.part1 <- unidata[unidata[, "seqn"] == 21005, "paxinten"]

# Process data from ID 21005 and request per-day variables
accel.days <- accel.process.uni(counts = counts.part1, id = id.part1)

# Process data from ID 21005 and request daily averages
accel.averages <- accel.process.uni(counts = counts.part1, id = id.part1, 
                                    return.form = 1)

# Process data from ID 21005 and request per-day variables and daily averages
accel.list <- accel.process.uni(counts = counts.part1, id = id.part1, 
                                    return.form = 3)
                  
# Process data according to methods used in NCI's SAS programs [2]
accel.nci1 <- accel.process.uni(counts = counts.part1, id = id.part1, brevity = 2, 
                                valid.days = 4, cpm.nci = TRUE, days.distinct = TRUE, 
                                nonwear.tol = 2, nonwear.tol.upper = 100,
                                nonwear.nci = TRUE, weartime.maximum = 1440, 
                                active.bout.tol = 2, active.bout.nci = TRUE, 
                                artifact.thresh = 32767, artifact.action = 3, 
                                return.form = 3)
                               
# Repeat, but use nci.methods input for convenience
accel.nci2 <- accel.process.uni(counts = counts.part1, id = id.part1, nci.methods = TRUE, 
                                brevity = 2, return.form = 3)
                                
# Verify that previous two function calls give identical results
all(accel.nci1[[1]] == accel.nci2[[1]])
all(accel.nci1[[2]] == accel.nci2[[2]])
}
\keyword{ accelerometry }
\keyword{ physical activity }