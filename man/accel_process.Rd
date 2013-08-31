\name{accel.process}
\alias{accel.process}
\title{
Process uniaxial minute-to-minute accelerometer data
}
\description{
This function calculates a variety of physical activity variables based on uniaxial minute-to-minute accelerometer data for individual participants. A data dictionary for the variables returned is available on the author's website, \url{https://sites.google.com/site/danevandomelen/}.
}
\usage{
accel.process(counts, steps = NULL, days = NULL, id = NULL, brevity=1, valid.days = 1, 
valid.week.days = 0, valid.weekend.days = 0, int.cuts = c(100, 760, 2020, 5999), 
days.distinct = FALSE, nonwear.window = 60, nonwear.tol = 0, nonwear.tol.upper = 99, 
nonwear.nci = FALSE, weartime.minimum = 600, weartime.maximum = 1200, 
use.partialdays = FALSE, active.bout.length = 10, active.bout.tol = 0, 
mvpa.bout.tol.lower = 0, vig.bout.tol.lower = 0, active.bout.nci = FALSE, 
sed.bout.tol = 0, sed.bout.tol.maximum = int.cuts[2]-1, artifact.thresh = 25000, 
artifact.action = 1, return.averages = FALSE, weekday.weekend = FALSE, 
skipchecks = FALSE)
}
\arguments{
  \item{counts}{
Time series accelerometer counts vector
}
  \item{steps}{
Steps vector; must be same length as counts (if specified).
}
  \item{days}{
Days vector, where 1 = Sunday, ... , 7 = Saturday; must be same length as counts (if specified).
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
Vector of four cut-points from which five intensity ranges are derived. For example, if thresh=c(100,760,2020,5999), minutes with 0-99 counts are classified as intensity level 1, minutes with 100-759 counts are classified as intensity level 2, ... , and minutes with 5999 or greater counts are classified as intensity level 5. Intensities 1-5 typically correspond to sedentary, light, lifestyle, moderate, and vigorous.
}
  \item{days.distinct}{
If TRUE, treat each day of data as distinct, i.e. identify non-wear time and activity bouts in day 1, then day 2, etc.; If FALSE, apply algorithms on continuous basis for full monitoring period.
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
If TRUE, use non-wear algorithm from the NCI SAS programs (see reference 2); if FALSE, use regular algorithm.
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
If TRUE, use activity bouts algorithm from the NCI SAS programs (see reference 2); if FALSE, use regular algorithm.
}
  \item{sed.bout.tol}{
Number of minutes with counts outside sedentary range allowed during sedentary bouts.
}
  \item{sed.bout.tol.maximum}{
Upper cut-off for count values outside sedentary range during a sedentary bout.
}
  \item{artifact.thresh}{
Lower cut-off for counts that are abnormally high and should be considered artifacts.
}
  \item{artifact.action}{
If 1, exclude days that have one or more artifacts; if 2, consider artifacts as non-wear time; if 3, replace artifacts with average of neighboring count values; if 4, take no action.
}
  \item{return.averages}{
If TRUE, function returns vector that summarizes data (e.g. number of valid days, average daily counts, etc.); if FALSE, function returns matrix with activity variables for each day of data.
}
  \item{weekday.weekend}{
If TRUE and return.averages=TRUE, function returns vector that summarizes data across all valid days as well as all valid weekdays and all valid weekend days.
}
  \item{skipchecks}{
If TRUE, function skips error checking code and runs slightly faster.
}
}
\details{
NA
}
\value{
Either a matrix with physical activity variables for each day of data or a vector that summarizes data, i.e. number of valid days, average daily counts, etc.
}
\references{
1. Centers for Disease Control and Prevention (CDC). National Center for Health Statistics (NCHS). National Health and Nutrition Examination Survey Data. Hyattsville, MD: US Department of Health and Human Services, Centers for Disease Control and Prevention, 2003-6 \url{http://www.cdc.gov/nchs/nhanes/nhanes_questionnaires.htm}. Accessed August 12, 2013.

2. National Cancer Institute. Risk factor monitoring and methods: SAS programs for analyzing NHANES 2003-2004 accelerometer data. Available at: \url{http://riskfactor.cancer.gov/tools/nhanes_pam}. Accessed August 12, 2013.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
Leaving days.distinct set to FALSE is strongly encouraged. If set to TRUE, activity bouts and non-wear periods that span the end of one day and the beginning of the next may be missed. This option is included because it is necessary to reproduce the algorithm used in the NCI SAS programs (see reference 2).

The default for weartime.maximum is 1200. Daily wear time greater than 1200 minutes corresponds to less than 4 hours of sleep. In such cases, it seems more likely that the participant slept while wearing the device, and as a result had small movements overnight show up as wear time. However, wear time maximums are not uncommon. Users who wish to use more traditional data processing methodology can set weartime.maximum to 1440. Also, if participants in the study were not instructed to remove the accelerometer during sleeping, weartime.maximum should be set to 1440.

For use.partialdays, one might prefer to include days of monitoring that do not have a full 1440 minutes of data, as long as the wear time criteria are met. However, it seems likely that missing segments of data might tend to occur at a particular time of day (e.g. evening), and physical activity is strongly associated with time of day. Therefore, including such days could introduce bias.

There is no perfect solution for dealing with abnormally high count values, also known as artifacts. The NCI SAS programs (see reference 2) replace artifacts (which they define as the ActiGraph AM-7164 maximum, 32767) with the mean of neighboring count values. This can be done by specifying artifact.thresh=32767 and artifact.action=3. This method may work well, but in many cases count values that are artifacts are surrounded by count values that are only slightly lower, bringing into question whether the entire group of counts is plausible or implausible. Count values at or around the cut-point defined by artifact.thresh can contribute greatly to daily counts. Therefore the default settings, artifact.thresh=25000 and artifact.action=1, simply excludes days of monitoring with one or more count values of 25000 or greater. As this solution is clearly not ideal, users are welcome to choose their own preferred setting for artifact.thresh and artifact.action.

If the user allows for a tolerance in bout detection (e.g. active.bout.tol=2) and does not use the NCI algorithm (active.bout.nci=FALSE), specifying non-zero values for mvpa.bout.tol.lower and vig.bout.tol.lower is highly recommended. Otherwise the algorithm will tend to classify minutes immediately before and after an activity bout as being part of the bout.

Some additional information on the package accelerometry and its functions can be found on the author's website, \url{https://sites.google.com/site/danevandomelen/}.
}
\seealso{
\code{\link{blockaves}}, \code{\link{accel.artifacts}}, \code{\link{accel.bouts}}, \code{\link{accel.intensities}}, \code{\link{accel.sedbreaks}}, \code{\link{accel.weartime}}, \code{\link{movingaves}}
}
\examples{
data(testdata)
id.part1 = testdata[testdata[,"seqn"]==21005,"seqn"]
days.part1 = testdata[testdata[,"seqn"]==21005,"paxday"]
counts.part1 = testdata[testdata[,"seqn"]==21005,"paxinten"]
accel.days = accel.process(counts=counts.part1,days=days.part1,id=id.part1)
accel.averages = accel.process(counts=counts.part1,days=days.part1,id=id.part1,return.averages=TRUE)
}
\keyword{ accelerometry }
\keyword{ physical activity }