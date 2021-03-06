% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{bouts}
\alias{bouts}
\title{Physical Activity Bout Detection}
\usage{
bouts(counts, weartime = NULL, bout_length = 10L, thresh_lower = 0L,
  thresh_upper = 100000L, tol = 0L, tol_lower = 0L, tol_upper = 100000L,
  nci = FALSE, days_distinct = FALSE)
}
\arguments{
\item{counts}{Integer vector with accelerometer count values.}

\item{weartime}{Integer vector with 1's for wear time minutes and 0's for 
non-wear time minutes.}

\item{bout_length}{Integer value specifying minimum length of an activity 
bout.}

\item{thresh_lower}{Integer value specifying lower bound for count values to 
be included for the intensity level.}

\item{thresh_upper}{Integer value specifying upper bound for count values to 
be included for the intensity level.}

\item{tol}{Integer value specifying number of minutes with count values 
outside of [\code{thresh_lower}, \code{thresh_upper}] to allow during an 
activity bout.}

\item{tol_lower}{Integer value specifying lower cut-off for count values 
outside of intensity range during an activity bout.}

\item{tol_upper}{Integer value specifying upper cut-off for count values 
outside of intensity range during an activity bout.}

\item{nci}{Logical value for whether to use algorithm from NCI's SAS 
programs. See \bold{Details}.}

\item{days_distinct}{Logical value for whether to treat each day of data as 
distinct, i.e. identify non-wear time and activity bouts for day 1, then day 
2, etc. If \code{FALSE}, algorithm is applied to full monitoring period 
continuously. If protocol has participants remove accelerometer for sleep, 
strongly recommend setting to \code{FALSE} to capture non-wear periods that 
start between 11 pm and midnight. Function assumes that first 1440 data 
points are day 1, next 1440 are day 2, and so on.}
}
\value{
Integer vector with 1's for minutes that are part of an activity 
bout and 0's for minutes that are not.
}
\description{
Identify bouts of physical activity based on a vector of accelerometer count 
values.
}
\details{
If \code{nci = FALSE}, the algorithm uses a moving window to go through 
every possible interval of length \code{bout_length} in \code{counts}. Any 
interval in which all counts are >= \code{tol_lower} and <= 
\code{tol_upper}, and no more than \code{tol} counts are less than 
\code{thresh_lower} or greater than \code{thresh_upper}, is classified as an 
activity bout.

If \code{nci = TRUE}, activity bouts are classified according to the 
algorithm used in the NCI's SAS programs. Briefly, this algorithm defines an 
activity bout as an interval of length \code{bout_length} that starts with a 
count value in \code{[thresh_lower, thresh_upper]} and has no more than 
\code{tol} counts outside of that range. If these criteria are met, the bout 
continues until there are \code{(tol + 1)} consecutive minutes outside of 
\code{[thresh_lower, thresh_upper]}. The parameters \code{tol_lower} and 
\code{tol_upper} are not used.

If the user allows for a tolerance (e.g. \code{tol = 2}) and does not use 
the NCI algorithm (i.e. \code{nci = FALSE}), specifying a non-zero value for 
\code{tol_lower} is highly recommended. Otherwise the algorithm will tend to 
classify minutes immediately before and after an activity bout as being part 
of the bout.

Specifying \code{thresh_lower} while using an arbitrarily large value for 
\code{thresh_upper} is generally recommended. Specifying both of these 
parameters can be overly restrictive in that the algorithm may miss bouts of 
activity in which counts are consistently high, but not exclusively in one 
intensity range.
}
\examples{
# Load accelerometer data for first 5 participants in NHANES 2003-2004
data(unidata)

# Get data from ID number 21005
counts.part1 <- unidata[unidata[, "seqn"] == 21005, "paxinten"]

# Identify periods of valid wear time
wear.part1 <- weartime(counts = counts.part1)

# Identify moderate-to-vigorous activity bouts
mvpa.bouts <- bouts(counts = counts.part1, weartime = wear.part1, 
                    thresh_lower = 2020)


}
\references{
National Cancer Institute. Risk factor monitoring and methods: SAS programs 
for analyzing NHANES 2003-2004 accelerometer data. Available at: 
\url{http://riskfactor.cancer.gov/tools/nhanes_pam}. Accessed Aug. 19, 2018.

Acknowledgment: This material is based upon work supported by the National 
Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
