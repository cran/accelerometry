\name{testdata}
\alias{testdata}
\docType{data}
\title{
Test dataset for package accelerometry
}
\description{
This dataset is used to illustrate the various functions in the package accelerometry.
}
\usage{data(testdata)}
\format{
  The format is:
 int [1:50400, 1:3] 21005 21005 21005 21005 21005 21005 21005 21005 21005 21005 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr [1:3] "seqn" "paxday" "paxinten"
}
\details{
NA
}
\source{
The data is for from five participants in NHANES 2003-2004 (see reference). The full dataset is available at: \url{http://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Examination&CycleBeginYear=2003}
}
\references{
Centers for Disease Control and Prevention (CDC). National Center for Health Statistics (NCHS). National Health and Nutrition Examination Survey Data. Hyattsville, MD: US Department of Health and Human Services, Centers for Disease Control and Prevention, 2003-6 \url{http://www.cdc.gov/nchs/nhanes/nhanes_questionnaires.htm}. Accessed August 12, 2013.
}
\examples{
data(testdata)
counts.part3 = testdata[testdata[,"seqn"]==21007,"paxinten"]
plot(counts.part3)
}
\keyword{datasets}