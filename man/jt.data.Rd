% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{jt.data}
\alias{jt.data}
\title{jt.data}
\format{
A 30 x 5 data frame: identifiers for 30 matched pairs with pre post scores for two (DAS & GDS) instruments. \cr

\describe{
\item{subject}{\verb{[numeric]} = Unique identifier }
\item{DAS.Pretest}{\verb{[numeric]} = Baseline DAS psychometric instrument scores prior to an event of interest}
\item{DAS.Posttest}{\verb{[numeric]} = Follow-up DAS psychometric instrument scores following to an event of interest}
\item{GDS.Pretest}{\verb{[numeric]}  = Baseline GDS psychometric instrument scores prior to an event of interest}
\item{GDS.Posttest}{\verb{[numeric]} = Follow-up GDS psychometric instrument scores following  an event of interest}
} \cr
}
\source{
Listed in \href{https://pubmed.ncbi.nlm.nih.gov/2002127/}{Jacobson and Truax (1991)}.
Reproduce the data using the code below: \cr
}
\usage{
jt.data
}
\description{
Example pre and post scores for thirty participant dyad's as included in \href{https://pubmed.ncbi.nlm.nih.gov/2002127/}{Jacobson and Truax (1991)}.
The data set lists results from the Dyadic Adjustment Scale (DAS; \href{https://doi.org/10.2307/350547}{Spanier, 1976}) and the Global Distress Scale of the
Marital Satisfaction Inventory (MSI)(GDS; \href{https://doi.org/10.2307/350547}{Snyder, 1979}).
Both instruments were computed for thirty dyad's at both pre and post. \cr
}
\examples{

# Inspect the example data given by Jacobson and Truax (1991):

View(jt.data)
names(jt.data)

# Standard deviation of the DAS pre scores:

sd(jt.data$DAS.Pretest, na.rm = TRUE)

# Standard deviation of the GDS pre scores:

sd(jt.data$GDS.Pretest, na.rm = TRUE)


# the jt.data data frame is generated using the following code:

DAS.data <- data.frame( subject = seq(1,30, by = 1),
                       DAS.Pretest =  c(90.5, 74.0,  97.0, 73.5, 61.0, 66.5, 68.5,  86.5,  88.5, 68.5, 98.0,  80.5, 89.5,  91.5, 83.5, 60.5, 83.0,  88.0,  98.5,  78.5,   99.5,  79.5,  84.5,  92.5,  93.0, 85.0,  64.0, 61.0, 80.0, 82.5),
                       DAS.Posttest = c(97.0, 124.0, 97.5, 88.0, 96.5, 62.5, 112.5, 103.5, 90.0, 82.5, 105.0, 99.5, 112.5, 101.0, 99.5, 79.5, 88.0, 100.5, 119.0, 116.0,  116.0, 129.0, 113.0, 118.0, 92.0, 114.0, 68.0, 52.0, 60.5, 104.5))

GDS.data <- data.frame( subject = seq(1,30, by = 1),
                       GDS.Pretest =  c(68.0, 74.5, 58.5, 73.5, 78.5, 76.0, 76.5, 63.0, 70.0, 75.0, 63.5, 73.5, 71.5, 63.5, 57.0, 75.0, 63.0, 75.0, 71.5, 68.0, 75.5, 67.5, 62.5, 69.5, 61.0, 67.0, 75.5, 75.5, 69.5, 66.5),
                      GDS.Posttest = c(62.5, 56.0, 58.0, 71.0, 60.5, 77.0, 58.5, 52.0, 65.5, 73.0, 64.0, 55.5, 53.0, 55.0, 50.0, 78.0, 65.5, 62.0, 60.5, 51.0, 50.0, 44.0, 55.5, 56.0, 60.5, 47.5, NA, NA, NA , NA))

jt.data <- merge(DAS.data, GDS.data, by = 'subject', all = TRUE)




}
\keyword{datasets}
