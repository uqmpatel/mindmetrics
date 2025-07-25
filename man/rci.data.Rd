% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{rci.data}
\alias{rci.data}
\title{rci.data}
\format{
A 3 x 160 data frame. Pre and post scores for 160 matched pairs with identifiers. \cr

\describe{
\item{ID}{\verb{[character]} = unique identifier }
\item{pre}{\verb{[numeric]} = Baseline hypothetical psychometric instrument scores prior to an event of interest}
\item{post}{\verb{[numeric]} = Follow-up hypothetical psychometric instrument scores following to an event of interest}
} \cr
}
\source{
Reproduce the data using the code below: \cr
}
\usage{
rci.data
}
\description{
Data frame containing total pre and post scores for a hypothetical psychometric instrument. Total scores would ordinarily be computed from individual instrument items using published instructions. \cr
}
\examples{

# Inspect the hypothetical rci.data dataset:

View(rci.data)
names(rci.data)

# Standard deviation of the pre scores:

sd(rci.data$pre, na.rm = TRUE)

# Standard deviation of the post scores:

sd(rci.data$post, na.rm = TRUE)


# rci.data dataframe is generated using the following code:

set.seed(321)
sample.size = 160

 rci.data <-  data.frame(
   ID = c(paste0( 'ID', stringr::str_pad( (seq(1, sample.size, by = 1) ), 4, 'left', '0' ) ) ),
   pre = replace(round(abs(rnorm(sample.size, 29, 12.2)), 0 ) , round(runif(n = 19, min = 1, max = sample.size), 0),  NA  )   ,
   post = replace(round(abs(rnorm(sample.size, 22, 8.3)), 0 ) , round(runif(n = 28, min = 1, max = sample.size), 0),  NA  )  )




}
\keyword{datasets}
