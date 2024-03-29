% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gsample.R
\docType{data}
\name{sample_hf}
\alias{sample_hf}
\title{Sample of 10 children from GSED HF}
\format{
A \code{data.frame} with 10 rows and 57 variables:\tabular{ll}{
   Name \tab Label \cr
   \code{subjid} \tab Integer, child ID \cr
   \code{agedays} \tab Integer, age in days \cr
   \code{hf001} \tab Integer, ...: 1 = yes, 0 = no, NA = not administered \cr
   \code{hf002} \tab Integer, ...: 1 = yes, 0 = no, NA = not administered \cr
   \code{...} \tab and so on.. \cr
}


Sample data for 55 \code{gpa} items forming GSED HF V1
}
\usage{
sample_hf
}
\description{
A demo dataset with developmental scores at the item level for
10 random children from the GSED Phase 1 data.
}
\examples{
head(sample_hf)
}
\seealso{
\code{\link[=dscore]{dscore()}}
}
\keyword{datasets}
