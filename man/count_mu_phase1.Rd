% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_mu.R
\name{count_mu_phase1}
\alias{count_mu_phase1}
\title{Median of phase1 references}
\usage{
count_mu_phase1(t)
}
\arguments{
\item{t}{Decimal age, numeric vector}
}
\value{
A vector of length \code{length(t)} with the median of the GCDG references.
}
\description{
Returns the age-interpolated median of the phase1 references
based on LF & SF in GSED-BGD, GSED-PAK, GSED-TZA. This function is used
to set prior mean under keys \code{"293_0"} and \code{"gsed2212"}.
}
\details{
The interpolation is done in two rounds. First round: Calculate D-scores
using .gcdg prior-mean, calculate reference, estimate round 1 parameters
used in this function. Round 2: Calculate D-score using round 1 estimates as
the prior mean (most differences are within 0.1 D-score points),
recalculate references, estimate round 2 parameters used in this function.

Round 1:
Count model: <= 9MN: 21.3449 + 26.4916 t +  7.0251(t + 0.2)
Count model: > 9Mn & <= 3.5 YR: 14.69947 - 12.18636 t + 69.11675(t + 0.92)
Linear model: > 3.5 YRS: 61.40956 + 3.80904 t

Round 2:
Count model: < 9MND: 20.5883 + 27.3376 t +  6.4254(t + 0.2)
Count model: > 9MND & < 3.5 YR: 14.63748 - 12.11774 t + 69.05463(t + 0.92)
Linear model: > 3.5 YRS: 61.37967 + 3.83513 t

The working range is 0-3.5 years. After the age of 3.5 years, the function
will increase at an arbitrary rate of 3.8 D-score points per year.
}
\note{
Internal function. Called by \code{dscore()}
}
\examples{
dscore:::count_mu_phase1(0:5)
}
\author{
Stef van Buuren, on behalf of GSED project
}
