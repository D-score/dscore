% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/daz.R
\name{daz}
\alias{daz}
\alias{zad}
\title{D-score standard deviation score: DAZ}
\usage{
daz(d, x, reference = get_reference(), dec = 3)

zad(z, x, reference = get_reference(), dec = 2)
}
\arguments{
\item{d}{Vector of D-scores}

\item{x}{Vector of ages (decimal age)}

\item{reference}{A \code{data.frame} with the LMS reference values.
The default uses the \code{get_reference()} function. This selects
a subset of rows from the \code{builtin_references}.}

\item{dec}{The number of decimals (default \code{dec = 3}).}

\item{z}{Vector of standard deviation scores (DAZ)}
}
\value{
Unnamed numeric vector with Z-scores of length \code{length(d)}.

Unnamed numeric vector with D-scores of length \code{length(z)}.
}
\description{
The \code{daz()} function calculated the
"Development for Age Z-score".
The DAZ represents a child's D-score after adjusting
for age by an external age-conditional reference.
The \code{zad()} is the inverse of \code{daz()}: Given age and
the Z-score, it finds the raw D-score.
}
\details{
Note 1: The Box-Cox Cole and Green (BCCG) and Box-Cox t (BCT)
distributions model only positive D-score values. To increase
robustness, the \code{daz()} and \code{zad()} functions will round up any
D-scores lower than 1.0 to 1.0.

Note 2: The \code{daz()} and \code{zad()} function call modified version of the
\code{pBCT()} and \code{qBCT()} functions from \code{gamlss} for better handling
of \code{NA}'s and rounding.
}
\examples{
# using GSED Phase 1 reference
daz(d = c(35, 50), x = c(0.5, 1.0))

# using Dutch reference
daz(d = c(35, 50), x = c(0.5, 1.0), reference = get_reference("gcdg"))

# using Dutch reference
daz(d = c(35, 50), x = c(0.5, 1.0), reference = get_reference("dutch"))
# population median at ages 0.5, 1 and 2 years, phase1 reference
zad(z = rep(0, 3), x = c(0.5, 1, 2))

# population median at ages 0.5, 1 and 2 years, gcdg reference
zad(z = rep(0, 3), x = c(0.5, 1, 2), reference = get_reference("gcdg"))

# population median at ages 0.5, 1 and 2 years, dutch reference
zad(z = rep(0, 3), x = c(0.5, 1, 2), reference = get_reference("dutch"))

# percentiles of D-score reference
g <- expand.grid(age = seq(0.1, 2, 0.1), p = c(0.1, 0.5, 0.9))
d <- zad(z = qnorm(g$p), x = g$age)
matplot(
  x = matrix(g$age, ncol = 3), y = matrix(d, ncol = 3), type = "l",
  lty = 1, col = "blue", xlab = "Age (years)", ylab = "D-score"
)
}
\references{
Cole TJ, Green PJ (1992). Smoothing reference centile curves: The LMS
method and penalized likelihood. Statistics in Medicine, 11(10),
1305-1319.
}
\seealso{
\code{\link[=dscore]{dscore()}}
}
\author{
Stef van Buuren 2020
}
