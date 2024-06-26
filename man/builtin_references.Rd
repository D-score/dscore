% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/builtin_references.R
\docType{data}
\name{builtin_references}
\alias{builtin_references}
\title{Collection of age-conditional reference distributions}
\format{
A \code{data.frame} with the following variables:\tabular{ll}{
   Name \tab Label \cr
   \code{population} \tab Name of the reference population \cr
   \code{key} \tab D-score key, e.g., \code{"dutch"}, \code{"gcdg"} or \code{"gsed"} \cr
   \code{distribution} \tab Distribution family: \code{"LMS"} or \code{"BCT"} \cr
   \code{age} \tab Decimal age in years \cr
   \code{mu} \tab M-curve, median D-score, P50 \cr
   \code{sigma} \tab S-curve, spread expressed as coefficient of variation \cr
   \code{nu} \tab L-curve, the lambda coefficient of the LMS/BCT model for skewness \cr
   \code{tau} \tab Kurtosis parameter in the BCT model \cr
   \code{P3} \tab P3 percentile \cr
   \code{P10} \tab P10 percentile \cr
   \code{P25} \tab P25 percentile \cr
   \code{P50} \tab P50 percentile \cr
   \code{P75} \tab P75 percentile \cr
   \code{P90} \tab P90 percentile \cr
   \code{P97} \tab P97 percentile \cr
   \code{SDM2} \tab -2SD centile \cr
   \code{SDM1} \tab -1SD centile \cr
   \code{SD0} \tab 0SD centile, median \cr
   \code{SDP1} \tab +1SD centile \cr
   \code{SDP2} \tab +2SD centile \cr
}
}
\usage{
builtin_references
}
\description{
A data frame containing the age-dependent distribution of the
D-score for children aged 0-5 years. The distribution is modelled
after the LMS distribution (Cole & Green, 1992) or BCT model
(Stasinopoulos & Rigby, 2022) and is equal for
both boys and girls. The LMS/BCT values can be used to graph
reference charts and to calculate age-conditional Z-scores, also
known as the \emph{Development-for-Age Z-score (DAZ)}.
}
\details{
Here are more details on the reference population:
The \code{"dutch"} references were calculated from the SMOCC data, and cover
age range 0-2.5 years (van Buuren, 2014).
The \code{"gcdg"} references were calculated from the 15 cohorts of the
GCDG-study, and cover age range 0-5 years (Weber, 2019).
The \code{"phase1"} references were calculated from the GSED Phase 1 validation
data (GSED-BGD, GSED-PAK, GSED-TZA) cover age range 2w-3.5 years. The
age range 3.5-5 yrs is linearly extrapolated and are only indicative.
The \code{"preliminary_standards"} were calculated from the GSED Phase 1 validation
data (GSED-BGD, GSED-PAK, GSED-TZA) using a subset of children with
covariate indicating healthy development.
}
\examples{
# get an overview of available references per key
table(builtin_references$population, builtin_references$key)
}
\references{
Cole TJ, Green PJ (1992). Smoothing reference centile curves: The LMS
method and penalized likelihood. Statistics in Medicine, 11(10),
1305-1319.

Van Buuren S (2014). Growth charts of human development.
Stat Methods Med Res, 23(4), 346-368.
\url{https://stefvanbuuren.name/publication/van-buuren-2014-gc/}

Weber AM, Rubio-Codina M, Walker SP, van Buuren S, Eekhout I,
Grantham-McGregor S, Caridad Araujo M, Chang SM, Fernald LCH,
Hamadani JD, Hanlon A, Karam SM, Lozoff B, Ratsifandrihamanana L,
Richter L, Black MM (2019). The D-score: a metric for interpreting
the early development of infants and toddlers across global settings.
BMJ Global Health, BMJ Global Health 4: e001724.
\url{https://gh.bmj.com/content/bmjgh/4/6/e001724.full.pdf}

Stasinopoulos M, Rigby R (2022). gamlss.dist: Distributions for
Generalized Additive Models for Location Scale and Shape,
R package version 6.0-3,
\url{https://CRAN.R-project.org/package=gamlss.dist}
}
\seealso{
\code{\link[=dscore]{dscore()}}
}
\keyword{datasets}
