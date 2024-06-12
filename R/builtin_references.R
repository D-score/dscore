#' Collection of age-conditional reference distributions
#'
#' A data frame containing the age-dependent distribution of the
#' D-score for children aged 0-5 years. The distribution is modelled
#' after the LMS distribution (Cole & Green, 1992) or BCT model
#' (Stasinopoulos & Rigby, 2022) and is equal for
#' both boys and girls. The LMS/BCT values can be used to graph
#' reference charts and to calculate age-conditional Z-scores, also
#' known as the *Development-for-Age Z-score (DAZ)*.
#'
#' @docType data
#' @format A `data.frame` with the following variables:
#'
#' | Name    | Label |
#' | ------- | --------- |
#' | `population` | Name of the reference population |
#' | `key`   | D-score key, e.g., `"dutch"`, `"gcdg"` or `"gsed"` |
#' | `distribution` | Distribution family: `"LMS"` or `"BCT"` |
#' | `age`   | Decimal age in years |
#' | `mu`    | M-curve, median D-score, P50 |
#' | `sigma` | S-curve, spread expressed as coefficient of variation |
#' | `nu`    | L-curve, the lambda coefficient of the LMS/BCT model for skewness |
#' | `tau`   | Kurtosis parameter in the BCT model |
#' | `P3`    | P3 percentile |
#' | `P10`   | P10 percentile |
#' | `P25`   | P25 percentile |
#' | `P50`   | P50 percentile |
#' | `P75`   | P75 percentile |
#' | `P90`   | P90 percentile |
#' | `P97`   | P97 percentile |
#' | `SDM2`  | -2SD centile |
#' | `SDM1`  | -1SD centile |
#' | `SD0`   | 0SD centile, median |
#' | `SDP1`  | +1SD centile |
#' | `SDP2`  | +2SD centile |
#'
#' @details
#' Here are more details on the reference population:
#' The `"dutch"` references were calculated from the SMOCC data, and cover
#' age range 0-2.5 years (van Buuren, 2014).
#' The `"gcdg"` references were calculated from the 15 cohorts of the
#' GCDG-study, and cover age range 0-5 years (Weber, 2019).
#' The `"phase1"` references were calculated from the GSED Phase 1 validation
#' data (GSED-BGD, GSED-PAK, GSED-TZA) cover age range 2w-3.5 years. The
#' age range 3.5-5 yrs is linearly extrapolated and are only indicative.
#' The `"preliminary_standards"` were calculated from the GSED Phase 1 validation
#' data (GSED-BGD, GSED-PAK, GSED-TZA) using a subset of children with
#' covariate indicating healthy development.
#'
#' @examples
#' # get an overview of available references per key
#' table(builtin_references$population, builtin_references$key)
#' @references
#' Cole TJ, Green PJ (1992). Smoothing reference centile curves: The LMS
#' method and penalized likelihood. Statistics in Medicine, 11(10),
#' 1305-1319.
#'
#' Van Buuren S (2014). Growth charts of human development.
#' Stat Methods Med Res, 23(4), 346-368.
#' <https://stefvanbuuren.name/publication/van-buuren-2014-gc/>
#'
#' Weber AM, Rubio-Codina M, Walker SP, van Buuren S, Eekhout I,
#' Grantham-McGregor S, Caridad Araujo M, Chang SM, Fernald LCH,
#' Hamadani JD, Hanlon A, Karam SM, Lozoff B, Ratsifandrihamanana L,
#' Richter L, Black MM (2019). The D-score: a metric for interpreting
#' the early development of infants and toddlers across global settings.
#' BMJ Global Health, BMJ Global Health 4: e001724.
#' <https://gh.bmj.com/content/bmjgh/4/6/e001724.full.pdf>
#'
#' Stasinopoulos M, Rigby R (2022). gamlss.dist: Distributions for
#' Generalized Additive Models for Location Scale and Shape,
#' R package version 6.0-3,
#' <https://CRAN.R-project.org/package=gamlss.dist>
#' @seealso [dscore()]
"builtin_references"
