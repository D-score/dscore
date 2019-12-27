#' Age-conditional reference distribution of D-score
#'
#' A data frame containing the age-dependent distribution of the
#' D-score for children aged 0-5 years. The distribution is modelled
#' after the LMS distribution (Cole & Green, 1992), and is equal for
#' both boys and girls. The LMS values can be used to graph
#' reference charts and to calculate age-conditonal Z-scores, also
#' known as DAZ.
#'
#' @docType data
#' @format A `data.frame` with 265 rows and 17 variables:
#'
#' | Name | Label |
#' | --- | --------- |
#' | `pop` | Population, either `"dutch"` or `"gcdg"` |
#' | `age` | Decimal age in years |
#' | `mu`  | M-curve, median D-score, P50 |
#' | `sigma` | S-curve, spread expressed as coefficient of variation |
#' | `nu`    | L-curve, the lambda coefficient of the LMS model for skewness |
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
#' The `"dutch"` references were calculated from the SMOCC data, and cover
#' age range 0-2.5 years (van Buuren, 2014).
#' The `"gcdg"` references were calculated from the 15 cohorts of the
#' GCDG-study, and cover age range 0-5 years (Weber, 2019).
#' @examples
#' head(builtin_references)
#' @references
#' Cole TJ, Green PJ (1992). Smoothing reference centile curves: The LMS
#' method and penalized likelihood. Statistics in Medicine, 11(10),
#' 1305-1319.
#'
#' Van Buuren S (2014). Growth charts of human development.
#' Stat Methods Med Res, 23(4), 346-368.
#' [pdf](https://stefvanbuuren.name/publications/2014\%20Growth\%20charts\%20for\%20development\%20-\%20SMMR.pdf)
#'
#' Weber AM, Rubio-Codina M, Walker SP, van Buuren S, Eekhout I,
#' Grantham-McGregor S, Caridad Araujo M, Chang SM, Fernald LCH,
#' Hamadani JD, Hanlon A, Karam SM, Lozoff B, Ratsifandrihamanana L,
#' Richter L, Black MM (2019). The D-score: a metric for interpreting
#' the early development of infants and toddlers across global settings.
#' BMJ Global Health, BMJ Global Health 4: e001724.
#' [pdf](https://gh.bmj.com/content/bmjgh/4/6/e001724.full.pdf).
#'
#' @seealso [dscore()]
"builtin_references"
