#' Project On Preterm and Small for gestational age infants (POPS)
#' 
#' A dataset with developmental scores at the item level from a subset
#' of 25 children from the POPS cohort. 
#' 
#' @docType data
#' @format A \code{data.frame} with 100 rows (25 children times 4 time points) and 65 variables:
#' \describe{
#' \item{patid}{Patient ID (scrambled)}
#' \item{gender}{1=male, 1 = female}
#' \item{gestionalage}{Gestational age in fractional weeks}
#' \item{moment}{Sequence number}
#' \item{age}{Age in days}
#' \item{occ}{Measurement occasion (1-4)}
#' \item{daycor}{Corrected age in days}
#' \item{Fixateseyes}{Fixates eyes: 1 = yes, 0 = no}
#' \item{\dots}{and so on..}
#' \item{dscore}{Calculated D-score (needed for \code{daz} example code)}
#' }
#' 
#' @examples 
#' head(popsdemo)
#' 
#' @source \url{https://www.tno.nl/nl/aandachtsgebieden/gezond-leven/prevention-work-health/voor-tijdens-en-na-de-zwangerschap/pops-langlopend-onderzoek-onder-prematuren-en-baby-s-met-laag-geboortegewicht/}
#' \code{\link{dscore}}
"popsdemo"


#' Itembank containing difficulties per item
#' 
#' A data frame with administrative information per item: 
#' item name (in various systems), item labels and difficulty. 
#' 
#' @docType data
#' @format A \code{data.frame} with 112 rows and 13 variables:
#' \describe{
#' \item{lex_dutch1996}{Item naming, VWO1996 version}
#' \item{lex_dutch2005}{Item naming, VWO2005 version}
#' \item{lex_dutch1983}{Item naming, VWO1983 version}
#' \item{lex_smocc}{Item naming, original SMOCC variable names}
#' \item{lex_ghap}{Item naming used by GHAP platform of BMGF}
#' \item{lex_gcdg}{Item naming for GCDG project}
#' \item{lex_gsed}{Item naming for GSED project}
#' \item{occ}{Occasion number}
#' \item{labelNL}{Label (Dutch)}
#' \item{labelEN}{Label (English)}
#' \item{tau}{Difficulty level}
#' \item{m}{ }
#' \item{LR}{Left-right item scores}
#' \item{hot}{Item used to estimate D-score? (1=y, 0=no)}
#' }
#' 
#' @examples 
#' head(itembank)
#' 
#' @references
#' Van Buuren S (2014). Growth charts of human development.
#' Stat Methods Med Res, 23(4), 346-368.
#' @seealso \code{\link{dscore}}, \code{\link{gettau}}
"itembank"

#' Age-conditional reference distribution of D-score
#' 
#' A data frame containing the age-dependent distribution of the 
#' D-score for children aged 14-1015 days. The distribution is modelled
#' after the LMS distribution (Cole & Green, 1992), and is equal for
#' both boys and girls. The LMS values can be used to graph 
#' reference charts and to calculate age-conditonal Z-scores, also 
#' known as DAZ.
#' 
#' @docType data
#' @format A \code{data.frame} with 144 rows and 19 variables:
#' \describe{
#' \item{day}{Age in days}
#' \item{week}{Age in weeks}
#' \item{year}{Decimal age in years}
#' \item{mu}{M-curve, median D-score, P50}
#' \item{sigma}{S-curve, spread expressed as coefficient of variation}
#' \item{nu}{L-curve, the lambda coefficient of the LMS model for skewness}
#' \item{P3}{Calculated third percentile}
#' \item{\dots}{And so on..}
#' }
#' @examples 
#' head(Dreference)
#' 
#' @references
#' Cole TJ, Green PJ (1992). Smoothing reference centile curves: The LMS 
#' method and penalized likelihood. Statistics in Medicine, 11(10), 
#' 1305-1319.
#' 
#' Van Buuren S (2014). Growth charts of human development.
#' Stat Methods Med Res, 23(4), 346-368.
#' @seealso \code{\link{dscore}}
"Dreference"

#' Itembank from model 565_18
#' 
#' A data frame with administrative information per item: 
#' item name (in various systems), item labels and difficulty. 
#' 
#' The itembank was calculated from 15 cohorts of the GCDG-study.
#' 
#' @docType data
#' @format A \code{data.frame} with 565 rows and 6 variables:
#' \describe{
#' \item{lex_gcdg}{Item naming for GCDG project}
#' \item{instrument}{Instrument name}
#' \item{equate}{Equate group}
#' \item{domain}{Developmental domain}
#' \item{label}{Item label}
#' \item{tau}{Difficulty level, model 565_18}
#' }
#' 
#' @examples 
#' head(gcdg_itembank)
#' 
#' @seealso \code{\link{dscore}}, \code{\link{gettau}}
"gcdg_itembank"

#' Age-conditional reference distribution of D-score
#' 
#' A data frame containing the age-dependent distribution of the 
#' D-score for children aged 0-5 years. The distribution is modelled
#' by a normal distribution, and is equal for
#' both boys and girls. The LMS values can be used to graph 
#' reference charts and to calculate age-conditonal Z-scores, also 
#' known as \code{DAZ}.
#' 
#' The references were calculated from 15 cohorts of the GCDG-study.
#' 
#' @docType data
#' @format A \code{data.frame} with 121 rows and 19 variables:
#' \describe{
#' \item{day}{Age in days}
#' \item{week}{Age in weeks}
#' \item{year}{Decimal age in years}
#' \item{mu}{M-curve, median D-score, P50}
#' \item{sigma}{S-curve, spread expressed as coefficient of variation}
#' \item{nu}{L-curve, the lambda coefficient of the LMS model for skewness}
#' \item{P3}{Calculated third percentile}
#' \item{\dots}{And so on..}
#' }
#' @examples 
#' head(gcdg_reference)
#' 
#' @references
#' Cole TJ, Green PJ (1992). Smoothing reference centile curves: The LMS 
#' method and penalized likelihood. Statistics in Medicine, 11(10), 
#' 1305-1319.
#' 
#' Van Buuren S (2014). Growth charts of human development.
#' Stat Methods Med Res, 23(4), 346-368.
#' @seealso \code{\link{dscore}}
"gcdg_reference"


#' Expanded version of itembank including Mullen
#' 
#' @docType data
"gcdg_itembank_m"
