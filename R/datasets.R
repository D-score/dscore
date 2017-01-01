#' Project On Preterm and Small for gestational age infants (POPS)
#' 
#' A dataset with developmental scores at the item level from a subset
#' of 25 children from the POPS cohort. 
#' 
#' @docType data
#' @format A \code{tbl_df} with 100 rows (25 children times 4 time points) and 67 variables:
#' \describe{
#' \item{patid}{Patient ID}
#' \item{gender}{1=male, 1 = female}
#' \item{gestionalage}{Gestational age in fractional weeks}
#' \item{moment}{Sequence number}
#' \item{age}{Age in days}
#' \item{occ}{Measurement occasion (1-4)}
#' \item{daycor}{Corrected age in days}
#' \item{Dscore}{Calculated D-score}
#' \item{popsdscoresds}{Corrected age-conditional standard deviation score of D-score}
#' \item{dead}{0 = alive, 1 = dead}
#' \item{Fixateseyes}{Fixates eyes: 0 = yes, 1 = no}
#' \item{\dots}{and so on..}
#' }
#' 
#' @note 
#' Note that the PASS/FAIL items in the \code{pops} data are coded
#' in the `wrong` way: 0 = PASS, 1 = FAIL.
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
#' \item{lex.dutch1996}{Item naming, VWO1996 version}
#' \item{lex.dutch2005}{Item naming, VWO2005 version}
#' \item{lex.dutch1983}{Item naming, VWO1983 version}
#' \item{lex.SMOCC}{Item naming, original SMOCC variable names}
#' \item{lex.GHAP}{Item naming used by GHAP platform of BMGF}
#' \item{lex.jam}{Item naming for Jamaica project}
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
#' @seealso \code{\link{dscore}}, \code{\link{adp}}
"Dreference"

#' Color palettes for the Jamaica data
#' 
#' A list containing five color palettes for the Jamaica data.
#' 
#' @docType data
#' @format A named \code{list} with 5 elements.
"jamaica_palettes"
