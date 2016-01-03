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
#' @source \url{https://www.tno.nl/nl/aandachtsgebieden/gezond-leven/prevention-work-health/voor-tijdens-en-na-de-zwangerschap/pops-langlopend-onderzoek-onder-prematuren-en-baby-s-met-laag-geboortegewicht/}
#' 
"pops"


#' Itembank containing difficulties per item
#' 
#' A data frame with administrative information per item: 
#' item name (in various systems), item labels and difficulty. 
#' 
#' @docType data
#' @format A \code{data.frame} with 84 rows and 11 variables:
#' \describe{
#' \item{ID.VWO1996}{Item name, VWO1996 version}
#' \item{ID.VWO2005}{Item name, VWO2005 version}
#' \item{ID.VWO1983}{Item name, VWO1983 version}
#' \item{ID.smock}{Item name, SMOCC data}
#' \item{occ}{Occasion number}
#' \item{labelNL}{Label (Dutch)}
#' \item{labelEN}{Label (English)}
#' \item{tau}{Difficulty level}
#' \item{m}{ }
#' \item{LR}{Left-right item scores}
#' \item{hot}{Item used to estimate D-score? (1=y, 0=no)}
#' }
#' 
#' @references
#' Van Buuren S (2014). Growth charts of human development.
#' Stat Methods Med Res, 23(4), 346-368.
#' 
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
#' 
#' @references
#' Cole TJ, Green PJ (1992). Smoothing reference centile curves: The LMS 
#' method and penalized likelihood. Statistics in Medicine, 11(10), 
#' 1305-1319.
#' 
#' Van Buuren S (2014). Growth charts of human development.
#' Stat Methods Med Res, 23(4), 346-368.
#' 
"Dreference"


