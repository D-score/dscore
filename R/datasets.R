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


#' Expanded version of itembank including Mullen
#' 
#' @docType data
"gcdg_itembank_m"
