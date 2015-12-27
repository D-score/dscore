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
#' @source \url{http://www.stefvanbuuren.nl/publications/2014%20Growth%20charts%20for%20development%20-%20SMMR.pdf}
#' 
"itembank"

