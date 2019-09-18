#' Data on developmental milestones for preterm-born children
#' 
#' A demo dataset with developmental scores at the item level for 
#' a set of 27 preterm children.
#' 
#' @docType data
#' @format A \code{data.frame} with 100 rows and 61 variables:
#' \describe{
#' \item{id}{Integer, patient ID}
#' \item{agedays}{Integer, age in days}
#' \item{age}{Numeric, decimal age in years}
#' \item{sex}{Character, "male", "female"}
#' \item{gagebrth}{Integer, gestational age in days}
#' \item{ddifmd001}{Integer, Fixates eyes: 1 = yes, 0 = no}
#' \item{\dots}{and so on..}
#' }
#' 
#' @examples 
#' head(preterm27)
#' 
#' @seealso \code{\link{dscore}}
"preterm27"