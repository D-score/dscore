#' Built-in itembank
#' 
#' A data frame with administrative information per item. Includes 
#' only items that are part of a Rasch model.
#' See \code{\link{builtin_itemtable}} for an overview of all currently 
#' defined items.
#' 
#' @docType data
#' @format A \code{data.frame} with variables:
#' \describe{
#' \item{key}{String indicating a specific Rasch model, either \code{"gsed"}, \code{"gcdg"} or \code{"dutch"}}
#' \item{item}{Item name, gsed lexicon}
#' \item{tau}{Difficulty estimate}
#' \item{instrument}{Instrument code, 3 letters}
#' \item{domain}{Domain code, 2 letter}
#' \item{mode}{Mode code, 1 letter}
#' \item{number}{Item number}
#' \item{label}{Label (English)}
#' }
#' 
#' @examples 
#' head(builtin_itembank)
#' 
#' @seealso \code{\link{dscore}}, \code{\link{get_tau}}, \code{\link{builtin_itemtable}}
"builtin_itembank"
