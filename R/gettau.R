#' Obtain difficulty parameters from item bank
#' 
#' Searches the item bank for matching items, and returns the 
#' difficulty estimates. Matching is done by item name. Comparisons
#' are done in lower case.
#' 
#' @inheritParams dscore
#' @return A named vector with the difficulty estimate per item with
#' \code{length(items)} elements.
#' @author Stef van Buuren 2019
#' @seealso \code{\link{itembank}}, \code{\link{dscore}}
#' @examples 
#' # difficulty levels in the GHAP lexicon
#' gettau(items = c("ddifmd001", "DDigmd052", "xyz"))
#' 
#' # difficulty levels in the GHAP lexicon
#' gettau(items = c("GSFIXEYE", "gsmarm", "GSMARMR"), lexicon = "ghap")
#' 
#' # difficulty levels of same items in the SMOCC lexicon
#' gettau(items = c("v1430", "V1432", "v1432", "IdontExist"), lexicon = "smocc")
#' 
#' @export
gettau <- function(items, 
                   lexicon = "gsed", 
                   itembank = dscore::itembank) {
  
  itembank_items <- lexicon(lexicon = lexicon, itembank = itembank, 
                            na.omit = FALSE)

  # find exact matching items rows
  p <- match(tolower(items), tolower(itembank_items))
  r <- itembank[p, "tau"]
  names(r) <- items
  return(r)
}
