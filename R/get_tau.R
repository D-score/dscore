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
#' @seealso \code{\link{builtin_itembank}}, \code{\link{dscore}}
#' @examples 
#' # difficulty levels in the GHAP lexicon
#' get_tau(items = c("ddifmd001", "DDigmd052", "xyz"))
#' 
#' @export
get_tau <- function(items, 
                   key = "gsed", 
                   itembank = dscore::builtin_itembank) {
  
  # no itembank, no items, no tau
  ibname <- deparse(substitute(itembank))
  ibname <- unlist(strsplit(ibname, "::"))
  ibname <- ibname[length(ibname)]
  if (!exists(ibname)) return(numeric(0))
  
  # no lexixon, no items
  mib <- itembank[itembank$key == key, c("key", "item", "tau")]

  # find exact matching items rows
  p <- match(tolower(items), tolower(mib$item))
  r <- mib[p, "tau"]
  names(r) <- items
  return(r)
}
