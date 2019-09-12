#' Find lexicon item names
#' 
#' The function returns the item names recognized under a specified
#' lexicon in a given itembank.
#' 
#' @inheritParams dscore
#' @param na.omit Logical signalling where \code{NA} should be omitted.
#' The default is \code{na.omit = TRUE}.
#' @return A character vector with all items recognized under the 
#' lexicon-itembank combination. May be of length zero if the 
#' lexicon or itembank is not found.
#' @seealso \code{\link{dscore}}
#' @examples 
#' lexicon("smocc")
#' @export
lexicon <- function(lexicon = "gsed", 
                    itembank = dscore::builtin_itembank,
                    na.omit = TRUE) {
  # no itembank, no items
  ibname <- deparse(substitute(itembank))
  ibname <- unlist(strsplit(ibname, "::"))
  ibname <- ibname[length(ibname)]
  if (!exists(ibname)) return(character(0))
  
  # no lexixon, no items
  lexname <- paste("lex", lexicon, sep = "_")
  q <- pmatch(tolower(lexname), tolower(names(itembank)))
  if (is.na(q)) return(character(0))
  
  items <- itembank[, lexname]
  if (na.omit) items <- items[!is.na(items)]
  as.character(items)
}

