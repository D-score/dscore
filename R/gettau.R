#' Obtain difficulty parameters from item bank
#' 
#' Searches the item bank for matching items, and returns the 
#' difficulty estimates. Matching is done by item name. Comparisons
#' are done in lower case.
#' @aliases gettau
#' @param items A character vector with item names in the 
#' chosen lexicon.
#' @param itembank A \code{data.frame} that contains the item names 
#' (in various lexicons), the item label, the item difficulty parameter
#' \code{tau}. Lexicon column names start with \code{"lex."}. 
#' The default \code{dscore::itembank} contains the 
#' difficulty levels as published in Van Buuren (2014).
#' @param lexicon A character string indicating the column in 
#' \code{itembank} used to match item names. It must be one of 
#' the lexicon columns, e.g., \code{dutch1983}, 
#' \code{dutch1996}, \code{dutch2005}, \code{smocc} 
#' \code{ghap} or \code{gcdg}. The default is \code{lexicon = "ghap"}. 
#' @param check Logical that indicates whether the lexicon name
#' should be checked. The default is \code{TRUE}.
#' @param \dots Additional arguments (ignored).
#' @return A named vector with the difficulty estimate per item with
#' \code{length(items)} elements, or \code{NULL} if items are 
#' not found.
#' @author Stef van Buuren 2016
#' @seealso \code{\link{itembank}}, \code{\link{dscore}}
#' @examples 
#' # difficulty levels in default GHAP lexicon
#' gettau(items = c("GSFIXEYE", "GSMARMR"))
#' 
#' # difficulty levels of same items in the SMOCC lexicon
#' gettau(items = c("v1430", "v1432"), lexicon = "smocc")
#' 
#' @export
gettau <- function(items, 
                   itembank = dscore::itembank, 
                   lexicon = "ghap", 
                   check = TRUE, 
                   ...) {
  # check whether lexicon is a column in item bank
  lex <- paste("lex", lexicon, sep = "_")
  if (check) {
    q <- pmatch(tolower(lex), tolower(names(itembank)))
    if (is.na(q)) stop ("Lexicon `", lexicon, "` not found in item bank.")
  }
  
  # find exact matching items rows
  p <- match(items, itembank[, lex])
  if (all(is.na(p))) return(NULL)
  r <- itembank[p, "tau"]
  names(r) <- items
  return(r)
}
