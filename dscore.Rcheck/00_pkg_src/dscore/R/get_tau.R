#' Obtain difficulty parameters from item bank
#'
#' Searches the item bank for matching items, and returns the
#' difficulty estimates. Matching is done by item name. Comparisons
#' are done in lower case.
#'
#' @inheritParams dscore
#' @return A named vector with the difficulty estimate per item with
#' `length(items)` elements.
#' @author Stef van Buuren 2020
#' @seealso [builtin_itembank()], [dscore()]
#' @examples
#' # difficulty levels in the GHAP lexicon
#' get_tau(items = c("ddifmd001", "DDigmd052", "xyz"))
#' @export
get_tau <- function(items,
                    key = "gsed",
                    itembank = dscore::builtin_itembank) {
  # if key = "", then search in all rows
  if (key == "") {
    mib <- data.frame(
      key = "",
      itembank[, c("item", "tau")]
    )
  } else {
    mib <- itembank[itembank$key == key, c("key", "item", "tau")]
  }

  # find exact matching items rows
  p <- match(tolower(items), tolower(mib$item))
  r <- mib[p, "tau"]
  names(r) <- items
  return(r)
}
