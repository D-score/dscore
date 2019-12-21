#' Get a subset of items from the itemtable
#'
#' The \code{builtin_itemtable} object in the \code{dscore} package
#' contains basic meta-information about items: a name, the equate group,
#' the instrument, the domain and the item label.
#' The \code{get_itemtable()} function returns a subset of \code{items}
#' in the itemtable.
#' @inheritParams get_itemnames
#' @return A \code{data.frame} with seven columns.
#' @param items A logical or character vector of item names to return. The
#' default (\code{NULL}) returns all items.
#' @seealso \code{\link{get_labels}}, \code{\link{get_itemnames}}
#' @export
get_itemtable <- function(items = NULL, itemtable = NULL) {

  # construct variable names
  if (is.null(itemtable)) itemtable <- dscore::builtin_itemtable
  if (is.null(items)) {
    return(itemtable)
  }
  itemtable[itemtable$item %in% items, ]
}
