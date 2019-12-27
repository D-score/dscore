#' Get a subset of items from the itemtable
#'
#' The `builtin_itemtable` object in the `dscore` package
#' contains basic meta-information about items: a name, the equate group,
#' the instrument, the domain and the item label.
#' The `get_itemtable()` function returns a subset of `items`
#' in the itemtable.
#' @inheritParams get_itemnames
#' @return A `data.frame` with seven columns.
#' @param items A logical or character vector of item names to return. The
#' default (`NULL`) returns all items.
#' @seealso [get_labels()], [get_itemnames()]
#' @export
get_itemtable <- function(items = NULL, itemtable = NULL) {

  # construct variable names
  if (is.null(itemtable)) itemtable <- dscore::builtin_itemtable
  if (is.null(items)) {
    return(itemtable)
  }
  itemtable[itemtable$item %in% items, ]
}
