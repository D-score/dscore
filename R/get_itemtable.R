#' Get a subset of items from the itemtable
#'
#' The `builtin_itemtable` object in the `dscore` package
#' contains basic meta-information about items: a name, the equate group,
#' the instrument, the domain and the item label.
#' The `get_itemtable()` function returns a subset of `items`
#' in the itemtable.
#' @return A `data.frame` with seven columns.
#' @param items A logical or character vector of item names to return. The
#' default (`NULL`) returns all items.
#' @param itemtable A `data.frame` set up according to the
#' same structure as [builtin_itemtable()]. If not specified,
#' the `builtin_itemtable` is used. If `itemtable = ""`, then
#' a dynamic item table is created from any specified item names.
#' @seealso [get_labels()], [get_itemnames()]
#' @examples
#' head(get_itemtable(), 3)
#' get_itemtable(LETTERS[1:3], "")
#' @export
get_itemtable <- function(items = NULL, itemtable = NULL) {

  if (is.null(itemtable)) itemtable <- dscore::builtin_itemtable
  else if (itemtable == "" && length(items))
    return(data.frame(item = items,
                      instrument = NA_character_,
                      domain = NA_character_,
                      mode = NA_character_,
                      number = NA_character_,
                      equate = NA_character_,
                      label = paste("Label for", items),
                      stringsAsFactors = FALSE))
  if (is.null(items)) return(itemtable)
  itemtable[itemtable$item %in% items, ]
}
