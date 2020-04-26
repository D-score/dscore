#' Get a subset of items from the itemtable
#'
#' The `builtin_itemtable` object in the `dscore` package
#' contains basic meta-information about items: a name, the equate group,
#' and the item label.
#' The `get_itemtable()` function returns a subset of `items`
#' in the itemtable.
#' @return A `data.frame` with seven columns.
#' @param items A logical or character vector of item names to return. The
#' default (`NULL`) returns all items.
#' @param itemtable A `data.frame` set up according to the
#' same structure as [builtin_itemtable()]. If not specified,
#' the `builtin_itemtable` is used. If `itemtable = ""`, then
#' a dynamic item table is created from any specified item names.
#' @param decompose If \code{TRUE}, the function adds four columns:
#' `instrument`, `domain`, `mode` and `number`.
#' @seealso [get_labels()], [get_itemnames()]
#' @examples
#' head(get_itemtable(), 3)
#' get_itemtable(LETTERS[1:3], "")
#' @export
get_itemtable <- function(items = NULL, itemtable = NULL,
                          decompose = FALSE) {

  if (is.null(itemtable)) itemtable <- dscore::builtin_itemtable

  # itemtable == "" is a special case for creating new items
  else if (is.character(itemtable)) {
    if (itemtable == "")
      if (length(items))
        itemtable <- data.frame(item = items,
                                equate = NA_character_,
                                label = paste("Label for", items),
                                stringsAsFactors = FALSE)
      else
        itemtable <- data.frame(item = "",
                                equate = NA_character_,
                                label = paste("Label for", items),
                                stringsAsFactors = FALSE)
  }

  if (length(items))
    itemtable <- filter(itemtable, .data$item %in% items)
  itemtable <- select(itemtable, all_of(c("item", "equate", "label")))

  if (decompose)
    itemtable <- bind_cols(itemtable,
                           decompose_itemnames(itemtable$item))

  itemtable
}
