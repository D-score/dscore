#' Get labels for items
#'
#' The \code{get_labels()} function obtains the item labels for a
#' specified set of items.
#' @inheritParams get_itemnames
#' @param items A character vector of item names to return. The
#' default (\code{NULL}) returns the labels of all items.
#' @param trim The maximum number of characters in the label. The
#' default \code{trim = NULL} does not trim labels.
#' @return A named character vector with item labels.
#' @seealso \code{\link{builtin_itemtable}}, \code{\link{get_itemnames}}
#' @examples
#' # get labels of first two Macarthur items
#' get_labels(get_itemnames(instrument = "mac", number = 1:2), trim = 40)
#' @export
get_labels <- function(items = NULL, trim = NULL, itemtable = NULL) {

  # construct variable names
  if (is.null(items)) items <- get_itemnames(itemtable = itemtable)

  # obtain label
  label <- get_itemtable(items = items, itemtable = itemtable)$label
  if (!is.null(trim)) label <- substr(label, 1L, trim)
  names(label) <- get_itemtable(items = items, itemtable = itemtable)$item
  label
}
