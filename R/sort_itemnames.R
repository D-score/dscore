#' Sorts item names according to user-specified priority
#'
#' This function sorts the item names according to instrument,
#' domain, mode and number. The user can specify the sorting
#' order.
#' @param x A character vector containing item names (gcdg lexicon)
#' @param order A four-letter string specifying the sorting order.
#' The four letters are: \code{i} for instrument, \code{d} for domain,
#' \code{m} for mode and \code{n} for number. The default is
#' \code{"idnm"}.
#' @return \code{sort_itemnames()} return a character vector with
#' \code{length(x)} sorted elements. \code{order_itemnames()} return
#' an integer vector of length \code{length(x)} with positions of
#' the sorted elements.
#' @seealso \code{\link{decompose_itemnames}}
#' @author Stef van Buuren
#' @examples
#' itemnames <- c("aqigmc028", "grihsd219", "", "by1mdd157", "mdsgmd006")
#' decompose_itemnames(itemnames)
#' @export
sort_itemnames <- function(x, order = "idnm") {
  x[order_itemnames(x, order)]
}

#' @rdname sort_itemnames
#' @export
order_itemnames <- function(x, order = "idnm") {
  din <- decompose_itemnames(x)
  order(din[, pmatch(substr(order, 1, 1), names(din))],
        din[, pmatch(substr(order, 2, 2), names(din))],
        din[, pmatch(substr(order, 3, 3), names(din))],
        din[, pmatch(substr(order, 4, 4), names(din))])

}
