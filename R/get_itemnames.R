#' Extract item names
#'
#' The `get_itemnames()` function matches names against the 9-code
#' template. This is useful for quickly selecting names of items from a larger
#' set of names.
#' @param x A character vector, `data.frame` or an object of
#' class `lean`. If not specified,
#' the function will return all item names in `itemtable`.
#' @param instrument A character vector with 3-position codes of instruments
#' that should match. The default `instrument = NULL` allows for
#' all instruments.
#' @param domain A character vector with 2-position codes of domains
#' that should match. The default `instrument = NULL` allows for
#' all domains.
#' @param mode A character vector with 1-position codes of the mode
#' of administration. The default `mode = NULL` allows for all
#' modes.
#' @param number A numeric or character vector with item numbers.
#' The default `number = NULL` allows for all numbers.
#' @param strict A logical specifying whether the resulting item
#' names must conform to one of the built-in names. The default is
#' `strict = FALSE`.
#' @param itemtable A `data.frame` set up according to the
#' same structure as [builtin_itemtable()]. If not specified,
#' the `builtin_itemtable` is used.
#' @return A vector with names of items
#' @details
#' The gsed-naming convention is as follows. Position 1-3 codes the
#' instrument, position 4-5 codes the domain, position 6 codes
#' direct/caregiver/message, positions 7-9 is a item sequence number.
#'
#' @seealso [sort_itemnames()]
#' @author Stef van Buuren 2020
#' @examples
#' itemnames <- c("aqigmc028", "grihsd219", "", "age", "mdsgmd999")
#'
#' # filter out impossible names
#' get_itemnames(itemnames)
#' get_itemnames(itemnames, strict = TRUE)
#'
#' # only items from specific instruments
#' get_itemnames(itemnames, instrument = c("aqi", "mds"))
#' get_itemnames(itemnames, instrument = c("aqi", "mds"), strict = TRUE)
#'
#' # get all items from the se domain of iyo instrument
#' get_itemnames(domain = "se", instrument = "iyo")
#'
#' # get all item from the se domain with direct assessment mode
#' get_itemnames(domain = "se", mode = "d")
#'
#' # get all item numbers 70 and 73 from gm domain
#' get_itemnames(number = c(70, 73), domain = "gm")
#' @export
get_itemnames <- function(x, instrument = NULL, domain = NULL,
                          mode = NULL, number = NULL, strict = FALSE,
                          itemtable = NULL) {
  if (is.null(itemtable)) {
    builtin <- dscore::builtin_itemtable$item
  } else {
    builtin <- itemtable$item
  }
  if (missing(x)) x <- builtin
  if (is.data.frame(x)) x <- names(x)
  if (inherits(x, "lean")) x <- unique(x[["itm"]]$item)
  if (strict) {
    z <- builtin[builtin %in% x]
  } else {
    z <- x[grep("^......\\d\\d\\d$", x)]
  }
  if (!is.null(instrument)) z <- z[substr(z, 1, 3) %in% instrument]
  if (!is.null(domain)) z <- z[substr(z, 4, 5) %in% domain]
  if (!is.null(mode)) z <- z[substr(z, 6, 6) %in% mode]
  if (!is.null(number)) z <- z[as.numeric(substr(z, 7, 9)) %in% number]
  z
}
