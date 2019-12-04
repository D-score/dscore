#' Decomposes item names into their four components
#'
#' This utility function decomposes item names into components:
#' instrument, domain, mode and number
#' @param x A character vector containing item names (gcdg lexicon)
#' @return A \code{data.frame} with \code{length(x)} rows and
#' four columns, named: \code{instrument}, \code{domain}, \code{mode},
#' and \code{number}.
#' @details
#' The gsed-naming convention is as follows. Position 1-3 codes the
#' instrument, position 4-5 codes the domain, position 6 codes
#' direct/caregiver/message, positions 7-9 is a item sequence number.
#'
#' @seealso \code{\link{sort_itemnames}}
#' @references
#' \url{https://docs.google.com/spreadsheets/d/1zLsSW9CzqshL8ubb7K5R9987jF4YGDVAW_NBw1hR2aQ/edit#gid=0}
#' @author Stef van Buuren
#' @examples
#' itemnames <- c("aqigmc028", "grihsd219", "", "by1mdd157", "mdsgmd006")
#' decompose_itemnames(itemnames)
#' @export
decompose_itemnames <- function(x) {
  instrument <- substr(x, 1, 3)
  domain <- substr(x, 4, 5)
  mode <- substr(x, 6, 6)
  number <- substr(x, 7, 9)
  data.frame(instrument, domain, mode, number,
             stringsAsFactors = FALSE)
}
