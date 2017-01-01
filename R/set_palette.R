#' Obtains color palettes for the Jamaica data
#' 
#' This function returns one of built-in color palettes from a 
#' list of palettes. 
#'@param palet The name of the list component in the built-in 
#'  color palette list \code{jamaica_palettes}. Currently implemented
#'  are \code{"study"}, \code{"country"}, \code{"domain"}, 
#'  \code{"instrument"}, \code{"wave"} and \code{"item"}.
#'@param which_palettes A named list containing palettes. The default is the
#'  built-in object \code{jamaica_palettes}.
#'@param na_color The color that is return if not found. The default is 
#'  \code{"grey"}.
#'@return The selected color palette. If not found, it returns the single 
#'  color specified by \code{na_color}.
#'@examples 
#'  get_palette("domain")
#'@export
get_palette <- function(palet = c("study", "country", "domain",
                                  "instrument", "wave", "item"),
                        which_palettes = "jamaica_palettes",
                        na_color = "grey") {
  p <- get0(which_palettes, mode = "list")
  if (is.null(p)) return(rgb(t(col2rgb(na_color)), maxColorValue = 255))
  return(p[[palet]])
}
