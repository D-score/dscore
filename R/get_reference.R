#' Get D-score reference
#' 
#' The \code{get_reference()} function selects the D-score reference
#' distribution.
#'  
#' @param population A string describing the population. Currently supported
#' are \code{"dutch"} and \code{"gcdg"} (default).
#' @param references A \code{data.frame} with the same structure 
#' as \code{builtin_references}. The default is to use
#' \code{builtin_references}.
#' @return A \code{data.frame} with the LMS reference values. 
#' @note No references for population \code{"gsed"} exist. 
#' The function will silently rewrite \code{population = "gsed"} 
#' into to the \code{population = "gsed"}. 
#' 
#' The \code{"dutch"} reference was published in Van Buuren (2014)
#' 
#' The \code{"gcdg"} was calculated from 15 cohorts with direct 
#' observations (Weber, 2019).
#' @references
#' Van Buuren S (2014). Growth charts of human development.
#' Stat Methods Med Res, 23(4), 346-368.
#' 
#' Weber AM, Rubio-Codina M, Walker SP, van Buuren S, Eekhout I, 
#' Grantham-McGregor S, Caridad Araujo M, Chang SM, Fernald LCH, 
#' Hamadani JD, Hanlon A, Karam SM, Lozoff B, Ratsifandrihamanana L, 
#' Richter L, Black MM (2019). The D-score: a metric for interpreting 
#' the early development of infants and toddlers across global settings. 
#' BMJ Global Health, accepted for publication.
#' @seealso \code{\link{builtin_references}}
#' @export
get_reference <- function(population = "gcdg", 
                          references = dscore::builtin_references) {
  if (population == "gsed") population <- "gcdg"
  references[references$pop == population, ]
}