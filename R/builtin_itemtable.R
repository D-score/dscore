#' Global Scale for Early Development - itemtable
#'
#' The built-in variable named \code{itemtable}
#' contains descriptions of all items found in the \code{gsed}
#' data.
#'@docType data
#'@format A \code{data.frame} with five columns:
#'\describe{
#'\item{\code{item}}{Item name, in GSED lexicon}
#'\item{\code{instrument}}{Instrument name (3-letter code)}
#'\item{\code{domain}}{Developmental domain (2-letter code)}
#'\item{\code{mode}}{Mode of administration (1-letter code)}
#'\item{\code{number}}{Item number (3-number code)}
#'\item{\code{equate}}{Equate group}
#'\item{\code{label}}{Item label}
#'}
#'@details
#'Data are collected by the members of the Global Scale for Early
#'Development (GSED) group.
#'The \code{itemtable} is created by \code{\\data-raw\\R\\import_itemtable.R}.
#'@author Compiled by Stef van Buuren
#'@keywords datasets
"builtin_itemtable"
