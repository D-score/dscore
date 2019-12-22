#' Global Scale for Early Development - itemtable
#'
#' The built-in variable named `itemtable`
#' contains descriptions of all items found in the `gsed`
#' data.
#' @docType data
#' @format A `data.frame` with five columns:
#' \describe{
#' \item{`item`}{Item name, in GSED lexicon}
#' \item{`instrument`}{Instrument name (3-letter code)}
#' \item{`domain`}{Developmental domain (2-letter code)}
#' \item{`mode`}{Mode of administration (1-letter code)}
#' \item{`number`}{Item number (3-number code)}
#' \item{`equate`}{Equate group}
#' \item{`label`}{Item label}
#' }
#' @details
#' Data are collected by the members of the Global Scale for Early
#' Development (GSED) group.
#' The `itemtable` is created by `\\data-raw\\R\\import_itemtable.R`.
#' @author Compiled by Stef van Buuren
#' @keywords datasets
"builtin_itemtable"
