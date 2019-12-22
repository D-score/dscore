#' Global Scale for Early Development - itemtable
#'
#' The built-in variable named `itemtable`
#' contains descriptions of all items found in the `gsed`
#' data.
#' @docType data
#' @format A `data.frame` with variables:
#'
#' | Name | Label |
#' | --- | --------- |
#' | `item` | Item name, gsed lexicon |
#' | `instrument` | Instrument code, 3 letters |
#' | `domain` | Domain code, 2 letter |
#' | `mode` | Mode code, 1 letter |
#' | `number` | Item number, 3 numbers |
#' | `equate` | Equate group |
#' | `label` | Label (English) |
#'
#' @details
#' Data are collected by the members of the Global Scale for Early
#' Development (GSED) group.
#' The `itemtable` is created by `\\data-raw\\R\\import_itemtable.R`.
#' @author Compiled by Stef van Buuren
#' @keywords datasets
"builtin_itemtable"
