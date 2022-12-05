#' Global Scale for Early Development - itemtable
#'
#' The built-in variable named `builtin_itemtable`
#' contains descriptions of all items found in the `gsed`
#' data.
#' @docType data
#' @format A `data.frame` with variables:
#'
#' | Name | Label |
#' | --- | --------- |
#' | `item` | Item name, gsed lexicon |
#' | `equate` | Equate group |
#' | `label` | Label (English) |
#'
#' @details
#' Data are collected by the members of the Global Scales for Early
#' Development (GSED) group.
#' The `itemtable` is created by `\\data-raw\\R\\save_builtin_itemtable.R`.
#'
#' Last update:
#'  - May 30, 2022 - added gto (LF) and gpa (SF) items
#'  - June 1, 2022 - added seven gsd items
#'  - Nov 24, 2022 - Added instruments gs1, gs2
#'  - Dec 01, 2022 - Labels of gto replaced by correct order. This change invalidates
#'  any analyses done on LF done after May 30, 2022 !!!
#'  - Dec 05, 2022 - Redefines gs1 and instrument for Phase 2, removes gs2 (139)
#'                   Adds gl1 (Long Form Phase 2 items 155)
#' @author Compiled by Stef van Buuren
#' @keywords datasets
"builtin_itemtable"
