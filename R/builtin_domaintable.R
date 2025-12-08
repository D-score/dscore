#' Developmental domain for items
#'
#' The built-in variable `builtin_domaintable` contains the domain and weight
#' of items for measuring early child development.
#'
#' @docType data
#' @format A `data.frame` with variables:
#'
#' | Name     | Label                                                 |
#' | -------- | ----------------------------------------------------- |
#' | `set`    | Item name, gsed lexicon                               |
#' | `domain` | name of the developmental domain                      |
#' | `item`   | Item name, gsed lexicon                               |
#' | `weight` | Proportional weight in domain based on expert votes   |
#'
#' @details
#' The `builtin_domaintable` is created by script
#' `data-raw/R/save_builtin_domaintable.R`.
#'
#' Votings are collected by Gareth MacCray in surveys administered to several
#' subject matter experts. Each SME could attribute domains to an item, the
#' expert could distribute their vote to multiple domains if applicable. SME
#' votes were weighted to a proportional distribution of votes over the domains
#' for each item.
#'
#' @author Compiled by Iris Eekhout using different sources
#' @keywords datasets
"builtin_domaintable"
