#' Collection of items from instruments measuring early child development
#'
#' The built-in variable `builtin_itemtable` contains the name and label
#' of items for measuring early child development.
#'
#' @docType data
#' @format A `data.frame` with variables:
#'
#' | Name     | Label                     |
#' | -------- | ------------------------- |
#' | `item`   | Item name, gsed lexicon   |
#' | `equate` | Equate group              |
#' | `label`  | Label (English)           |
#'
#' @details
#' The `builtin_itemtable` is created by script
#' `data-raw/R/save_builtin_itemtable.R`.
#'
#' Updates:
#'  - May 30, 2022 - added gto (LF) and gpa (SF) items
#'  - June 1, 2022 - added seven gsd items
#'  - Nov 24, 2022 - Added instruments gs1, gs2
#'  - Dec 01, 2022 - Labels of gto replaced by correct order.
#'  Incorrect item order affects analyses done on LF between 20220530 - 20221201 !!!
#'  - Dec 05, 2022 - Redefines gs1 and instrument for Phase 2, removes gs2 (139)
#'                   Adds gl1 (Long Form Phase 2 items 155)
#'  - Jan 05, 2023 - Adds 55 items from GSED-HF
#'  - Jul 15, 2025 - Rename gpaclc088 --> gpaclc089 (Can you child say five or more separate words)
#'                   Rename gpasec089 --> gpasec088 (Is your child able to pee and poo)
#' @author Compiled by Stef van Buuren using different sources
#' @keywords datasets
"builtin_itemtable"
