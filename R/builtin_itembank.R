#' Built-in itembank
#'
#' A data frame with administrative information per item. Includes
#' only items that are part of a Rasch model.
#' See [dscore::builtin_itemtable] for an overview of all currently
#' defined items.
#'
#' In general, one can only compare D-score calculated with the same
#' key. The current recommendation for new projects is to choose
#' key `gsed2212`.
#'
#' @docType data
#' @format A `data.frame` with variables:
#'
#' | Name | Label |
#' | --- | --------- |
#' | `key` | String indicating a specific Rasch model (the key) |
#' | `item` | Item name, gsed lexicon |
#' | `tau`  | Difficulty estimate |
#' | `label` | Label (English) |
#' | `instrument` | Instrument code |
#' | `domain`     | Domain code |
#' | `mode`       | Administration mode |
#' | `number`     | Item number |
#'
#' @note
#' Last update:
#'  - Dec 01, 2022 - Overwrite labels of gto by correct item order.
#'  - Dec 05, 2022 - Adds key `gsed2212`, adding instruments `gl1` and `gs1`, and
#'    defining correct order for `gto`
#'  - Jan 05, 2023 - Adds instrument `gh1` to key `gsed2212`
#'
#' @examples
#' head(builtin_itembank)
#' @seealso [dscore()], [get_tau()],
#' [builtin_itemtable()]
"builtin_itembank"
