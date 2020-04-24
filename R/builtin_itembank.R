#' Built-in itembank
#'
#' A data frame with administrative information per item. Includes
#' only items that are part of a Rasch model.
#' See [dscore::builtin_itemtable] for an overview of all currently
#' defined items.
#'
#' @docType data
#' @format A `data.frame` with variables:
#'
#' | Name | Label |
#' | --- | --------- |
#' | `key` | String indicating a specific Rasch model, either `"gsed"`, `"gcdg"` or `"dutch"`|
#' | `item` | Item name, gsed lexicon |
#' | `tau`  | Difficulty estimate |
#' | `label` | Label (English) |
#' | `instrument` | Instrument code |
#' | `domain`     | Domain code |
#' | `mode`       | Administration mode |
#' | `number`     | Item number |
#'
#' @examples
#' head(builtin_itembank)
#' @seealso [dscore()], [get_tau()],
#' [builtin_itemtable()]
"builtin_itembank"
