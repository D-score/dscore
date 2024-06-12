#' Collection of items fitting the Rasch model
#'
#' A data frame with administrative information per item with difficulty
#' estimates (`tau`) from the Rasch model. The item bank provides the basic
#' information to calculate D-scores. The items in the item bank
#' are a subset of all items as collected in [dscore::builtin_itemtable].
#'
#' The difficulty estimates were estimated by a Rasch model. The `key`
#' indicates the specific Rasch model used to estimate the difficulty.
#' Strictly speaking, one can only compare D-score calculated from the
#' same `key`.
#'
#' @docType data
#' @format A `data.frame` with variables:
#'
#' | Name           | Label                                    |
#' | -------------- | ---------------------------------------- |
#' | `key`          | String indicating a specific Rasch model |
#' | `item`         | Item name, gsed lexicon                  |
#' | `tau`          | Difficulty estimate                      |
#' | `label`        | Label (English)                          |
#' | `instrument`   | Instrument code                          |
#' | `domain`       | Domain code                              |
#' | `mode`         | Administration mode                      |
#' | `number`       | Item number                              |
#'
#' @note
#' Updates:
#'
#' - Dec 01, 2022 - Overwrite labels of gto by correct item order.
#' - Dec 05, 2022 - Adds key `gsed2212`, adding instruments `gl1` and `gs1`, and
#'   defining correct order for `gto`
#' - Jan 05, 2023 - Adds instrument `gh1` to key `gsed2212`
#'
#' @examples
#' # count number of items per instrument in each key
#' table(builtin_itembank$instrument, builtin_itembank$key)
#' @seealso [dscore()], [get_tau()], [builtin_itemtable()]
"builtin_itembank"
