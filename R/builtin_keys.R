#' D-score builtin_keys table
#'
#' A key represents a set of Rasch models that have the same item difficulty
#' estimates (tau). The key is used to calculate D-scores. D-scores can
#' only be compared when calculated with the same key. The current
#' recommendation for new projects is to choose key `gsed2212`.
#'
#' Some case are depecreated and should not be used, other than for
#' replication. The `deprecated` column indicates if a key is deprecated.
#'
#' @docType data
#' @format A `data.frame` with variables:
#'
#' | Name | Label |
#' | --- | --------- |
#' | `key` | String indicating a specific Rasch model (the key) |
#' | `n_items` | Number of items in the key |
#' | `n_instruments`  | Number of instruments in the key |
#' | `intercept` | Intercept to convert logit into D-score |
#' | `slope` | Slope to convert logit into D-score |
#' | `deprecated` | Logical. Is key deprecated? |
#' @note
#' 20240609 SvB: Added `builtin_keys` table by `data-raw\data\R\save_builtin_keys.R`
"builtin_keys"
