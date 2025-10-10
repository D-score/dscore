#' Available keys for calculating the D-score
#'
#' A key contains the item difficulty estimates from a given Rasch model.
#' The difficulty estimates (`tau`) within a given key are used to
#' calculate D-scores. D-scores can only be compared when calculated
#' from the same key.
#'
#' @docType data
#' @format `builtin_keys` is a `data.frame` with variables:
#'
#' | Name | Label |
#' | --- | --------- |
#' | `key` | String. Name of the key indicating the Rasch model |
#' | `base_population` | String. Name of the base population for the key |
#' | `n_items` | Number of items in the key |
#' | `n_instruments`  | Number of instruments in the key |
#' | `intercept` | Intercept to convert logit into D-score |
#' | `slope` | Slope to convert logit into D-score |
#' | `from` | Starting value of the quadrature points |
#' | `to` | Stopping value of the quadrature points |
#' | `by` | Increment of the quadrature points |
#' | `retired` | Has the key been retired? |
#' @note
#' Updated: 20251010 SvB: Added `builtin_keys` table by
#' `data-raw\data\R\save_builtin_keys.R`
"builtin_keys"
