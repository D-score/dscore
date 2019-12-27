#' Outcomes on developmental milestones for preterm-born children
#'
#' A demo dataset with developmental scores at the item level for
#' a set of 27 preterm children.
#'
#' @docType data
#' @format A `data.frame` with 100 rows and 62 variables:
#'
#' Name       | Label
#' ---------- | ---------
#' `id`       | Integer, child ID
#' `agedays`  | Integer, age in days
#' `age`      | Numeric, decimal age in years
#' `sex`      | Character, "male", "female"
#' `gagebrth` | Integer, gestational age in days
#' `ddifmd001`| Integer, Fixates eyes: 1 = yes, 0 = no
#' `...`      | and so on..
#'
#' @examples
#' head(milestones)
#' @seealso [dscore()]
"milestones"
