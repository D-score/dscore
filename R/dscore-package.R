#' dscore: D-score for Child Development
#' 
#' The \code{dscore} package implements several tools needed to 
#' calculate the D-score, a numerical score that measures
#' generic development in children.
#' The available functions are:
#' \tabular{ll}{
#'   \code{\link{dscore}}  \tab Estimate D-score and DAZ\cr
#'   \code{\link{daz}}     \tab Transform to age-adjusted standardized D-score\cr
#'   \code{\link{zad}}     \tab Inverse of \code{\link{daz}}\cr
#'   \code{\link{get_tau}}  \tab Get difficulty parameters from item bank\cr
#'   \code{\link{get_itemnames}} \tab Extract item names \cr
#'   \code{\link{get_itemtable}} \tab Get a subset from the itemtable\cr
#'   \code{\link{get_labels}}     \tab Get labels for items \cr
#'   \code{\link{order_itemnames}}\tab Sort item names  \cr
#'   \code{\link{sort_itemnames}}  \tab Sort item names  \cr
#'   }
#' 
#' @section Built-in data:
#' The package contains three sets of built-in data:
#' \describe{
#' \item{\code{\link{builtin_itembank}}}{A \code{data.frame} containing 
#' the difficulty estimates of items according to final Rasch models.}
#' \item{\code{\link{builtin_itemtable}}}{A \code{data.frame} containing 
#' names and descriptions of items from 22 instruments.}
#' \item{\code{\link{builtin_references}}}{A \code{data.frame} with LMS 
#' reference values used to transform from D-score to DAZ, 
#' DAZ to D-score, and to calculate reference charts of normal development.}
#' \item{\code{\link{milestones}}}{A small demo dataset with PASS/FAIL 
#' responses from 27 preterms, measured at various ages between birth 
#' and 2.5 years.}
#' }
#' 
#' @references
#' Van Buuren S (2014). Growth charts of human development.
#' Stat Methods Med Res, 23(4), 346-368.
#' 
#' @note 
#' Development of this package was kindly supported under the Healthy
#' Birth, Growth and Development knowledge integration (HBGDki)
#' program of the Bill & Melinda Gates Foundation.
#' 
#' @name dscore-package
#' @docType package
#' @aliases dscore-package
NULL

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

## usethis namespace: start
#' @useDynLib dscore
## usethis namespace: end
NULL

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL
