#' dscore: Measuring child development by the D-score
#' 
#' The \code{dscore} package implements several steps needed to 
#' calculate the D-score, a numerical score that measures
#' generic development in children 0-4 years.
#' 
#' The main functions are:
#' \tabular{ll}{
#'   \code{\link{dscore}} \tab Estimate D-scores of children\cr
#'   \code{\link{daz}} \tab Transform to age-adjusted standardized D-score\cr
#'   \code{\link{zad}} \tab Inverse of \code{\link{daz}}
#'   }
#'   
#' @section Built-in data:
#' The package contains three sets of built-in data:
#' \describe{
#' \item{\code{\link{builtin_itembank}}}{A data frame containing 
#' the difficulty estimates of the item according to the Rasch model.}
#' \item{\code{\link{builtin_itemtable}}}{A data frame containing 
#' names and descriptions of items from 23 instruments.}
#' \item{\code{\link{builtin_references}}}{A data frame with LMS 
#' reference values used to transform from D-score to DAZ, 
#' DAZ to D-score, and to calculate reference charts of normal development.}
#' \item{\code{\link{preterm27}}}{A small demo dataset with PASS/FAIL 
#' responses for 27 preterms at various ages.}
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
#' @docType package
#' @name dscore-package
NULL
