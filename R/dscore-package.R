#' dscore: D-score for Child Development
#'
#' The `dscore` package implements several tools needed to
#' calculate the D-score, a numerical score that measures
#' generic development in children.
#'
#' @section User functions:
#' The available functions are:
#' \tabular{ll}{
#'   [dscore()]  \tab Estimate D-score and DAZ\cr
#'   [dscore_posterior()]  \tab Calculate full posterior\cr
#'   [daz()] \tab Transform to age-adjusted standardized D-score\cr
#'   [zad()] \tab Inverse of [daz()]\cr
#'   [get_tau()] \tab Get difficulty parameters from item bank\cr
#'   [get_reference()]   \tab Get D-score reference\cr
#'   [get_itemnames()]   \tab Extract item names \cr
#'   [get_itemtable()]   \tab Get a subset from the itemtable\cr
#'   [get_labels()]      \tab Get labels for items \cr
#'   [order_itemnames()] \tab Sort item names  \cr
#'   [sort_itemnames()]  \tab Sort item names  \cr
#'   [rename_gcdg_gsed()]\tab Rename gcdg lexicon to gsed lexicon\cr
#'   }
#'
#' @section Built-in data:
#' The package contains the following built-in data:
#' \tabular{ll}{
#' [builtin_itembank()] \tab A `data.frame` containing
#' the difficulty estimates of items according to final Rasch models.\cr
#' [builtin_itemtable()] \tab A `data.frame` containing
#' names and descriptions of items from 22 instruments.\cr
#' [builtin_references()] \tab A `data.frame` with LMS
#' reference values used to transform from D-score to DAZ,
#' DAZ to D-score, and to calculate reference charts of normal development.\cr
#' [milestones()] \tab A small demo dataset with PASS/FAIL
#' responses from 27 preterms, measured at various ages between birth
#' and 2.5 years.\cr
#' }
#'
#' @references
#' Van Buuren S (2014). Growth charts of human development.
#' Stat Methods Med Res, 23(4), 346-368.
#'
#' @note
#' This study was supported by the Bill & Melinda Gates Foundation.
#' The contents are the sole responsibility of the authors and may not
#' necessarily represent the official views of the Bill & Melinda
#' Gates Foundation or other agencies that may have supported the
#' primary data studies used in the present study. The authors wish to
#' recognize the principal investigators and their study team members
#' for their generous contribution of the data that made this tool
#' possible and the members of the Ki team who directly or indirectly
#' contributed to the study: Amina Abubakar, Claudia R. Lindgren
#' Alves, Orazio Attanasio, Maureen M. Black, Maria Caridad Araujo,
#' Susan M. Chang-Lopez, Gary L. Darmstadt, Bernice M. Doove, Wafaie
#' Fawzi, Lia C.H. Fernald, Günther Fink, Emanuela Galasso, Melissa
#' Gladstone, Sally M. Grantham-McGregor, Cristina Gutierrez de
#' Pineres, Pamela Jervis, Jena Derakhshani Hamadani, Charlotte
#' Hanlon, Simone M. Karam, Gillian Lancaster, Betzy Lozoff, Gareth
#' McCray, Jeffrey R Measelle, Girmay Medhin, Ana M. B. Menezes,
#' Lauren Pisani, Helen Pitchik, Muneera Rasheed, Lisy
#' Ratsifandrihamanana, Sarah Reynolds, Linda Richter, Marta
#' Rubio-Codina, Norbert Schady, Limbika Sengani, Chris Sudfeld,
#' Marcus Waldman, Susan P. Walker, Ann M. Weber and Aisha K.
#' Yousafzai.
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
