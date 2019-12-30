#' dscore: D-score for Child Development
#'
#' The `dscore` package implements several tools needed to
#' calculate the D-score, a numerical score that measures
#' generic development in children.
#'
#' @section User functions:
#' The available functions are:
#'
#'   | Function | Description |
#'   | -------- | ----------- |
#'   | [get_itemnames()]   | Extract item names from an itemtable |
#'   | [order_itemnames()] | Order item names |
#'   | [sort_itemnames()]  | Sort item names |
#'   | [decompose_itemnames()] | Get four components from itemname |
#'   &nbsp;| | |
#'   | [get_itemtable()]   | Get a subset from the itemtable |
#'   | [get_labels()]      | Get labels for items |
#'   | [rename_gcdg_gsed()]| Rename gcdg into gsed lexicon |
#'   &nbsp;| | |
#'   | [dscore()]  | Estimate D-score and DAZ |
#'   | [dscore_posterior()]  | Calculate full posterior of D-score |
#'   | [get_tau()] | Get difficulty parameters from item bank |
#'   &nbsp;| | |
#'   | [daz()] | Transform to age-adjusted standardized D-score |
#'   | [zad()] | Inverse of [daz()] |
#'   | [get_reference()]   | Get D-score age-reference |
#'   | [get_age_equivalent()] | Translate difficulty to age |
#'
#' @section Built-in data:
#' The package contains the following built-in data:
#'
#'   Data     | Description
#'   -------- | ---------
#' [builtin_itembank()] | A `data.frame` containing the difficulty estimates of items according to final Rasch models.
#' [builtin_itemtable()] | A `data.frame` containing names and descriptions of items from 22 instruments.
#' [builtin_references()] | A `data.frame` with LMS reference values used to transform from D-score to DAZ, DAZ to D-score.
#' [milestones()] | A small demo dataset with PASS/FAIL responses from 27 preterms, measured at various ages between birth
#' and 2.5 years.
#'
#' @references
#' Jacobusse, G., S. van Buuren, and P.H. Verkerk. 2006. “An Interval Scale
#' for Development of Children Aged 0-2 Years.” *Statistics in Medicine* 25 (13):
#' 2272–83. [pdf](https://stefvanbuuren.name/publications/Interval\%20scale\%20-\%20Stat\%20Med\%202006.pdf)
#'
#' Van Buuren S (2014). Growth charts of human development.
#' Stat Methods Med Res, 23(4), 346-368.
#' [pdf](https://stefvanbuuren.name/publications/2014\%20Growth\%20charts\%20for\%20development\%20-\%20SMMR.pdf)
#'
#' Weber AM, Rubio-Codina M, Walker SP, van Buuren S, Eekhout I,
#' Grantham-McGregor S, Caridad Araujo M, Chang SM, Fernald LCH,
#' Hamadani JD, Hanlon A, Karam SM, Lozoff B, Ratsifandrihamanana L,
#' Richter L, Black MM (2019). The D-score: a metric for interpreting
#' the early development of infants and toddlers across global settings.
#' BMJ Global Health, BMJ Global Health 4: e001724.
#' [pdf](https://gh.bmj.com/content/bmjgh/4/6/e001724.full.pdf).
#'
#' GSED team (Maureen Black, Kieran Bromley, Vanessa Cavallera
#' (lead author), Jorge Cuartas, Tarun Dua (corresponding author),
#' Iris Eekhout, Gunther Fink, Melissa Gladstone, Katelyn Hepworth,
#'  Magdalena Janus, Patricia Kariger, Gillian Lancaster, Dana McCoy,
#'  Gareth McCray, Abbie Raikes, Marta Rubio-Codina, Stef van Buuren,
#'  Marcus Waldman, Susan Walker and Ann Weber). 2019. “The Global Scale
#'  for Early Development (GSED).” *Early Childhood Matters*.
#'  [link](https://earlychildhoodmatters.online/2019/the-global-scale-for-early-development-gsed/)
#'
#' @note
#' This study was supported by the Bill & Melinda Gates Foundation.
#' The contents are the sole responsibility of the authors and may not
#' necessarily represent the official views of the Bill & Melinda
#' Gates Foundation or other agencies that may have supported the
#' primary data studies used in the present study.
#'
#' The authors wish to
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
