#' @title D-score for child development
#'
#' @description
#' The `dscore` package implements tools needed to calculate the D-score,
#' a numerical score that summarizes early development in children by
#' one number, the D-score.
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
#'   | [get_reference()]   | Get D-score reference tables |
#'   | [get_age_equivalent()] | Translate difficulty to age |
#'
#' @section Built-in data:
#' The package contains the following built-in data:
#'
#'   Data     | Description
#'   -------- | ---------
#' [builtin_keys()] | Available keys for calculating the D-score
#' [builtin_itembank()] | Collection of items fitting the Rasch model
#' [builtin_itemtable()] | Collection of items from instruments measuring early child development
#' [builtin_references()] | Collection of age-conditional reference distributions
#'   &nbsp;| | |
#' [milestones()] | Dataset with PASS/FAIL responses for 27 preterms
#' [gsample] | Sample of 10 children from the GSED Phase 1 study, gsed lexicon
#' [sample_sf] | Sample of 10 children from GSED Short Form (GSED-SF)
#' [sample_lf] | Sample of 10 children from GSED Long Form (GSED-LF)
#' [sample_hf] | Sample of 10 children from GSED Household Form (GSED-HF)
#'
#' @references
#' Jacobusse, G., S. van Buuren, and P.H. Verkerk. 2006. “An Interval Scale
#' for Development of Children Aged 0-2 Years.” *Statistics in Medicine* 25 (13):
#' 2272–83. <https://stefvanbuuren.name/publication/jacobusse-2006/>
#'
#' Van Buuren S (2014). Growth charts of human development.
#' Stat Methods Med Res, 23(4), 346-368.
#' <https://stefvanbuuren.name/publication/van-buuren-2014-gc/>
#'
#' Weber AM, Rubio-Codina M, Walker SP, van Buuren S, Eekhout I,
#' Grantham-McGregor S, Caridad Araujo M, Chang SM, Fernald LCH,
#' Hamadani JD, Hanlon A, Karam SM, Lozoff B, Ratsifandrihamanana L,
#' Richter L, Black MM (2019). The D-score: a metric for interpreting
#' the early development of infants and toddlers across global settings.
#' BMJ Global Health, BMJ Global Health 4: e001724.
#' <https://gh.bmj.com/content/bmjgh/4/6/e001724.full.pdf>.
#'
#' GSED team (Maureen Black, Kieran Bromley, Vanessa Cavallera
#' (lead author), Jorge Cuartas, Tarun Dua (corresponding author),
#' Iris Eekhout, Gunther Fink, Melissa Gladstone, Katelyn Hepworth,
#'  Magdalena Janus, Patricia Kariger, Gillian Lancaster, Dana McCoy,
#'  Gareth McCray, Abbie Raikes, Marta Rubio-Codina, Stef van Buuren,
#'  Marcus Waldman, Susan Walker and Ann Weber). 2019. “The Global Scale
#'  for Early Development (GSED).” *Early Childhood Matters*.
#'  <https://earlychildhoodmatters.online/2019/the-global-scale-for-early-development-gsed/>
#'
#' @section Acknowledgements:
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
#' This study was supported by the Bill & Melinda Gates Foundation.
#' The contents are the sole responsibility of the authors and may not
#' necessarily represent the official views of the Bill & Melinda
#' Gates Foundation or other agencies that may have supported the
#' primary data studies used in the present study.
#'
#' @name dscore-package
#' @aliases dscore-package
#' @keywords internal
"_PACKAGE"

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
