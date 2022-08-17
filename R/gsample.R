#' Sample of 10 children from the GSED Phase 1 study
#'
#' A demo dataset with developmental scores at the item level for
#' 10 random children from the GSED Phase 1 data.
#'
#' @docType data
#' @format A `data.frame` with 10 rows and 295 variables:
#'
#' Name       | Label
#' ---------- | ---------
#' `id`       | Integer, child ID
#' `agedays`  | Integer, age in days
#' `gpalac001`| Integer, Cry when hungry...: 1 = yes, 0 = no, NA = not administered
#' `gpalac002`| Integer, Look at/focus...: 1 = yes, 0 = no, NA = not administered
#' `...`      | and so on..
#'
#' There are 138 `gpa` items (item `gpamoc008` (clench fists) removed) from GSED SF and
#' and 155 `gto` items from GSED LF.
#' @examples
#' head(gsample)
#' @seealso [dscore()]
"gsample"
