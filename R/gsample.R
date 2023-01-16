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

#' Sample of 10 children from gpa (SF)
#'
#' A demo dataset with developmental scores at the item level for
#' 10 random children from the GSED Phase 1 data.
#'
#' @docType data
#' @format A `data.frame` with 10 rows and 141 variables:
#'
#' Name       | Label
#' ---------- | ---------
#' `subjid`   | Integer, child ID
#' `agedays`  | Integer, age in days
#' `sf001`    | Integer, Cry when hungry...: 1 = yes, 0 = no, NA = not administered
#' `sf002`    | Integer, Look at/focus...: 1 = yes, 0 = no, NA = not administered
#' `...`      | and so on..
#'
#' Sample data for 139 `gpa` items from GSED SF
#' @examples
#' head(sample_sf)
#' @seealso [dscore()]
"sample_sf"

#' Sample of 10 children from gto (LF)
#'
#' A demo dataset with developmental scores at the item level for
#' 10 random children from the GSED Phase 1 data.
#'
#' @docType data
#' @format A `data.frame` with 10 rows and 157 variables:
#'
#' Name       | Label
#' ---------- | ---------
#' `subjid`   | Integer, child ID
#' `agedays`  | Integer, age in days
#' `lf001`    | Integer, ...: 1 = yes, 0 = no, NA = not administered
#' `lf002`    | Integer, ...: 1 = yes, 0 = no, NA = not administered
#' `...`      | and so on..
#'
#' Sample data for 155 `gto` items from GSED SF
#' @examples
#' head(sample_lf)
#' @seealso [dscore()]
"sample_lf"

#' Sample of 10 children from GSED HH
#'
#' A demo dataset with developmental scores at the item level for
#' 10 random children from the GSED Phase 1 data.
#'
#' @docType data
#' @format A `data.frame` with 10 rows and 57 variables:
#'
#' Name       | Label
#' ---------- | ---------
#' `subjid`   | Integer, child ID
#' `agedays`  | Integer, age in days
#' `hf001`    | Integer, ...: 1 = yes, 0 = no, NA = not administered
#' `hf002`    | Integer, ...: 1 = yes, 0 = no, NA = not administered
#' `...`      | and so on..
#'
#' Sample data for 55 `gpa` items forming GSED HH V1
#' @examples
#' head(sample_hf)
#' @seealso [dscore()]
"sample_hf"
