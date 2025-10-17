#' Sample of 50 children measured with three instruments
#'
#' An example dataset with developmental scores at the item level for
#' 50 random children from the GSED Validation Study (Cavellera et al, 2023).
#' Each child has measurements from GSED SF (`gs1`), GSED LF (`gl1`) and
#' BSID-III (`by3`).
#'
#' @docType data
#' @format A `data.frame` with 50 rows and 559 variables:
#'
#' Name       | Label
#' ---------- | ---------
#' `id`       | Integer, child ID
#' `age`      | Numeric, age in decimal years
#' `agedays`  | Integer, age in days
#' `gs1sec001`| Integer, SF001 Does your child smile?
#' `gs1moc002`| Integer, SF002 When lying on his/her back, ...
#' `...`      | and so on..
#'
#' The dataset contains 138 items from GSED SF (`gs1`),
#' (item `gs1moc028` was skipped), 155 items from GSED LF (`gl1`),
#' and 263 (out of 326) items from BSID-III (`by3`).
#'
#' @examples
#' # calculate D-score from all instruments
#' ds_all <- dscore(triple)
#' head(ds_all)
#' # calculate D-score from only GSED SF items
#' ds_sf <- dscore(triple, items = get_itemnames(instrument = "gs1"))
#' head(ds_sf)
#' @seealso [dscore()]
#' @references
#' Cavallera et al. (2023). Protocol for validation of the Global
#' Scales for Early Development (GSED) for children under 3 years of
#' age in seven countries. BMJ Open, 13(1), e062562.
#' DOI: 10.1136/bmjopen-2022-062562.
#' <https://bmjopen.bmj.com/content/13/1/e062562>
#'
#' World Health Organization (WHO) (2023). Global Scales for Early
#' Development (GSED) V1.0: Technical Report. Geneva: World Health
#' Organization.
#' <https://www.who.int/publications/i/item/WHO-MSD-GSED-package-v1.0-2023.1>
"triple"
