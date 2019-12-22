#' Get D-score reference
#'
#' The `get_reference()` function selects the D-score reference
#' distribution.
#'
#' @param population A string describing the population. Currently supported
#' are `"dutch"` and `"gcdg"` (default).
#' @param references A `data.frame` with the same structure
#' as `builtin_references`. The default is to use
#' `builtin_references`.
#' @return A `data.frame` with the LMS reference values.
#' @note No references for population `"gsed"` exist.
#' The function will silently rewrite `population = "gsed"`
#' into to the `population = "gsed"`.
#'
#' The `"dutch"` reference was published in Van Buuren (2014)
#'
#' The `"gcdg"` was calculated from 15 cohorts with direct
#' observations (Weber, 2019).
#' @references
#' Van Buuren S (2014). Growth charts of human development.
#' Stat Methods Med Res, 23(4), 346-368.
#'
#' Weber AM, Rubio-Codina M, Walker SP, van Buuren S, Eekhout I,
#' Grantham-McGregor S, Caridad Araujo M, Chang SM, Fernald LCH,
#' Hamadani JD, Hanlon A, Karam SM, Lozoff B, Ratsifandrihamanana L,
#' Richter L, Black MM (2019). The D-score: a metric for interpreting
#' the early development of infants and toddlers across global settings.
#' BMJ Global Health, BMJ Global Health 4: e001724.
#' <https://gh.bmj.com/content/bmjgh/4/6/e001724.full.pdf>.
#'
#' @seealso [builtin_references()]
#' @export
get_reference <- function(population = "gcdg",
                          references = dscore::builtin_references) {
  if (population == "gsed") population <- "gcdg"
  references[references$pop == population, ]
}
