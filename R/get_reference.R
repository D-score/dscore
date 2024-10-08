#' Get D-score reference
#'
#' The `get_reference()` function selects the D-score reference
#' distribution.
#'
#' @inheritParams dscore
#' @param references A `data.frame` with the same structure as `builtin_references`.
#' The default is to use `builtin_references`.
#' @param \dots Used to test whether the call contained the deprecated argument
#' `references`.
#' @return A `data.frame` with the LMS reference values.
#' @note No references for population `"gsed"` exist.
#' The function will silently rewrite `population = "gsed"`
#' into to the `population = "gsed"`.
#'
#' The `"dutch"` reference was published in Van Buuren (2014)
#' The `"gcdg"` was calculated from 15 cohorts with direct
#' observations (Weber, 2019).
#' The `"phase1"` references were calculated from the GSED Phase 1 validation
#' data (GSED-BGD, GSED-PAK, GSED-TZA) cover age range 2w-3.5 years. The
#' age range 3.5-5 yrs is linearly extrapolated and are only indicative.
#' The `"preliminary_standards"` references were calculated from the GSED
#' Phase 1 validation using a subset of children with healthy development.
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
#' @examples
#' # see key-population combinations of builtin_references
#' table(builtin_references$key, builtin_references$population)
#'
#' # get the default reference
#' reftab <- get_reference()
#' head(reftab, 2)
#'
#' # get the default reference for the key "gsed2212"
#' reftab <- get_reference(key = "gsed2212", verbose = TRUE)
#'
#' # get dutch reference for default key
#' reftab <- get_reference(population = "dutch", verbose = TRUE)
#'
#' # loading a non-existing reference yields zero rows
#' reftab <- get_reference(population = "france", verbose = TRUE)
#' nrow(reftab)
#' @export
get_reference <- function(population = NULL,
                          key = NULL,
                          references = dscore::builtin_references,
                          verbose = FALSE,
                          ...) {
  init <- init_key(key = key, population = population,
                   transform = NULL, qp = NULL)
  key <- init$key
  population <- init$population

  if (verbose) {
    cat("key:        ", key, "\n")
    cat("population: ", population, "\n")
  }

  # filter references
  idx <- which(references$key == key & references$population == population)
  if (!any(idx)) {
    # use default reference if not found
    warning("Reference '", population, "' for key '", key, "' not found. Using default.",
            call. = FALSE)
    idx <- which(references$key == "gsed2406" &
                   references$population == "preliminary_standards")
  }
  return(references[idx, ])
}
