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
#'
#' The `"gcdg"` was calculated from 15 cohorts with direct
#' observations (Weber, 2019).
#'
#' The `"phase1"` references were calculated from the GSED Phase 1 validation
#' data (GSED-BGD, GSED-PAK, GSED-TZA) cover age range 2w-3.5 years. The
#' age range 3.5-5 yrs is linearly extrapolated and are only indicative.
#' (Van Buuren et al, 2025)
#'
#' The `"preliminary_standards"` references were calculated from the GSED
#' Phase 1 validation using a subset of children with healthy development.
#' (Van Buuren et al, 2025)
#'
#' The `"who_descriptive"` references were calculated from the GSED
#' Phase 1 + 2 (Seven countries) validation study using the `"gsed2510"` key.
#' It is a descriptive reference, i.e., no selection of children growing
#' up in healthy environments was made. (In preparation for publication).
#'
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
#' van Buuren S, Eekhout I, McCray G, Lancaster GA, Waldman MR, McCoy DC,
#' Gladstone M, Cavallera, V, Dua T, Black MM, GSED Team (2025).
#' Enhancing comparability in early child development assessment with the
#' D-score. International Journal of Behavioral Development, 49(4), 348-364,
#' <https://doi.org/10.1177/01650254241294033>
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
#' # loading a non-existing reference yield fallback to default
#' reftab <- get_reference(population = "france", verbose = TRUE)
#'
#' # if user specifies a builtin population (e.g. who_descriptive) and the key
#' # is not found, then it returns the specified reference for its most recent key
#' reftab <- get_reference(key = "none", population = "preliminary_standards", verbose = TRUE)
#' nrow(reftab)
#' @export
get_reference <- function(
  population = NULL,
  key = NULL,
  references = dscore::builtin_references,
  verbose = FALSE,
  ...
) {
  user_specified_population <- ifelse(is.null(population), FALSE, TRUE)

  init <- init_key(
    key = key,
    population = population,
    transform = NULL,
    qp = NULL
  )
  key <- init$key
  population <- init$population

  if (verbose) {
    cat("key:        ", key, "\n")
    cat("population: ", population, "\n")
  }

  # filter rows from references
  idx <- which(references$key == key & references$population == population)
  if (!any(idx)) {
    # use default reference `preliminary_standards` (calculated for gsed2406)
    # if 1) key is one of the builtin keys and 2) the requested population in
    # is not found for that key

    # If user speficies a builtin population, then find the most recent key
    # and return the reference for that key
    if (
      user_specified_population &&
        population %in% unique(dscore::builtin_references$population)
    ) {
      history <- c(
        "gsed2510",
        "gsed2406",
        "gsed2212",
        "293_0",
        "gsed1912",
        "gcdg",
        "dutch"
      )
      for (k in history) {
        if (any(references$key == k & references$population == population)) {
          key_new <- k
          if (verbose) {
            cat("Using key:  ", key_new, "\n")
          }

          idx <- which(
            references$key == key_new &
              references$population == population
          )
          ref <- references[idx, ]
          return(ref)
        }
      }
      if (any(idx)) {
        warning(
          "Reference '",
          population,
          "' for key '",
          key,
          "' not found. Fallback: '",
          population,
          "' for key 'gsed2406'.",
          call. = FALSE
        )
        ref <- references[idx, ]
        return(ref)
      }
    }
    warning(
      "Reference '",
      population,
      "' for key '",
      key,
      "' not found. Fallback: 'preliminary_standards' for key 'gsed2406'.",
      call. = FALSE
    )
    idx <- which(
      references$key == "gsed2406" &
        references$population == "preliminary_standards"
    )
  }

  ref <- references[idx, ]
  return(ref)
}
