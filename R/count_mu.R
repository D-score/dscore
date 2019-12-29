#' Median of Dutch references
#'
#' Returns the age-interpolated median of the Dutch references. Thw
#' working range is 0-3 years.
#' @param t Decimal age, numeric vector
#' @return
#' A vector of length `length(t)` with the median of the dutch references.
#' @note Interal function. Called by `dscore()`
#' @examples
#' dscore:::count_mu_dutch(0:2)
count_mu_dutch <- function(t) {
  44.35 - 1.8 * t + 28.47 * log(t + 0.25)
}

#' Median of gcdg references
#'
#' Returns the age-interpolated median of the GCDG references (Weber
#' et al, 2019). The working range is 0-4 years.
#' @param t Decimal age, numeric vector
#' @return
#' A vector of length `length(t)` with the median of the dutch references.
#' @note Interal function. Called by `dscore()`
#' @examples
#' dscore:::count_mu_gcdg(0:2)
count_mu_gcdg <- function(t) {
  47.65 - 3.05 * t + 26.70 * log(t + 0.19)
}
