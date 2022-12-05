#' Median of Dutch references
#'
#' Returns the age-interpolated median of the Dutch references (van Buuren 2014).
#' The working range is 0-3 years. This function should
#' be called when the `key = "dutch"`.
#' @param t Decimal age, numeric vector
#' @return
#' A vector of length `length(t)` with the median of the Dutch references.
#' @note Internal function. Called by `dscore()`
#' @examples
#' dscore:::count_mu_dutch(0:2)
count_mu_dutch <- function(t) {
  suppressWarnings(44.35 - 1.8 * t + 28.47 * log(t + 0.25))
}

#' Median of GCDG references
#'
#' Returns the age-interpolated median of the GCDG references (Weber
#' et al, 2019). The working range is 0-4 years. This function should
#' be called when the `key = "gsed"` or `key = "gcdg"`.
#' @param t Decimal age, numeric vector
#' @return
#' A vector of length `length(t)` with the median of the GCDG references.
#' @note Internal function. Called by `dscore()`
#' @examples
#' dscore:::count_mu_gcdg(0:2)
count_mu_gcdg <- function(t) {
  suppressWarnings(47.65 - 3.05 * t + 26.70 * log(t + 0.19))
}


#' Median of phase1 references
#'
#' Returns the age-interpolated median of the phase1 references
#' based on LF & SF in GSED-BGD, GSED-PAK, GSED-TZA.
#'
#' The interpolation is done in two rounds. First round: Calculate D-scores
#' using .gcdg prior-mean, calculate reference, estimate round 1 parameters
#' used in this function. Round 2: Calculate D-score using round 1 estimates as
#' the prior mean (most differences are within 0.1 D-score points),
#' recalculate references, estimate round 2 parameters used in this function.
#'
#' Round 1:
#' Count model: <= 9MN: 21.3449 + 26.4916 t +  7.0251(t + 0.2)
#' Count model: > 9Mn & <= 3.5 YR: 14.69947 - 12.18636 t + 69.11675(t + 0.92)
#' Linear model: > 3.5 YRS: 61.40956 + 3.80904 t
#'
#' Round 2:
#' Count model: < 9MND: 20.5883 + 27.3376 t +  6.4254(t + 0.2)
#' Count model: > 9MND & < 3.5 YR: 14.63748 - 12.11774 t + 69.05463(t + 0.92)
#' Linear model: > 3.5 YRS: 61.37967 + 3.83513 t
#'
#' The working range is 0-3.5 years. After the age of 3.5 years, the function
#' will increase at an arbitrary rate of 3.8 D-score points per year.
#' This function is intended to be called when `key = "gsed2212"`,
#' `key = "gsed2208"` or `key = "293_0"`.
#' @param t Decimal age, numeric vector
#' @return
#' A vector of length `length(t)` with the median of the GCDG references.
#' @note Internal function. Called by `dscore()`
#' @author Stef van Buuren, on behalf of GSED project
#' @examples
#' dscore:::count_mu_phase1(0:5)
count_mu_phase1 <- function(t) {

  to <- !is.na(t)
  t1 <- to & t <= 0.75
  t2 <- to & t > 0.75 & t <= 3.5
  t3 <- to & t > 3.5

  # Round 1 model
  # t[t1] <- suppressWarnings(21.3449 + 26.4916 * t[t1] + 7.0251 * log(t[t1] + 0.2))
  # t[t2] <- suppressWarnings(14.69947 - 12.18636 * t[t2] + 69.11675 * log(t[t2] + 0.92))
  # t[t3] <- suppressWarnings(61.40956 + 3.80904 * t[t3])

  # Round 2 model
  t[t1] <- suppressWarnings(20.5883 + 27.3376 * t[t1] + 6.4254 * log(t[t1] + 0.2))
  t[t2] <- suppressWarnings(14.63748 - 12.11774 * t[t2] + 69.05463 * log(t[t2] + 0.92))
  t[t3] <- suppressWarnings(61.37967 + 3.83513 * t[t3])

  return(t)
}
