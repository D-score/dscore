#' Median D-score from the base population for a given key
#'
#' Returns the age-interpolated median of the D-score of the default
#' reference for a given key.
#'
#' Use `get_reference()` for more options.
#' @param t Decimal age, numeric vector
#' @param key Character, key of the reference population
#' @param prior_mean_NA Numeric, prior mean when age is missing
#' @return
#' A vector of length `length(t)` with the median of the default reference
#' population for the key.
#' @export
get_mu <- function(t, key, prior_mean_NA = NA_real_) {
  # calculate P50 from the default population for the key
  init <- init_key(key = key, population = NULL, transform = NULL, qp = NULL)
  population <- init$population
  mu <- switch(population,
               "dutch" = count_mu_dutch(t),
               "gcdg" = count_mu_gcdg(t),
               "phase1" = count_mu_phase1(t),
               "preliminary_standards" = count_mu_preliminary_standards(t),
               rep(NA_real_, length(t)))
  mu[is.na(t)] <- prior_mean_NA
  return(mu)
}

#' Median D-score from the default references for the given key
#'
#' Returns the age-interpolated median of the D-score of the default
#' reference for a given key.
#'
#' Do not use this function if you want the median D-score for a specific
#' reference.
#'
#' DEPRECATED in dscore 1.9.6
#' @param t Decimal age, numeric vector
#' @param key Character, key of the reference population
#' @param prior_mean_NA Numeric, prior mean when age is missing
#' @return
#' A vector of length `length(t)` with the median of the default reference
#' population for the key.
#' @export
count_mu <- function(t, key, prior_mean_NA = NA_real_) {
  .Deprecated("get_mu")
  get_mu(t, key, prior_mean_NA)
}


#' Median of Dutch references
#'
#' Returns the age-interpolated median of the Dutch references (van Buuren 2014).
#' The working range is 0-3 years. This function is used
#' to set prior mean under key `"dutch"`.
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
#' et al, 2019). The working range is 0-4 years. This function is used
#' to set prior mean under keys `"gcdg"` and `"gsed1912"`.
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
#' based on LF & SF in GSED-BGD, GSED-PAK, GSED-TZA. This function is used
#' to set prior mean under keys `"293_0"` and `"gsed2212"`.
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
#'
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

#' Median of preliminary_standards
#'
#' Returns the age-interpolated median of the preliminary_standards
#' based on LF & SF in GSED-BGD, GSED-PAK, GSED-TZA. This function is used
#' to set prior mean under key `"gsed2406"`.
#'
#' @param t Decimal age, numeric vector
#' @return
#' A vector of length `length(t)` with the median of the GCDG references.
#' @note Internal function. Called by `dscore()`
#' @author Stef van Buuren, on behalf of GSED project
#' @examples
#' dscore:::count_mu_preliminary_standards(0:5)
count_mu_preliminary_standards <- function(t) {

  to <- !is.na(t)
  t0 <- to & t < -1/12
  t1 <- to & t <= 0.75
  t2 <- to & t > 0.75 & t <= 3.5
  t3 <- to & t > 3.5

  # Round 1 model
  # t[t1] <- suppressWarnings(24.226 + 24.057 * t[t1] + 8.996 * log(t[t1] + 0.2))
  # t[t2] <- suppressWarnings(18.012 - 9.561 * t[t2] + 62.214 * log(t[t2] + 0.92))
  # t[t3] <- suppressWarnings(63.0822 + 3.9134 * t[t3])

  # Round 3 model
  # t[t1] <- suppressWarnings(24.486 + 23.912 * t[t1] + 9.165 * log(t[t1] + 0.2))
  # t[t2] <- suppressWarnings(18.391 - 9.245 * t[t2] + 61.361 * log(t[t2] + 0.92))
  # t[t3] <- suppressWarnings(61.5214 + 4.4309 * t[t3])

  # Round 2 model
  t[t0] <- NA_real_
  t[t1] <- suppressWarnings(24.522 + 23.886 * t[t1] + 9.190 * log(t[t1] + 0.2))
  t[t2] <- suppressWarnings(18.434 - 9.206 * t[t2] + 61.255 * log(t[t2] + 0.92))
  t[t3] <- suppressWarnings(62.6268 + 4 * t[t3])

  return(t)
}
