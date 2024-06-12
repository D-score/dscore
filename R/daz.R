#' Calculate Development-for-Age Z-score (DAZ)
#'
#' The `daz()` function calculated the Development-for-Age Z-score (DAZ).
#' The DAZ represents a child's D-score after adjusting for age by an
#' external age-conditional reference.
#'
#' The `zad()` is the inverse of `daz()`: Given age and
#' the Z-score, it finds the raw D-score.
#'
#' @rdname daz
#' @param d Vector of D-scores
#' @param z Vector of standard deviation scores (DAZ)
#' @param x Vector of ages (decimal age)
#' @param reference_table A `data.frame` with the LMS or BCT reference values.
#' The default `NULL` selects the default reference belonging to the `key`,
#' as specified in the `base_population` field in `dscore::builtin_keys`.
#' @param dec The number of decimals (default `dec = 3`).
#' @param verbose Print out the used reference table (default `verbose = FALSE`).
#' @return Unnamed numeric vector with Z-scores of length `length(d)`.
#' @details
#'
#' Note 1: The Box-Cox Cole and Green (BCCG) and Box-Cox t (BCT)
#' distributions model only positive D-score values. To increase
#' robustness, the `daz()` and `zad()` functions will round up any
#' D-scores lower than 1.0 to 1.0.
#'
#' Note 2: The `daz()` and `zad()` function call modified version of the
#' `pBCT()` and `qBCT()` functions from `gamlss` for better handling
#' of `NA`'s and rounding.
#' @references
#' Cole TJ, Green PJ (1992). Smoothing reference centile curves: The LMS
#' method and penalized likelihood. Statistics in Medicine, 11(10),
#' 1305-1319.
#' @seealso [dscore()][get_reference()]
#' @author Stef van Buuren
#' @examples
#' # using default reference and key
#' daz(d = c(35, 50), x = c(0.5, 1.0))
#'
#' # print out names of the used reference table
#' daz(d = c(35, 50), x = c(0.5, 1.0), verbose = TRUE)
#'
#' # using the default reference in key gcdg
#' reftab <- get_reference(key = "gcdg")
#' daz(d = c(35, 50), x = c(0.5, 1.0), reference_table = reftab)
#'
#' # using Dutch reference in default key
#' reftab <- get_reference(population = "dutch", verbose = TRUE)
#' daz(d = c(35, 50), x = c(0.5, 1.0), reference_table = reftab)
#' @export
daz <- function(d, x, reference_table = NULL, dec = 3, verbose = FALSE) {
  if (length(d) != length(x)) stop("Arguments `x` and  `d` of different length")

  if (is.null(reference_table)) {
    rt <- get_reference(verbose = verbose)
  } else {
    rt <- reference_table
  }

  # Return NA if there is no reference
  if (!nrow(rt)) {
    return(rep(NA_real_, length(d)))
  }

  dist <- rt$distribution[1L]
  if (dist == "LMS") {
    l <- approx(x = rt[, "age"], y = rt[, "nu"], xout = x)$y
    m <- approx(x = rt[, "age"], y = rt[, "mu"], xout = x)$y
    s <- approx(x = rt[, "age"], y = rt[, "sigma"], xout = x)$y
    z <- ifelse(l > 0.01 | l < (-0.01),
                (((d / m)^l) - 1) / (l * s),
                log(d / m) / s)
  } else if (dist == "BCT") {
    mu <- approx(x = rt[, "age"], y = rt[, "mu"], xout = x)$y
    sigma <- approx(x = rt[, "age"], y = rt[, "sigma"], xout = x)$y
    nu <- approx(x = rt[, "age"], y = rt[, "nu"], xout = x)$y
    tau <- approx(x = rt[, "age"], y = rt[, "tau"], xout = x)$y
    z <- qnorm(pBCT(d, mu, sigma, nu, tau))
  } else {
    stop("Unknown distribution '", dist, "'.")
  }

  return(round(z, dec))
}

#' @return Unnamed numeric vector with D-scores of length `length(z)`.
#' @rdname daz
#' @examples
#' # population median at ages 0.5, 1 and 2 years, default reference
#' zad(z = rep(0, 3), x = c(0.5, 1, 2))
#'
#' # population median at ages 0.5, 1 and 2 years, gcdg key
#' reftab <- get_reference(key = "gcdg", verbose = TRUE)
#' zad(z = rep(0, 3), x = c(0.5, 1, 2), reference_table = reftab)
#'
#' # population median at ages 0.5, 1 and 2 years, dutch key
#' reftab <- get_reference(key = "dutch", verbose = TRUE)
#' zad(z = rep(0, 3), x = c(0.5, 1, 2), reference = reftab)
#' @export
zad <- function(z, x, reference_table = NULL, dec = 2, verbose = FALSE) {
  if (length(z) != length(x)) stop("Arguments `x` and  `z` of different length")

  if (is.null(reference_table)) {
    rt <- get_reference(verbose = verbose)
  } else {
    rt <- reference_table
  }

  # Return NA if there is no reference
  if (!nrow(rt)) {
    return(rep(NA_real_, length(z)))
  }

  dist <- rt$distribution[1L]
  if (dist == "LMS") {
    mu <- approx(rt[, "age"], rt[, "mu"], xout = x)$y
    sigma <- approx(rt[, "age"], rt[, "sigma"], xout = x)$y
    nu <- approx(rt[, "age"], rt[, "nu"], xout = x)$y
    d <- ifelse(nu > 0.01 | nu < (-0.01),
                mu * ((nu * sigma * z + 1)^(1 / nu)),
                mu * exp(sigma * z))
  } else if (dist == "BCT") {
    mu <- approx(x = rt[, "age"], y = rt[, "mu"], xout = x)$y
    sigma <- approx(x = rt[, "age"], y = rt[, "sigma"], xout = x)$y
    nu <- approx(x = rt[, "age"], y = rt[, "nu"], xout = x)$y
    tau <- approx(x = rt[, "age"], y = rt[, "tau"], xout = x)$y
    d <- qBCT(pnorm(z), mu, sigma, nu, tau)
  } else {
    stop("Unknown distribution '", dist, "'.")
  }

  return(round(d, dec))
}
