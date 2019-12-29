#' D-score standard deviation score: DAZ
#'
#' The `daz()` function calculated the
#' "Development for Age Z-score".
#' The DAZ represents a child's D-score after adjusting
#' for age by an external age-conditional reference.
#' The `zad()` is the inverse of `daz()`: Given age and
#' the Z-score, it finds the raw D-score.
#'
#' @rdname daz
#' @param d Vector of D-scores
#' @param z Vector of standard deviation scores (DAZ)
#' @param x Vector of ages (decimal age)
#' @param reference A `data.frame` with the LMS reference values.
#' The default uses the `get_reference()` function. This selects
#' a subset of rows from the `builtin_references` using its
#' default `pop` argument.
#' @param dec The number of decimals (default `dec = 3`).
#' @return The `daz()` function return a named vector with
#' Z-scores with `length(d)` elements
#' @references
#' Cole TJ, Green PJ (1992). Smoothing reference centile curves: The LMS
#' method and penalized likelihood. Statistics in Medicine, 11(10),
#' 1305-1319.
#' @seealso [dscore()]
#' @author Stef van Buuren 2020
#' @examples
#' # using gcdg-reference
#' daz(d = c(35, 50), x = c(0.5, 1.0))
#'
#' # using Dutch reference
#' daz(d = c(35, 50), x = c(0.5, 1.0), reference = get_reference("dutch"))
#' @export
daz <- function(d, x = as.numeric(names(d)),
                reference = get_reference(),
                dec = 3) {
  if (length(d) != length(x)) stop("Arguments `x` and  `d` of different length")

  # interpolate to proper ages
  l <- approx(x = reference[, "age"], y = reference[, "nu"], xout = x)$y
  m <- approx(x = reference[, "age"], y = reference[, "mu"], xout = x)$y
  s <- approx(x = reference[, "age"], y = reference[, "sigma"], xout = x)$y

  # LMS formula
  z <- ifelse(l > 0.01 | l < (-0.01),
    (((d / m)^l) - 1) / (l * s),
    log(d / m) / s
  )
  names(z) <- as.character(x)
  return(round(z, dec))
}

#' @return The `zad()` function returns a vector with D-scores
#' with `length(z)` elements.
#' @rdname daz
#' @examples
#' # population median at ages 0.5, 1 and 2 years, gcdg reference
#' zad(z = rep(0, 3), x = c(0.5, 1, 2))
#'
#' # population median at ages 0.5, 1 and 2 years, dutch reference
#' zad(z = rep(0, 3), x = c(0.5, 1, 2), reference = get_reference("dutch"))
#'
#' # percentiles of D-score reference
#' g <- expand.grid(age = seq(0.1, 2, 0.1), p = c(0.1, 0.5, 0.9))
#' d <- zad(z = qnorm(g$p), x = g$age)
#' matplot(
#'   x = matrix(g$age, ncol = 3), y = matrix(d, ncol = 3), type = "l",
#'   lty = 1, col = "blue", xlab = "Age (years)", ylab = "D-score"
#' )
#' @export
zad <- function(z, x = as.numeric(names(z)),
                reference = get_reference(),
                dec = 2) {
  if (length(z) != length(x)) stop("Arguments `x` and  `z` of different length")

  # interpolate to proper ages
  mu <- approx(reference[, "age"], reference[, "mu"], xout = x)$y
  sigma <- approx(reference[, "age"], reference[, "sigma"], xout = x)$y
  nu <- approx(reference[, "age"], reference[, "nu"], xout = x)$y

  # centile formula
  d <- ifelse(nu > 0.01 | nu < (-0.01),
    mu * ((nu * sigma * z + 1) ^ (1 / nu)),
    mu * exp(sigma * z)
  )
  names(d) <- as.character(x)
  return(round(d, dec))
}
