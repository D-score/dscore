#' D-score standard deviation score: DAZ
#' 
#' The \code{daz()} function calculated the 
#' "Development for Age Z-score". 
#' The DAZ represents a child's D-score after adjusting 
#' for age by an external age-conditional reference.
#' The \code{zad()} is the inverse of \code{daz()}: Given age and 
#' the Z-score, it finds the raw D-score.
#' 
#' @rdname daz
#' @inheritParams dscore
#' @param d Vector of D-scores
#' @param z Vector of standard deviation scores (DAZ)
#' @param x Vector of ages (decimal age)
#' @param reference A \code{data.frame} with the LMS reference values. 
#' The default uses the \code{get_reference()} function. This selects 
#' a subset of rows from the \code{builtin_references} using its
#' default \code{pop} argument.
#' @param dec The number of decimals (default \code{dec = 3}).
#' @return The \code{daz()} function return a named vector with 
#' Z-scores with \code{length(d)} elements
#' @references
#' Cole TJ, Green PJ (1992). Smoothing reference centile curves: The LMS 
#' method and penalized likelihood. Statistics in Medicine, 11(10), 
#' 1305-1319.
#' @seealso \code{\link{dscore}}
#' @author Stef van Buuren 2019
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
  L <- approx(x = reference[, "age"], y = reference[, "nu"], xout = x)$y
  M <- approx(x = reference[, "age"], y = reference[, "mu"], xout = x)$y
  S <- approx(x = reference[, "age"], y = reference[, "sigma"], xout = x)$y
  
  # LMS formula
  z <- ifelse(L > 0.01 | L < (-0.01), 
              (((d / M)^L) - 1) / (L * S), 
              log(d / M) / S )
  names(z) <- as.character(x)
  return(round(z, dec))
}

#' @return The \code{zad()} function returns a vector with D-scores 
#' with \code{length(z)} elements.
#' @rdname daz
#' @examples
#' # population median at ages 0.5, 1 and 2 years, gcdg reference
#' zad(z = rep(0, 3), x = c(0.5, 1, 2))
#' 
#' # population median at ages 0.5, 1 and 2 years, dutch reference
#' zad(z = rep(0, 3), x = c(0.5, 1, 2), reference = get_reference("dutch"))
#' 
#' # percentiles of D-score reference
#' g <- expand.grid(age = seq(0.1, 2, 0.1), p = c(0.1,0.5,0.9))
#' d <- zad(z = qnorm(g$p), x = g$age)
#' matplot(x = matrix(g$age, ncol = 3), y = matrix(d, ncol = 3), type = "l",
#' lty = 1, col = "blue", xlab = "Age (years)", ylab = "D-score")
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
              mu * ((nu * sigma * z + 1)^(1/nu)), 
              mu * exp(sigma * z))
  names(d) <- as.character(x)
  return(round(d, dec))
}

