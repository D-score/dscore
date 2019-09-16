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
#' @param d Vector of D-scores, typically calculated by \code{dscore_vector()}
#' @param z Vector of standard deviation scores (DAZ)
#' @param x Vector of ages. The default is to take age from 
#' \code{names(d)}.
#' @param x.unit Units given in \code{x} specified by 
#' \code{"year"}, \code{"month"} or \code{"day"}. The default is 
#' \code{"year"}.
#' @param ref The LMS reference values. The default is to take 
#' GCDG-references 0-5 year. For Dutch data and key, use 
#' Dutch references.
#' @param dec The number of decimals (default \code{dec = 3}).
#' @return The \code{daz()} function return a named vector with 
#' Z-scores with \code{length(d)} elements
#' @references
#' Cole TJ, Green PJ (1992). Smoothing reference centile curves: The LMS 
#' method and penalized likelihood. Statistics in Medicine, 11(10), 
#' 1305-1319.
#' @seealso \code{\link{dscore}}
#' @examples
#' # preterms, uncorrected age, all Z-scores are fairly low
#' daz1 <- daz(d = popsdemo$dscore, x = popsdemo$age/365.25)
#' daz2 <- daz(d = popsdemo$dscore, x = popsdemo$age, x.unit = "day")
#' plot(daz1, daz2)
#' abline(0, 1, lty = 2)
#' 
#' # preterms, corrected age, most Z-scores are between -2 and +2 SD
#' daz3 <- daz(d = popsdemo$dscore, x = popsdemo$daycor, x.unit = "day")
#' plot(daz2, daz3, xlab = "DAZ uncorrected age", ylab = "DAZ corrected age")
#' abline(0, 1, lty = 2, h = c(2, 0, -2), v = c(2, 0, -2))
#' @export
daz <- function(d, x = as.numeric(names(d)),
                x.unit = c("year", "month", "day"), 
                ref = set_reference("gcdg"), 
                dec = 3) {
  if (length(d) != length(x)) stop("Arguments `x` and  `d` of different length")
  x.unit <- match.arg(x.unit)
  
  # interpolate to proper ages
  L <- approx(x = ref[, x.unit], y = ref[, "nu"], xout = x)$y
  M <- approx(x = ref[, x.unit], y = ref[, "mu"], xout = x)$y
  S <- approx(x = ref[, x.unit], y = ref[, "sigma"], xout = x)$y
  
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
#' # population median at ages 0.5, 1 and 2 years
#' zad(z = rep(0, 3), x = c(0.5, 1, 2))
#' 
#' # percentiles of D-score reference
#' g <- expand.grid(age = seq(0.1, 2, 0.1), p = c(0.1,0.5,0.9))
#' d <- zad(z = qnorm(g$p), x = g$age)
#' matplot(x = matrix(g$age, ncol = 3), y = matrix(d, ncol = 3), type = "l",
#' lty = 1, col = "blue", xlab = "Age (years)", ylab = "D-score")
#' @export
zad <- function(z, x = as.numeric(names(z)),
                x.unit = c("year", "month", "day"),
                ref = set_reference("gcdg"), 
                dec = 2) {
  if (length(z) != length(x)) stop("Arguments `x` and  `z` of different length")
  x.unit <- match.arg(x.unit)
  
  # interpolate to proper ages
  mu <- approx(ref[, x.unit], ref[, "mu"], xout = x)$y
  sigma <- approx(ref[, x.unit], ref[, "sigma"], xout = x)$y
  nu <- approx(ref[, x.unit], ref[, "nu"], xout = x)$y
  
  # centile formula
  d <- ifelse(nu > 0.01 | nu < (-0.01), 
              mu * ((nu * sigma * z + 1)^(1/nu)), 
              mu * exp(sigma * z))
  names(d) <- as.character(x)
  return(round(d, dec))
}

