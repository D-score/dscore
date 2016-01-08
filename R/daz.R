#' D-score standard deviation score: DAZ
#' 
#' DAZ stands for "Development - Age adjusted Z-score". 
#' The DAZ represents a child's D-score after adjusting 
#' for age by an external age-conditional reference.
#' @aliases daz
#' @param d Vector of D-scores, typically calculated by \code{dscore()}
#' @param x Vector of ages. The default is to take age from 
#' \code{names(d)}.
#' @param x.unit Units given in \code{x} specified by 
#' \code{"year"}, \code{"month"} or \code{"day"}. The default is 
#' \code{"year"}.
#' @param ref The LMS reference values. The default uses the 
#' built-in reference \code{dscore::Dreference} for Dutch children
#' published in Van Buuren (2014). The table should contain the columns
#' \code{nu}, \code{mu} and \code{sigma}, and at least one of the columns
#' \code{year}, \code{month} or \code{day}.
#' @param dec The number of decimals (default \code{dec = 3}).
#' @return Named vector with Z-scores with \code{length(d)} elements
#' @references
#' Cole TJ, Green PJ (1992). Smoothing reference centile curves: The LMS 
#' method and penalized likelihood. Statistics in Medicine, 11(10), 
#' 1305-1319.
#' @seealso \code{\link{dscore}}, \code{\link{daz}}
#' @examples
#' # preterms, uncorrected age, all Z-scores are fairly low
#' daz1 <- daz(d = pops$Dscore, x = pops$age/365.25)
#' daz2 <- daz(d = pops$Dscore, x = pops$age, x.unit = "day")
#' plot(daz1, daz2)
#' abline(0, 1, lty = 2)
#' 
#' # preterms, corrected age, most Z-scores are between -2 and +2 SD
#' daz3 <- daz(d = pops$Dscore, x = pops$daycor, x.unit = "day")
#' plot(daz2, daz3, xlab = "DAZ uncorrected age", ylab = "DAZ corrected age")
#' abline(0, 1, lty = 2, h = c(2, 0, -2), v = c(2, 0, -2))
#' @export
daz <- function(d, x = as.numeric(names(d)),
                x.unit = "year", 
                ref = dscore::Dreference, 
                dec = 3) {
  if (length(d) != length(x)) stop("Arguments `x` and  `d` of different length")
  x.unit <- match.arg(x.unit, c("year", "month", "day"))
  
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

#' Inverse D-score standard deviation score
#' 
#' This function is the inverse of \code{daz()}: Given age and 
#' the Z-score, it finds the raw D-score.
#' @aliases zad
#' @param z Vector of standard deviation scores (DAZ)
#' @param x Vector of ages. The default is to take age from 
#' \code{names(z)}.
#' @param x.unit Units given in \code{x} specified by 
#' \code{"year"}, \code{"month"} or \code{"day"}. The default is 
#' \code{"year"}.
#' @param ref The LMS reference values. The default uses the 
#' built-in reference \code{dscore::Dreference} for Dutch children
#' published in Van Buuren (2014). The table should contain the columns
#' \code{nu}, \code{mu} and \code{sigma}, and at least one of the columns
#' \code{year}, \code{month} or \code{day}.
#' @param dec The number of decimals (default \code{dec = 2}).
#' @return Names vector with D-scores with \code{length(z)} elements
#' @references
#' Cole TJ, Green PJ (1992). Smoothing reference centile curves: The LMS 
#' method and penalized likelihood. Statistics in Medicine, 11(10), 
#' 1305-1319.
#' @seealso \code{\link{dscore}}, \code{\link{daz}}
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
                x.unit = "year", 
                ref = dscore::Dreference, 
                dec = 2) {
  if (length(z) != length(x)) stop("Arguments `x` and  `z` of different length")
  x.unit <- match.arg(x.unit, c("year", "month", "day"))
  
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

