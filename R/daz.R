#' D-score standard deviation score: DAZ
#' 
#' DAZ stands for "Development - Age adjusted Z-score".
#' @aliases daz
#' @param d Vector of D-scores, typically calculated by \code{dscore()}
#' @param x Vector of ages
#' @param x.unit Units given in \code{x} specified by 
#' \code{"year"}, \code{"month"} or \code{""day}. The 
#' @param reference The LMS reference values. The default uses the 
#' built-in reference \code{dscore::Dreference} for Dutch children
#' published in Van Buuren (2014). The table should contain the columns
#' \code{nu}, \code{mu} and \code{sigma}, and at least one of the columns
#' \code{year}, \code{month} or \code{day}.
#' @param dec The number of decimals (default \code{dec = 3}).
#' @return Vector with Z-scores with \code{length(d)} elements
#' @references
#' Cole TJ, Green PJ (1992). Smoothing reference centile curves: The LMS 
#' method and penalized likelihood. Statistics in Medicine, 11(10), 
#' 1305-1319.
#' @seealso \code{\link{dscore}}
#' @export
daz <- function(d, x,
                x.unit = "year", 
                reference = dscore::Dreference, 
                dec = 3) {
  if (length(d) != length(x)) stop("Arguments `x` and  `d` of different length")
  x.unit <- match.arg(x.unit, c("year", "month", "day"))
  
  # interpolate to proper ages
  L <- approx(x = reference[, x.unit], 
              y = reference[, "nu"], 
              xout = x)$y
  M <- approx(x = reference[, x.unit], 
              y = reference[, "mu"], 
              xout = x)$y
  S <- approx(x = reference[, x.unit], 
              y = reference[, "sigma"], 
              xout = x)$y
  
  # LMS formula
  z <- ifelse(L > 0.01 | L < (-0.01), 
              (((d / M)^L) - 1) / (L * S), 
              log(d / M) / S )
  return(round(z))
}


