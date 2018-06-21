#' Age-dependent prior
#'
#' Returns the age-dependent prior N(mu, 5) at the 
#' specified quadrature points.
#' @aliases adp
#' @param age Age in years. Numeric, single value. If a vector, only the 
#' first value will be used.
#' @param qp A number vector of equally spaced quadrature points.
#' This vector should span the range of all D-score values, and 
#' have at least 80 elements. The default is 
#' \code{qp = -10:100}, which is suitable for age range 0-4 years.
#' @param mu The mean of the prior. If \code{mu = "dutch"} (the default)
#' then \code{mu} is calculated from the Count model coded in 
#' \code{dscore:::count_mu_dutch()}. Specify \code{mu = "reference"} in order
#' to take it from the age-dependent reference (default < 0.22).
#' @param sd Standard deviation of the prior. The default is 5.
#' @param reference the LMS reference values. The default uses the 
#' built-in reference \code{dscore::Dreference} for Dutch children
#' published in Van Buuren (2014).
#' @param \dots Additional parameters (ignored)
#' @return  A \code{vector} of \code{length(qp)} elements with 
#' the prior density estimate at each quadature point \code{qp}.
#' @note Use \code{qp = -10:80} to reproduce 0-2 year estimates in 
#' Van Buuren (2014).
#' @references
#' Van Buuren S (2014). Growth charts of human development.
#' Stat Methods Med Res, 23(4), 346-368.
#' @seealso \code{\link{dscore}}
#' @examples 
#' # define quadrature points for D-score, as used in Van Buuren 2014
#' qp <- -10:80
#' 
#' # calculate and plot three priors
#' plot(x = qp, y= adp(1/12, qp), type = "l", 
#'   main = "Priors at ages of 1, 12 and 24 months", 
#'   ylab = "Density", xlab = "D-score")
#' lines(x = qp, adp(1, qp), lty = 2)
#' lines(x = qp, adp(2, qp), lty = 3)
#' @export
adp <- function(age, qp = -10:100, mu = "dutch", sd = 5, 
                reference = dscore::Dreference, ...) {
  age <- age[1]
  if (mu == "identity"){ mu <- ifelse(is.na(age),NA,0)
  sd <- 1}
  if (mu == "dutch") mu <- ifelse(is.na(age), NA, count_mu_dutch(age))
  if (mu == "gcdg") mu <- ifelse(is.na(age), NA,  count_mu_gcdg(age))
  if (mu == "reference")
    mu <- ifelse(is.na(age),
                 NA, 
                 approx(y = reference$mu, x = reference$year,
                        xout = round(age, 4), yleft = reference$mu[1])$y)
  p <- dnorm(qp, mean = unlist(mu), sd = sd)
  return(normalize(p, qp))
}

count_mu_dutch <- function(t) {44.35 - 1.8 * t + 28.47 * log(t + 0.25)}
count_mu_gcdg  <- function(t) {47.65 - 3.05 * t + 26.70 * log(t + 0.19)}
