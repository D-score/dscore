#' Age-dependent prior
#'
#' Returns the age-dependent prior N(mu, 5) at the 
#' specified quadrature points.
#' @aliases adp
#' @inheritParams dscore
#' @inheritParams calculate_posterior 
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
#' plot(x = qp, y= adp(qp, 20, 5), type = "l", 
#'   main = "Priors at ages of 1, 12 and 24 months", 
#'   ylab = "Density", xlab = "D-score")
#' lines(x = qp, adp(qp, 40, 3), lty = 2)
#' lines(x = qp, adp(qp, 60, 6), lty = 3)
#' @export
adp <- function(qp, mu, sd) {
  p <- dnorm(qp, mean = mu, sd = sd)
  normalize(p, qp)
}

