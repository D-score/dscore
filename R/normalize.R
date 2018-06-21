#' Normalize distribution
#'
#' Normalizes the distribution so that the total mass equals 1.
#' @aliases normalize
#' @param d A vector with \code{length(qp)} elements representing
#' the unscaled density at each quadrature point.
#' @param qp Vector of equally spaced quadrature points.
#' @return A \code{vector} of \code{length(d)} elements with 
#' the prior density estimate at each quadature point.
#' @examples 
#' # simple normalization examples
#' dscore:::normalize(c(5, 10, 5), qp = c(0, 1, 2))
#' dscore:::normalize(c(1, 5, 8, 5, 1), qp = 1:5)
#' 
#' # the sum is always equal to 1
#' sum(dscore:::normalize(rnorm(5), qp = 1:5))
normalize <- function(d, qp) {
  if (length(d) != length(qp)) stop("Arguments `d` and  `qp` of different length")
  d <- d / sum(d)
  return(d / (qp[2] - qp[1]))
}

