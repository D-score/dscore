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
#' 
#' # the sum is always equal to 1
#' sum(dscore:::normalize(rnorm(5), qp = 1:5))
#' @export
normalize <- function(d, qp) {
  normalized = (d/sum(d)) / (qp[2] - qp[1]);
  return(normalized)
}

