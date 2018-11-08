#' Calculate posterior for one item given score, difficulty and prior
#'
#' @details
#' This function assumes that the difficulties have been estimated by 
#' a binary Rasch model (e.g. by 
#' \code{sirt::rasch.pairwise.itemcluster()}). 
#' 
#' @aliases posterior
#' @param score Integer, either 0 (fail) and 1 (pass)
#' @param delta Numeric, difficulty parameter
#' @param prior Vector of prior values on quadrature points \code{qp}
#' @param qp vector of equally spaced quadrature points
#' @return A vector of length \code{length(prior)}
#' @author Stef van Buuren 2018
#' @seealso \code{\link{dscore}}, \code{\link{adp}}, 
#' \code{\link[sirt]{rasch.pairwise.itemcluster}}
posterior <- function(score, delta, prior, qp)
{
  m <- length(delta)
  score <- as.integer(score) + 1L
  if (score < 1L | score > 2L) stop("score out-of-range.")
  
  # compute category response probability under the 1PL (Rasch) model
  # for a vector of uncentralized threshold parameters delta
  p <- plogis(qp, location = delta)
  cpc <- switch(score, 1.0 - p, p, NA)
  cpc <- cpc / sum(cpc)
  
  # clean out any missing entries
  prior[is.na(prior)] <- 0
  
  # calculate the posterior per category
  postcat <- cpc * prior
  postcat <- normalize(postcat, qp)
  
  # 
  return(postcat)
}

