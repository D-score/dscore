#' Calculate posterior for one item given score, difficulty and prior
#'
#' This function assumes that the difficulties have been estimated by 
#' a binary Rasch model (e.g. by 
#' \code{sirt::rasch.pairwise.itemcluster()}). 
#' 
#' @details
#' Internal function. Not the be called directly. 
#' @param score Integer, either 0 (fail) and 1 (pass)
#' @param tau Numeric, difficulty parameter
#' @param prior Vector of prior values on quadrature points \code{qp}
#' @param qp vector of equally spaced quadrature points
#' @return A vector of length \code{length(prior)}
#' @author Stef van Buuren 2018
#' @seealso \code{\link{dscore}}, 
#' \code{\link[sirt]{rasch.pairwise.itemcluster}}
posterior <- function(score, tau, prior, qp)
{
  m <- length(tau)
  score <- as.integer(score) + 1L
  if (score < 1L | score > 2L) stop("score out-of-range.")
  
  # compute category response probability under the 1PL (Rasch) model
  # for a vector of uncentralized threshold parameters tau
  p <- plogis(qp, location = tau)
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

