#' Calculate posterior for one item given score, difficulty and prior
#'
#' @details
#' This function assumes that the difficulties have been estimated by 
#' a binary Rasch model (e.g. by 
#' \code{sirt::rasch.pairwise.itemcluster()}) or - more generally - by 
#' a polytomous Rasch model (e.g. by \code{RUMM2030}), and 
#' transformed onto the 
#' appropriate scale. The response vector takes on values
#' \code{0:m}. The binary Rasch model assumes \code{m = 1}. The D-score
#' calculation assumes binary scores. 
#' 
#' Note: This function is intended to be used internally and 
#' does not check input arguments. Use \code{dscore()} to estimate
#' D-score from data.
#' 
#' @aliases posterior
#' @param score A scalar value between 0 and m
#' @param delta A vector of length \code{m} with uncentralized difficulties from the Rasch model
#' @param prior vector of prior values on quadrature points \code{qp}
#' @param qp vector of equally spaced quadrature points
#' @return A vector of length \code{length(prior)}
#' @author Stef van Buuren 2016
#' @references
#' Andrich D. Rasch Models for Measurement. Newbury Park: 
#' Sage Publications; 1988.
#' 
#' RUMM Laboratories. RUMM 2030. 
#' Rasch Unidimensional Measurement Models. Perth: 2015.
#' @seealso \code{\link{dscore}}, \code{\link{adp}}, 
#' \code{\link[sirt]{rasch.pairwise.itemcluster}}
posterior <- function(score, delta, prior, qp)
{
  m <- length(delta)
  score <- floor(score)
  if (score < 0 | score > m) stop("score out-of-range.")
  
  # compute category response probability under the 1PL (Rasch) model
  # for a vector of uncentralized threshold parameters delta
  cpc <- t(exp(outer(0:m, qp) + c(0, -cumsum(delta))))
  cpc <- cpc[,score + 1] / rowSums(cpc)
  cpc <- cpc / sum(cpc)
  
  # clean out any missing entries
  prior[is.na(prior)] <- 0
  
  # calculate the posterior per category
  postcat <- cpc * prior
  postcat <- normalize(postcat, qp)
  
  # 
  return(postcat)
}

