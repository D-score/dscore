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
#' @export
posterior <- function(score, tau, prior, qp){
  score <- score + 1
  if(score < 1 | score > 2){
    stop("score out-of-range.")}
  
  # compute category respones probability under 1PL model
  p <- plogis(qp, tau)
  
  if(score == 1){
    cpc = 1.0 - p
    cpc = cpc/sum(cpc)
  } else if(score == 2){
    cpc = p
    cpc = cpc/sum(cpc)
  } else if(score == 3){
    cpc = NA
  }
  
  # clean missings
  prior_miss = is.na(prior)
  prior[prior_miss] = 0
  
  # calc posterior per category
  postcat = cpc * prior
  postcat = normalize(postcat, qp)
  
  return(postcat)
}