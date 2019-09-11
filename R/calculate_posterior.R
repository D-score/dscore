#' Calculate posterior of ability
#' 
#' @inheritParams dscore
#' @param scores A vector with PASS/FAIL observations. 
#' Scores are coded numerically as \code{pass = 1} and \code{fail = 0}. 
#' Alternatively, \code{pass = TRUE} and \code{fail = FALSE} may be used. 
#' @param delta A vector containing the item difficulties for the item scores in \code{scores}
#' estimated from the Rasch model in the prefferred metric/scale.
#' @param mu Numeric scalar. The mean of the prior.
#' @param sd Numeric scalar. Standard deviation of the prior.
calculate_posterior <- function(scores, delta, qp, mu, sd) {
  fullpost <- list(eap = NA, start = NULL, qp = qp, posterior = NULL)
  
  k <- 0                          # valid scores counter
  for (j in seq_along(scores)) {  # loop over item scores
    score <- scores[j]            # observed score
    deltaj <- delta[j]            # difficulty for item of observed score
    if (is.na(score) | is.na(deltaj)) next
    k <- k + 1                    # yes, we have a valid response
    
    # For first item, get age-dependent prior, else old posterior
    if (k == 1) {
      prior <- dnorm(qp, mean = mu, sd = sd)
      prior <- normalize(prior, qp)
      fullpost$start <- prior
    }
    else 
      prior <- post
  
    # calculate posterior
    post <- posterior(score, deltaj, prior, qp)
    fullpost$posterior <- post
    
    # overwrite old eap estimate by new one
    fullpost$eap <- weighted.mean(x = qp, w = post)
  }
  fullpost
}
