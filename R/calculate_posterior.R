#' Calculate posterior of ability
#' 
#' @param scores A vector with PASS/FAIL observations. 
#' Scores are coded numerically as \code{pass = 1} and \code{fail = 0}. 
#' Alternatively, \code{pass = TRUE} and \code{fail = FALSE} may be used. 
#' @param delta A vector containing the item difficulties for the item scores in \code{scores}
#' estimated from the Rasch model in the prefferred metric/scale.
#' @param age A vector age in months.
#' @param qp A number vector of equally spaced quadrature points.
#' This vector should span the range of all posterior values, and 
#' have at least 80 elements. The default is 
#' \code{qp = -10:100}, which is suitable for a d-score for age range 0-4 years.
#' For the ability score in the logit scale qp should span min(logit)-3 to max(logit)+3.
#' @param mem.between Fraction of posterior from previous occasion 
#' relative to age-dependent prior. The 
#' value \code{mem.between = 0} (the default) means that 
#' no smoothing over age is performed, while a \code{mem.between = 1} 
#' corresponds to maximal smoothing over age. See details.
#' @param mem.within Fraction of posterior from previous item within
#' the same age  relative to age-dependent prior. The 
#' value \code{mem.within = 1} (the default) means that all items 
#' count equally in the posterior, while a \code{mem.within = 0} 
#' corresponds to counting only the last item. See details.
#' @param metric The metric of the posterior via the prior. The default is dscore. The other option
#' is logit, then the age dependent prior mu is transformed to the logit scale by \code{transform}.
#' @param \dots Additional parameters passed down to \code{adp}.
#' @export
calculate_posterior <- function(scores, delta, age, 
                                qp = -10:100, mem.between = 0, 
                                mem.within = 1, metric = metric,
                                ...) {
  fullpost <- list(eap = NA, start = NULL, qp = NULL, posterior = NULL)
  k <- 0       # valid scores counter
  if(!all.equal(max(age), min(age))) stop("age within group not equal")
  cage <-age[1]           # current age
  nextocc <- TRUE                 # flag for next occasion
  fullpost$qp <- qp #IE
  for (j in seq_along(scores)) {          # loop over item scores
    score <- scores[j]       # observed score
    deltaj <- delta[j]     # difficulty for item of observed score
    if (is.na(score) | is.na(cage) | is.na(deltaj)) next
    k <- k + 1                    # yes, we have a valid response
    
    # CASE A: k == 1: start with age-dependent prior for first valid score
    if (k == 1) {
      prior <- adp(age = cage, qp = qp, metric = metric,...)
      fullpost$start <- prior
      nextocc <- FALSE
    }
    
    # CASE B: nextocc is TRUE if this is the first valid response
    # at the present age. If so, weight the starting prior with
    # 'previous occasion posterior' by mem.between
    else if (nextocc) {
      prior <- mem.between * post +
        (1 - mem.between) * adp(age = cage, qp = qp, metric = metric,...)
      prior <- normalize(prior, qp)
      fullpost$start <- prior
      nextocc <- FALSE
    }
    
    # CASE C: weight 'previous score posterior' by mem.within
    else {
      prior <- mem.within * post +
        (1 - mem.within) * adp(age = cage, qp = qp, metric = metric,...)
      prior <- normalize(prior, qp)
    }
    # calculate posterior
    post <- posterior(score, deltaj, prior, qp)
    fullpost$posterior <- post
    
    # overwrite old eap estimate by new one
    fullpost$eap <- weighted.mean(qp, w = post)
  }
  fullpost
}
