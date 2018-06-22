#' Calculate posterior of ability
#' 
#' @param scores BLABLA
#' @param delta BLABLA
#' @param age BLABLA
#' @param qp A number vector of equally spaced quadrature points.
#' This vector should span the range of all D-score values, and 
#' have at least 80 elements. The default is 
#' \code{qp = -10:100}, which is suitable for age range 0-4 years.
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
#' @param \dots Additional parameters passed down to BLABLA
#' @export
calculate_posterior <- function(scores, delta, age, 
                                qp = -10:100, mem.between = 0, mem.within = 1,
                                ...){
  fullpost <- list(eap = NA, start = NULL, qp = NULL, posterior = NULL)
  k <- 0       # valid scores counter
  if(!all.equal( max(age) ,min(age))) stop("age within group not equal")
  cage <-age[1]           # current age
  nextocc <- TRUE                 # flag for next occasion
  fullpost$qp <- qp #IE
  for (j in seq_along(scores)) {          # loop over item scores
    score <- scores[j]       # observed score
    delta <- delta[j]     # difficulty for item of observed score
    if (is.na(score) | is.na(cage) | is.na(delta)) next
    k <- k + 1                    # yes, we have a valid response
    
    # CASE A: k == 1: start with age-dependent prior for first valid score
    if (k == 1) {
      prior <- adp(age = cage, qp = qp,...)
      fullpost$start <- prior
      nextocc <- FALSE
    }
    
    # CASE B: nextocc is TRUE if this is the first valid response
    # at the present age. If so, weight the starting prior with
    # 'previous occasion posterior' by mem.between
    else if (nextocc) {
      prior <- mem.between * post +
        (1 - mem.between) * adp(age = cage, qp = qp,...)
      prior <- normalize(prior, qp)
      fullpost$start <- prior
      nextocc <- FALSE
    }
    
    # CASE C: weight 'previous score posterior' by mem.within
    else {
      prior <- mem.within * post +
        (1 - mem.within) * adp(age = cage, qp = qp,...)
      prior <- normalize(prior, qp)
    }
    # calculate posterior
    post <- posterior(score, delta, prior, qp)
    fullpost$posterior <- post
    
    # overwrite old eap estimate by new one
    fullpost$eap <- weighted.mean(qp, w = post)
  }
  fullpost
}
