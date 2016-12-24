#' D-score estimation
#' 
#' This function estimates the D-score, a numerical score that measures
#' generic development in children, from PASS/FAIL observations 
#' on developental milestones (items). 
#' Item names are matched by name against the built-in item 
#' bank in \code{itembank}. 
#' Items that do not match, or for which the score is 
#' \code{NA}, are ignored. 
#' 
#' @details By default, the algorithm produces the expected a 
#' posteriori (EAP) estimate from \code{scores} using item 
#' difficulty as obtained from the item bank, and an age-dependent
#' prior of about twice the normal variation at the given age.
#' The method uses Bayes rule. See Bock and Mislevy (1982) for more 
#' details on the EAP and its variance. 
#' Optionally, the \code{dscore()} function returns the 
#' full posterior (instead of the EAP) if the argument 
#' \code{full = TRUE} is specified.
#' 
#' The mixing coefficients \code{mem.between} and \code{mem.within} are 
#' 0-1 numbers (fractions) that regulate the influence 
#' of the previous posterior on the prior. 
#' There are three cases, depending on whether it's the 
#' first age and first item:
#' \describe{
#' \item{First age, first item:}{For the first item at the first age
#' \code{t}, the prior is set to normal with mean equal to the
#' median D-score at age \code{t} as calculated from \code{Dreference[,"mu"]} and 
#' where the standard deviation is equal to 5, about twice the normal variation in \code{Dreference}.}
#' \item{Later age, first item:}{For the first item at the second or later age \code{t}, the 
#' prior is set to \code{mem.between * post + (1 - mem.between) * adp()}, 
#' where \code{post} is the D-score posterior as calculated from the 
#' previous age, and where \code{adp()} is the age-dependent prior at
#' age \code{t}. The mixing coefficient \code{mem.between = 0} by default, which
#' implies that D-scores of the same child at different ages are calculated 
#' independently.}
#' \item{Any age, later item:}{For the second or further items at any 
#' given age \code{t}, the prior
#' is set to \code{mem.within * post + (1 - mem.within) * adp()}, where 
#' \code{post} is the D-score posterior as calculated from the 
#' previous item within age \code{t}, and 
#' where \code{adp()} is the age-dependent prior at age \code{t}. 
#' By default the mixing coefficient \code{mem.within = 1}, which
#' mimmicks repeated application of Bayesian rule to the current data 
#' using 'old' data as prior.}
#' }
#' 
#' @aliases dscore 
#' @param scores A vector containing PASS/FAIL observations 
#' of a child on one or more developmental milestones. Scores are coded 
#' numerically as \code{pass = 1} and \code{fail = 0}. 
#' Alternatively, \code{pass = TRUE} and \code{fail = FALSE} may be used. Alternatively, \code{scores} can be a \code{data.frame} 
#' containing columns named \code{items}, \code{scores} and \code{ages}.
#' @param items A character vector with item names in the chosen \code{lexicon}. 
#' The default is \code{names(scores)}.
#' @param ages Numeric vector with \code{length(scores)} elements, 
#' specifying decimal age in years. This information 
#' is used 1) to break up calculations into separate D-scores per age, 
#' and 2) to specify age-dependent priors. 
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
#' @param full A logical indicating to return the mean of the posterior (\code{full = FALSE}, the default) or the full posterior 
#' (\code{full = TRUE}).
#' @param dec Number of decimals of the EAP estimates. Default is 2.
#' @param \dots Additional parameters passed down to \code{gettau()} (e.g., 
#' \code{lexicon} or \code{itembank}) and \code{adp()} (e.g., \code{mu} 
#' \code{sd} or \code{reference}).
#' @return If \code{full} is \code{FALSE}: 
#' A vector of EAP estimates per age 
#' with \code{length(unique(age))} elements. If \code{full} is \code{TRUE}: 
#' A \code{list} of \code{length(unique(age))} elements with the
#' starting prior, posterior, quadrature grid and eap.
#' @references
#' Bock DD, Mislevy RJ (1982).  
#' Adaptive EAP Estimation of Ability in a Microcomputer Environment.
#' Applied Psychological Measurement, 6(4), 431-444.
#' 
#' Van Buuren S (2014). Growth charts of human development.
#' Stat Methods Med Res, 23(4), 346-368.
#' 
#' @author Stef van Buuren 2016
#' @seealso \code{\link{adp}}, \code{\link{gettau}}, 
#' \code{\link{itembank}}, \code{\link{posterior}},
#' \code{\link{Dreference}}
#' @examples 
#' # GSFIXEYE: Fixate eyes
#' # GSRSPCH:  Reacts to speech (M; can ask parents)
#' # GSMLEG :  Same amount of movement in both legs
#' items <- c("GSFIXEYE", "GSRSPCH", "GSMLEG")
#' gettau(items)
#' age <- round(rep(21/365.25, 3), 4)  # age 21 days
#' dscore(c(1, 0, 0), items, age)
#' 
#' # two time points, one additional (overlapping) item
#' items <- c(items, items[3])
#' age <- round(c(age, 42/365.25), 4)  # add age 42 days
#' dscore(c(1, 0, 0, 1), items, age)
#' 
#' # save full posterior
#' fp <- dscore(c(1, 1, 0, 1), items, age, full = TRUE)
#' plot(fp[[1]]$qp, fp[[1]]$posterior, type = "l",
#' xlab = "D-score", ylab = "Density", 
#' main = "Age 21 days: 2 PASS, FAIL at GSMLEG")
#' lines(fp[[1]]$qp, fp[[1]]$start, lty = 2)
#' 
#' # hardly any difference between prior and posterior 
#' # because PASS score is uninformative at age 42 days
#' plot(fp[[2]]$qp, fp[[2]]$posterior, type = "l",
#' xlab = "D-score", ylab = "Density", main = "Age 42 days: PASS at GSMLEG")
#' lines(fp[[2]]$qp, fp[[2]]$start, lty = 2)
#' 
#' # However a FAIL score signals substantial delay at age 42 days
#' fp <- dscore(c(1, 1, 0, 0), items, age, full = TRUE)
#' plot(fp[[2]]$qp, fp[[2]]$posterior, type = "l",
#' xlab = "D-score", ylab = "Density", main = "Age 42 days: FAIL at GSMLEG")
#' lines(fp[[2]]$qp, fp[[2]]$start, lty = 2)
#' 
#' @export
dscore <- function(scores, 
                   items = names(scores), 
                   ages,
                   qp = -10:100,
                   mem.between = 0,
                   mem.within = 1,
                   full = FALSE,
                   dec = 2,
                   ...) {
  
  # call dscore as vector
  if (is.data.frame(scores))
    return(dscore(scores = scores$scores, 
                  items = scores$items,
                  ages = scores$ages, 
                  ...))
  
  # check input length
  if (length(scores) != length(ages)) stop("Arguments `scores` and `ages` of different length")
  if (length(scores) != length(items)) stop("Arguments `scores` and `items` of different length")
  txxx <- qp
  
  # find the difficulty levels  
  tau <- gettau(items = items, ...)
  if (is.null(tau)) warning("No difficulty parameters found for ", 
                            head(items), ".")
  
  # split the data by age
  # if (all(is.na(ages))) stop("All ages are missing.")
  dg <- split(data.frame(scores = scores, 
                         ages = ages, 
                         items = items, 
                         tau = tau), f = ages)
  
  # create output array
  eap <- rep(NA, length = length(dg))
  post <- list(eap = NA, start = NULL, qp = NULL, posterior = NULL)
  fullpost <- rep(list(post), length = length(dg))
  names(fullpost) <- names(eap) <- names(dg)
  
  # iterate
  k <- 0                            # valid scores counter
  for (i in 1:length(dg)) {         # loop over unique ages
    dgi <- dg[[i]]
    cage <- dgi[1, "ages"]            # current age
    nextocc <- TRUE                 # flag for next occasion
    fullpost[[i]]$qp <- qp
    for (j in 1:nrow(dgi)) {          # loop over items
      score <- dgi[j, "scores"]       # observed score
      tau   <- dgi[j, "tau"]
      if (is.na(score) | is.na(cage) | is.na(tau)) next
      k <- k + 1                    # yes, we have a valid response
      
      # CASE A: k == 1: start with age-dependent prior for first valid score
      if (k == 1) {
        prior <- adp(age = cage, qp = qp, ...)
        fullpost[[i]]$start <- prior
        nextocc <- FALSE
      }
      
      # CASE B: nextocc is TRUE if this is the first valid response
      # at the present age. If so, weight the starting prior with 
      # 'previous occasion posterior' by mem.between
      else if (nextocc) {
        prior <- mem.between * post + 
          (1 - mem.between) * adp(age = cage, qp = qp, ...)
        prior <- normalize(prior, qp)
        fullpost[[i]]$start <- prior
        nextocc <- FALSE
      }
      
      # CASE C: weight 'previous score posterior' by mem.within
      else {
        prior <- mem.within * post + 
          (1 - mem.within) * adp(age = cage, qp = qp, ...)
        prior <- normalize(prior, qp)
      }
      
      # calculate posterior
      post <- posterior(score, tau, prior, qp)
      fullpost[[i]]$posterior <- post
      
      # overwrite old eap estimate by new one
      fullpost[[i]]$eap <- eap[i] <- weighted.mean(qp, w = post)
    }
  }
  if (!full) return(round(eap, dec))
  return(fullpost)
}


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
#' @param tau A vector of length \code{m} with uncentralized thresholds from the Rasch model
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
#' @export
posterior <- function(score, tau, prior, qp)
{
  m <- length(tau)
  score <- floor(score)
  if (score < 0 | score > m) stop("score out-of-range.")
  
  # compute category response probability under the 1PL (Rasch) model
  # for a vector of uncentralized threshold parameters tau
  cpc <- t(exp(outer(0:m, qp) + c(0, -cumsum(tau))))
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

#' Obtain difficulty parameters from item bank
#' 
#' Searches the item bank for matching items, and returns the 
#' difficulty estimates. Matching is done by item name. Comparisons
#' are done in lower case.
#' @aliases gettau
#' @param items A character vector with item names in the 
#' chosen lexicon.
#' @param itembank A \code{data.frame} that contains the item names 
#' (in various lexicons), the item label, the item difficulty parameter
#' \code{tau}. Lexicon column names start with \code{"lex."}. 
#' The default \code{dscore::itembank} contains the 
#' difficulty levels as published in Van Buuren (2014).
#' @param lexicon A character string indicating the column in 
#' \code{itembank} used to match item names. It must be one of 
#' the lexicon columns, e.g., \code{dutch1983}, 
#' \code{dutch1996}, \code{dutch2005}, \code{SMOCC} 
#' or \code{GHAP}. The default is \code{lexicon = "GHAP"}. 
#' @param check Logical that indicates whether the lexicon name
#' should be checked. The default is \code{TRUE}.
#' @param \dots Additional arguments (ignored).
#' @return A named vector with the difficulty estimate per item with
#' \code{length(items)} elements, or \code{NULL} if items are 
#' not found.
#' @author Stef van Buuren 2016
#' @seealso \code{\link{itembank}}, \code{\link{dscore}}
#' @examples 
#' # difficulty levels in default GHAP lexicon
#' gettau(items = c("GSFIXEYE", "GSMARMR"))
#' 
#' # difficulty levels of same items in the SMOCC lexicon
#' gettau(items = c("v1430", "v1432"), lexicon = "SMOCC")
#' 
#' @export
gettau <- function(items, 
                   itembank = dscore::itembank, 
                   lexicon = "GHAP", 
                   check = TRUE, 
                   ...) {
  # check whether lexicon is a column in item bank
  lex <- paste("lex", lexicon, sep = ".")
  if (check) {
    q <- pmatch(tolower(lex), tolower(names(itembank)))
    if (is.na(q)) stop ("Lexicon `", lexicon, "` not found in item bank.")
  }
  
  # find exact matching items rows
  p <- match(items, itembank[, lex])
  if (all(is.na(p))) return(NULL)
  r <- itembank[p, "tau"]
  names(r) <- items
  return(r)
}

#' Age-dependent prior
#'
#' Returns the age-dependent prior N(mu, 5) at the 
#' specified quadrature points.
#' @aliases adp
#' @param age Age in years. Numeric, single value
#' @param qp A number vector of equally spaced quadrature points.
#' This vector should span the range of all D-score values, and 
#' have at least 80 elements. The default is 
#' \code{qp = -10:100}, which is suitable for age range 0-4 years.
#' @param mu The mean of the prior. If \code{is.null(mu)} (the default) 
#' the prior is taken from the age-dependent reference. 
#' @param sd Standard deviation of the prior. The default is 5.
#' @param reference the LMS reference values. The default uses the 
#' built-in reference \code{dscore::Dreference} for Dutch children
#' published in Van Buuren (2014).
#' @param \dots Additional parameters (ignored)
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
#' plot(x = qp, y= adp(1/12, qp), type = "l", 
#'   main = "Priors at ages of 1, 12 and 24 months", 
#'   ylab = "Density", xlab = "D-score")
#' lines(x = qp, adp(1, qp), lty = 2)
#' lines(x = qp, adp(2, qp), lty = 3)
#' @export
adp <- function(age, qp = -10:100, mu = NULL, sd = 5, 
                reference = dscore::Dreference, ...) {
  if (is.null(mu)) mu <- approx(y = reference$mu, x = reference$year,
                                xout = round(age, 4), yleft = reference$mu[1])$y
  p <- dnorm(qp, mean = mu, sd = sd)
  return(normalize(p, qp))
}

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
#' normalize(c(5, 10, 5), qp = c(0, 1, 2))
#' normalize(c(1, 5, 8, 5, 1), qp = 1:5)
#' 
#' # the sum is always equal to 1
#' sum(normalize(rnorm(5), qp = 1:5))
#' @export
normalize <- function(d, qp) {
  if (length(d) != length(qp)) stop("Arguments `d` and  `qp` of different length")
  d <- d / sum(d)
  return(d / (qp[2] - qp[1]))
}

