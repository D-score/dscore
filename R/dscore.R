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
#' The method uses Bayes rule. The function can also return the 
#' full posterior instead of the EAP. See Bock and Mislevy (1982).
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
#' mimmicks repeated application of Bayesian theorem to the current data 
#' using 'old' data as prior.}
#' }
#' 
#' @aliases dscore 
#' @param scores A vector containing PASS/FAIL observations 
#' of a child on one or more developmental milestones. Scores are coded 
#' as numerically as \code{pass = 1} and \code{fail = 0}. 
#' Alternatively, \code{pass = TRUE} and \code{fail = FALSE} may be used.
#' @param age Numeric vector with \code{length(scores)} elements, 
#' specifying decimal age in years. This information 
#' is used 1) to break up calculations into separate D-scores per age, 
#' and 2) to specify age-dependent priors. 
#' @param items A character vector with item names in the chosen \code{lexicon}. 
#' The default is \code{names(scores)}.
#' @param theta A number vector of equally spaced quadrature points.
#' This vector should span the range of all D-score values, and 
#' have at least 80 elements.
#' The default \code{theta = -10:80} is suitable for children 
#' aged 0-2 years.
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
#' @param \dots Additional parameters passed down to \code{gettau()} (e.g., 
#' \code{lexicon} or \code{itembank}) and \code{adp()} (e.g., \code{mu} 
#' \code{sd} or \code{reference}).
#' @return If \code{full} is \code{FALSE}: 
#' A vector of EAP estimates per age 
#' with \code{length(unique(age))} elements. If \code{full} is \code{TRUE}: 
#' A \code{list} of \code{length(unique(age))} elements with the density estimate at each quadature point.
#' @references
#' Bock DD, Mislevy RJ (1982).  
#' Adaptive EAP Estimation of Ability in a Microcomputer Environment.
#' Applied Psychological Measurement, 6(4), 431-444.
#' 
#' Van Buuren S (2014). Growth charts of human development.
#' Stat Methods Med Res, 23(4), 346-368.
#' 
#' @author Stef van Buuren 2016
#' @export
# @param prior A function that produces a numerical vector with
# \code{length(theta)} elements that sum to unity. The default function
# \code{adp(age, theta)} produces a an age-dependent normal prior
# centered around the median D-score and a standard deviation equal
# to 5.
dscore <- function(scores, 
                   age,
                   items = names(scores), 
                   theta = -10:80,
                   mem.between = 0,
                   mem.within = 1,
                   full = FALSE, 
                   ...) {
  # check inputs
  # split into age groups
  # create output arrays
  # iterate
  # unsplit groups
  
  # check input lengths
  if (length(scores) != length(age)) stop("Arguments `scores` and `age` of different length")
  if (length(scores) != length(items)) stop("Arguments `scores` and `items` of different length")
  if (length(theta) != length(prior)) stop("Arguments `theta` and `prior` of different length")
  
  # find the difficulty levels  
  tau <- gettau(items = items, ...)
  if (all(is.na(tau))) warning("No difficulty parameters found for ", 
                               head(items), 
                               ". Consider changing the `lexicon` argument.")
  
  # split the data by age
  dg <- split(data.frame(scores, age, items, tau), f = age)
  
  # create output array
  eap <- rep(NA, length = length(dg))
  names(eap) <- names(dg)
  
  # iterate
  k <- 0                            # valid scores counter
  for (i in 1:length(dg)) {         # loop over unique ages
    d <- dg[[i]]
    cage <- d[1, "age"]             # current age
    nextocc <- TRUE                 # flag for next occasion
    for (j in 1:nrow(d)) {          # loop over items
      score <- d[j, "scores"]       # observed score
      tau   <- d[j, "tau"]
      if (is.na(score) | is.na(cage) | is.na(tau)) next
      k <- k + 1                    # yes, we have a valid response
      
      # CASE A: k == 1: start with age-dependent prior for first valid score
      if (k == 1) {
        prior <- adp(age = cage, theta = theta, ...)
        nextocc <- FALSE
      }
      
      # CASE B: nextocc is TRUE if this is the first valid response
      # at the present age. If so, weight the starting prior with 
      # 'previous occasion posterior' by mem.between
      else if (nextocc) {
        prior <- mem.between * post + 
          (1 - mem.between) * adp(age = cage, theta = theta, ...)
        nextocc <- FALSE
      }
      
      # CASE C: weight 'previous score posterior' by mem.within
      else prior <- mem.within * post + 
        (1 - mem.within) * adp(age = cage, theta = theta, ...)
      
      # rescale priors to proper density
      prior <- normalize(prior, theta)
      
      # calculate posterior
      post <- posterior(score, tau, prior, theta)
      
      # overwrite old eap estimate by new one
      eap[i] <- weighted.mean(theta, w = post)
    }
    
    return(eap)
  }
}


#' Calculate posterior for one item given score, difficulty and prior
#'
#' @details
#' This function assumes that the difficulties have been estimated by 
#' a polytomous Rasch model, e.g. by \code{RUMM2030} or 
#' \code{sirt::rasch.pairwise()}, and transformed onto the 
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
#' @param prior vector of prior values on quadrature points \code{theta}
#' @param theta vector of equally spaced quadrature points
#' @return A vector of length \code{length(prior)}
#' @author Stef van Buuren 2016
#' @references
#' Andrich D. Rasch Models for Measurement. Newbury Park: 
#' Sage Publications; 1988.
#' 
#' RUMM Laboratories. RUMM 2030. 
#' Rasch Unidimensional Measurement Models. Perth: 2015.
#' @export
posterior <- function(score, tau, prior, theta)
{
  m <- length(tau)
  score <- floor(score)
  if (score < 0 | score > m) stop("score out-of-range.")
  
  # compute category response probability under the 1PL (Rasch) model
  # for a vector of uncentralized threshold parameters tau
  cpc <- t(exp(outer(0:m, theta) + c(0, -cumsum(tau))))
  cpc <- cpc[,score + 1] / rowSums(cpc)
  cpc <- cpc / sum(cpc)
  
  # clean out any missing entries
  prior[is.na(prior)] <- 0
  
  # calculate the posterior per category
  postcat <- cpc * prior
  postcat <- normalize(postcat, theta)
  
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
#' the lexicon columns, e.g., \code{lex.dutch1983}, 
#' \code{lex.dutch1996}, \code{lex.dutch2005} or \code{lex.smocc}.
#' The default is \code{lex.smocc}. 
#' @param check Logical that indicates whether the lexicon name
#' should be checked. The default is \code{TRUE}.
#' @param \dots Additional arguments (ignored).
#' @return A named vector with the difficulty estimate per item with
#' \code{length(items)} elements.
#' @author Stef van Buuren 2016
#' @examples 
#' # difficulty levels in default lex.smocc lexicon
#' gettau(items = c("v1436", "v1437"))
#' 
#' # difficulty levels of same items in lex.dutch1996 lexicon
#' gettau(items = c("v55", "v29"), lexicon = "lex.dutch1996")
#' @export
gettau <- function(items, 
                   itembank = dscore::itembank, 
                   lexicon = "lex.smocc", 
                   check = TRUE, 
                   ...) {
  # check whether lexicon is a column in item bank
  if (check) {
    q <- pmatch(tolower(lexicon), names(itembank))
    if (is.na(q)) stop ("Lexicon name `", lexicon, "` not found in item bank.")
  }
  
  # find exact matching items rows
  p <- match(tolower(items), itembank[, lexicon])
  if (all(is.na(p))) return(rep(NA, length = length(items)))
  r <- itembank[p, "tau"]
  names(r) <- items
  return(r)
}

#' Age-dependent prior
#'
#' Returns the age-dependent prior N(mu, 5) at the 
#' specified quadrature points.
#' @aliases adp
#' @param age Numeric, single value
#' @param theta A number vector of equally spaced quadrature points
#' @param mu The mean of the prior. If \code{is.null(mu)} (the default) the prior is taken from the age-dependent reference. 
#' @param sd Standard deviation of the prior. The default is 5.
#' @param reference the LMS reference values. The default uses the 
#' built-in reference \code{dscore::Dreference} for Dutch children
#' published in Van Buuren (2014).
#' @param \dots Additional parameters (ignored)
#' @return  A \code{vector} of \code{length(theta)} elements with 
#' the prior density estimate at each quadature point \code{theta}.
#' @references
#' Van Buuren S (2014). Growth charts of human development.
#' Stat Methods Med Res, 23(4), 346-368.
#' @examples 
#' # define quadrature points for D-score
#' theta <- -10:80
#' 
#' # calculate and plot three priors
#' plot(x = theta, y= adp(1/12, theta), type = "l", 
#'   main = "Priors at ages of 1, 12 and 24 months", 
#'   ylab = "Density", xlab = "D-score")
#' lines(x = theta, adp(1, theta), lty = 2)
#' lines(x = theta, adp(2, theta), lty = 3)
#' @export
adp <- function(age, theta, mu = NULL, sd = 5, 
                reference = dscore::Dreference, ...) {
  if (is.null(mu)) mu <- approx(y = reference$mu, x = reference$year,
                                xout = round(age, 4), yleft = reference$mu[1])$y
  p <- dnorm(theta, mean = mu, sd = sd)
  return(normalize(p, theta))
}

#' Normalize distribution
#'
#' Normalizes the distribution so that the total mass equals 1.
#' @aliases normalize
#' @param d A vector with \code{length(theta)} elements representing
#' the unscaled density at each quadrature point.
#' @param theta Vector of equally spaced quadrature points.
#' @return A \code{vector} of \code{length(d)} elements with 
#' the prior density estimate at each quadature point.
#' @examples 
#' # simple normalization examples
#' normalize(c(5, 10, 5), theta = c(0, 1, 2))
#' normalize(c(1, 5, 8, 5, 1), theta = 1:5)
#' 
#' # the sum is always equal to 1
#' sum(normalize(rnorm(5), theta = 1:5))
#' @export
normalize <- function(d, theta) {
  if (length(d) != length(theta)) stop("Arguments `d` and  `theta` of different length")
  d <- d / sum(d)
  return(d / (theta[2] - theta[1]))
}

