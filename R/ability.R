#' Ability estimation
#' 
#' This function estimates the ability, a numerical score that measures
#' generic abilty, from PASS/FAIL observations (items). 
#' 
#' @details By default, the algorithm produces a list containing the ful positior
#' and the expected a posteriori (EAP) estimate from \code{scores} using item 
#' difficulty as obtained from the item bank, and an age-dependent
#' prior of about twice the normal variation at the given age.
#' The method uses Bayes rule. See Bock and Mislevy (1982) for more 
#' details on the EAP and its variance. 
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
#' @aliases ability 
#' @param data A data.frame containing columns names \code{items} with PASS/FAIL observations 
#' Scores are coded numerically as \code{pass = 1} and \code{fail = 0}. 
#' Alternatively, \code{pass = TRUE} and \code{fail = FALSE} may be used. Additionally,
#' a column named \code{age} specifying decimal age in years.
#' @param items A character vector with item names in the chosen \code{lexicon}. 
#' The default is \code{names(scores)}.
#' @param key A data.frame of two columns containing the \code{items} names and a second column 
#' named \code{delta} containing the item difficulties estimated from the Rasch model in the 
#' prefferred metric/scale. 
#' @param age A character with the name of the age variable 
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
#' @param dec Number of decimals of the EAP estimates. Default is 2.
#' @param id DOCUMENTATION NEEDED
#' @param pdist DOCUMENTATION NEEDED
#' @param full DOCUMENTATIONNEEDED
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
#' data <- ddata::get_gcdg(study = "Netherlands 1", adm = TRUE)
#' items <- ddata::item_names(study = "Netherlands 1")
#' delta <- dscore::gettau(items = items, lexicon = "gcdg")
#' key <- data.frame(items = items, delta = delta)
#' data$age <- data$age/12
#' ability(data = data, items = items, age = "age", key = key, prior = "gcdg")
#' 
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
ability <- function(data, 
                   items, 
                   age = "age", id = "id",
                   key = NULL, 
                   qp = -10:100,
                   pdist = "dutch",
                   mem.between = 0,
                   mem.within = 1,
                   full = FALSE,
                   dec = 2,
                   ...) {
  
  if (is.null(key))
    key <- data.frame(items = items,
                      delta = gettau(items = items, lexicon = "gcdg"))
  # check input length
  if (!age %in% names(data))  stop("Age variable is not present in the data")
  if (!any(items %in% names(data))) stop("Item names are not present in the data")
  if (!any(names(data) %in% key[,1])) stop("Items are not present in the key")
  if(is.null(id)){data$id <- 1:nrow(data)} 


  #function to calculate posterior (was loop in original dscore)
  calculate_posterior <- function(scores,delta,age, qp = qp, mu=pdist,...){
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
  
  #only return eap in frame
  if (!full){
  eap <- data%>% select(id,age,items) %>% #drop extra vars
    gather(key=items, value=scores,items, na.rm = TRUE) %>%
    arrange(id,age,items)%>%
    left_join(key,by="items") %>%
    group_by(id,age)%>%
    summarise(ability = calculate_posterior(scores=scores,delta=delta,age=age, qp=qp)$eap) %>%
    ungroup()
  
  return(eap)
  }
  
  #return full output (full posterior eap etc) in list
  data$row <- 1:nrow(data)
  data2 <- data%>% select(row,id,age,items) %>% #drop extra vars
    gather(key=items, value=scores,items, na.rm = TRUE) %>%
    arrange(row,id,age,items)%>%
    left_join(key,by="items") 
  data2s <- split(data2, data2$row)
  fullpost <- lapply(data2s, function(x){calculate_posterior(scores=x$scores, delta=x$delta, age=x$age, qp=qp)})
  
  return(fullpost)
}




