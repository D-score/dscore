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
#' @param data A data.frame containing columns names \code{items} with PASS/FAIL observations. 
#' Scores are coded numerically as \code{pass = 1} and \code{fail = 0}. 
#' Alternatively, \code{pass = TRUE} and \code{fail = FALSE} may be used. Additionally,
#' a column named \code{age} specifying decimal age in years.
#' @param items A character vector with item names in the chosen \code{lexicon}. 
#' The default is \code{names(scores)}.
#' @param age A character with the name of the age variable 
#' specifying decimal age in years. This information 
#' is used 1) to break up calculations into separate D-scores per age, 
#' and 2) to specify age-dependent priors. 
#' @param key A data.frame of two columns containing the \code{items} names and a second column 
#' named \code{delta} containing the item difficulties estimated from the Rasch model in the 
#' prefferred metric/scale. 
#' @param metric A string, either \code{"dscore"} or \code{"logit"}, 
#' signalling the metric in which the ability are estimated
#' @param full DOCUMENTATIONNEEDED
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
#' @details 
#'  param prior: The mean of the prior. If \code{mu = "gcdg"} (the default)
#' then \code{mu} is calculated from the Count model coded in 
#' \code{dscore:::count_mu_gcdg()}. Specify \code{mu = "reference"} in order
#' to take it from the age-dependent reference (default < 0.22).
#' @seealso \code{\link{adp}}, \code{\link{gettau}}, 
#' \code{\link{itembank}}, \code{\link{posterior}},
#' \code{\link{Dreference}}
#' @examples 
#' \dontrun{
#'data <- ddata::get_gcdg(study="Netherlands 1", adm=TRUE)  
#'data$age <- data$age/12    
#'items <- dmetric::prepare_items(study="Netherlands 1")$items
#'dscore(data=data, items=items,lexicon="gcdg", itembank=gcdg_itembank)
#'}
#' @export
dscore <- function(data, 
                   items, 
                   age = "age", 
                   key = NULL, 
                   metric = "dscore",
                   full = FALSE,
                   dec = 2,
                   ...) {
  
  if (is.null(key))
    key <- data.frame(item = items,
                      delta = gettau(items = items, ...),
                      stringsAsFactors = FALSE)
  # check input length
  if (!age %in% names(data))  stop("Age not found in data")
  if (!any(items %in% names(data))) stop("Item names are not present in the data")
  if (!any(names(data) %in% key[, 1])) stop("Items are not present in the key")

  data2 <- data %>%
    mutate(.rownum = 1:n()) %>%
    select(.rownum, age, items) %>% 
    gather(key = item, value = score, items, na.rm = TRUE) %>%
    arrange(.rownum, item) %>%
    left_join(key, by = "item")
  
  # only return eap in frame
  if (!full) {
    eap <- data2 %>%
      group_by(.rownum, age) %>%
    summarise(n = n(),
              b = round(calculate_posterior(scores = score, delta = delta, age = age, metric=metric,...)$eap, dec)) %>%
      ungroup() 
    
    data3 <- data.frame(.rownum = 1:nrow(data)) %>%
      left_join(eap, by = ".rownum") %>%
      mutate(n = recode(n, .missing = 0L)) %>%
      select(n, b)
    return(data3)
  }
  
  # return full posterior and eap as list
  data2s <- split(data2, data2$.rownum)
  post <- lapply(data2s, 
                 function(x) {
                   calculate_posterior(scores = x$score, delta = x$delta, age = x$age, metric=metric, ...)})
  return(post)
}
