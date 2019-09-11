#' D-score estimation
#' 
#' This function estimates the D-score, a numeric score that measures
#' child development, from PASS/FAIL observations on milestones. 
#' 
#' @param data  A \code{tbl_df} or \code{data.frame} with the data. 
#' A row corresponds to an observation of a child at a given age.
#' @param items A character vector containing names of items to be 
#' included into the D-score calculation. Milestone scores are coded 
#' numerically as \code{1} (pass) and \code{0} (fail). By default, 
#' all items in the data are included that 1) appear in the specified 
#' lexicon, and 2) that have a difficulty parameter under the specified 
#' \code{model}.
#' @param id Name of the variable with a child id. If not specified, 
#' all rows in \code{data} are expected to apply to the same child.
#' @param age A character vector of length 2. The first element is 
#' the name of the age variable in \code{data}. The second element 
#' specifies the unit in which age is measured (either \code{"decimal"}, 
#' \code{"days"} or \code{"months"}). The default is 
#' \code{c("age", "decimal")}.
#' @param lexicon Item naming scheme. Can be one of 
#' \code{"gsed"} (default), \code{"gcdg"}, \code{"ghap"}, \code{dutch1983}, 
#' \code{dutch1996}, \code{"dutch2005"} or \code{"smocc"}.
#' @param model The model from which the difficulty estimates are taken.
#' Currently available are models \code{"807_17"} (default), \code{"565_18"}, 
#' and \code{"75_0"} and \code{"57_0"}. See details.
#' @param itembank A \code{data.frame} that contains the item names 
#' (in various lexicons), the item label, and the item difficulty 
#' parameters \code{tau} (under various models). 
#' Lexicon column names start with \code{"lex."}. 
#' The default uses the built-in \code{dscore::itembank} object.
#' @param metric A string, either \code{"dscore"} (default) or 
#' \code{"logit"}, signalling the metric in which ability is estimated.
#' See details.
#' @param prior_mean A string specifying a column name in \code{data} 
#' with the mean of the prior for the D-score calculation. 
#' The default \code{prior_mean = NULL} calculates an age-dependent 
#' prior mean internally calculated according to function 
#' \code{dscore:::count_mu_gcdg()}.
#' reference. The choice \code{prior_mean = ".dutch"} calculates
#' \code{prior_mean} from the Count model coded in 
#' \code{dscore:::count_mu_dutch()}).
#' @param prior_sd A string specifying a column name in \code{data} 
#' with the standard deviation of the prior for the D-score calculation. 
#' If not specified, the standard deviation is taken as 5.
#' @param transform Vector of length 2, signalling the intercept 
#' and slope of the linear transform from the D-score scale to the 
#' logit scale. Only needed if \code{metric == "logit"}.
#' @param qp A number vector of equally spaced quadrature points.
#' This vector should span the range of all D-score values. The default
#' (\code{qp = -10:100}) is suitable for age range 0-4 years.
#' @param mem_between Fraction of posterior from previous occasion 
#' relative to age-dependent prior. The 
#' value \code{mem_between = 0} (the default) means that 
#' no smoothing over age is performed, while a \code{mem_between = 1} 
#' corresponds to maximal smoothing over age. See details.
#' @param mem_within Fraction of posterior from previous item within
#' the same age relative to age-dependent prior. The 
#' value \code{mem_within = 1} (the default) means that all items 
#' count equally in the posterior, while a \code{mem_within = 0} 
#' corresponds to counting only the last item. See details.
#' @param full A logical indicating to return the mean of the 
#' posterior (\code{full = FALSE}, the default) or the full posterior 
#' (\code{full = TRUE}).
#' @param dec Integer vector of length 2 specifying the number of 
#' decimals of the ability and DAZ. Default is \code{dec = c(3L, 3L)}.
#' @return The return result depends on the \code{full} parameter.
#' If \code{full == FALSE}, a \code{data.frame} with \code{nrow(data)}
#' rows and the following columns:
#' \describe{
#' \item{\code{n_found}}{Number of items found in the lexicon}
#' \item{\code{n_valid}}{Number of items with valid (0/1) data}
#' \item{\code{p_pass}}{Percentage of passed milestones}
#' \item{\code{d}}{Ability estimate, mean of posterior, in scale that 
#' conforms to \code{metric} parameter}
#' \item{\code{sd}}{Standard deviation of the posterior, in scale that 
#' conforms to \code{metric} parameter}
#' \item{\code{daz}}{D-score corrected for age, calculated in D-score metric}
#' }
#' If \code{full == TRUE}, a \code{data.frame} with \code{nrow(data)}
#' rows and the following columns:
#' \describe{
#' \item{\code{n_found}}{Number of items found in the lexicon}
#' \item{\code{n_valid}}{Number of items with valid (0/1) data}
#' \item{\code{p_pass}}{Percentage of passed milestones}
#' \item{\code{d_x}}{A series of columns labeled \code{d_x}, where \code{x}
#' corresponds to the quadrature points specific in parameter \code{qp}}
#' }
#' 
#' @details
#' The algorithm is based on the method Bock and Mislevy (1982). The 
#' method uses Bayes rule to update a prior ability into a posterior
#' ability. 
#' 
#' The default starting prior is a mean calculated from a so-called 
#' "Count model" that describes mean D-score as a function of age. The
#' Count models are stored as internal functions 
#' \code{dscore:::count_mu_gcdg()} (default) and 
#' \code{dscore:::count_mu_dutch()}. The spread of the starting prior
#' is 5 D-score points around this mean D-score, which corresponds to 
#' approximately twice the normal spread of child of a given age. The
#' starting prior is thus somewhat informative for low numbers of 
#' valid items, and unformative for large number of items (say >10 items).
#' 
#' The mixing coefficients \code{mem_between} and \code{mem_within} are 
#' 0-1 numbers (fractions) that regulate the influence 
#' of the previous posterior on the prior. 
#' There are three cases, depending on whether it's the 
#' first age and first item:
#' \describe{
#' \item{First age, first item:}{For the first item at the first age
#' \code{t}, the starting prior as defined above is used.}
#' \item{Later age, first item:}{For the first item at the second or 
#' later age \code{t}, the prior is set to 
#' \code{mem_between * post + (1 - mem_between) * adp()}, 
#' where \code{post} is the D-score posterior as calculated from the 
#' previous age, and where \code{adp()} is the age-dependent prior at
#' age \code{t}. The mixing coefficient \code{mem_between = 0} by default, which
#' implies that D-scores of the same child at different ages are calculated 
#' independently.}
#' \item{Any age, later item:}{For the second or further items at any 
#' given age \code{t}, the prior
#' is set to \code{mem_within * post + (1 - mem_within) * adp()}, where 
#' \code{post} is the D-score posterior as calculated from the 
#' previous item within age \code{t}, and 
#' where \code{adp()} is the age-dependent prior at age \code{t}. 
#' By default the mixing coefficient \code{mem_within = 1}, which
#' mimmicks repeated application of Bayesian rule to the current data 
#' using 'old' data as prior.}
#' }
#' @references
#' Bock DD, Mislevy RJ (1982).  
#' Adaptive EAP Estimation of Ability in a Microcomputer Environment.
#' Applied Psychological Measurement, 6(4), 431-444.
#' 
#' Van Buuren S (2014). Growth charts of human development.
#' Stat Methods Med Res, 23(4), 346-368.
#' @author Stef van Buuren, Iris Eekhout 2019
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
                   items = names(data),
                   id = NULL,
                   age = c("age", "decimal"),
                   lexicon = c("gsed", "gcdg", "ghap", "dutch1983", 
                               "dutch1996", "dutch2005", "smocc"),
                   model = c("807_17", "565_18", "75_0", "57_0"), 
                   itembank = dscore::itembank,
                   metric = c("dscore", "logit"),
                   prior_mean = NULL,
                   prior_sd = NULL,
                   transform = NULL,
                   qp = -10:100,
                   mem_between = 0,
                   mem_within = 1,
                   full = FALSE,
                   dec = c(3L, 3L)) {
  
  lexicon <- match.arg(lexicon)
  model   <- match.arg(model)
  metric  <- match.arg(metric)
  
  # get decimal age
  if (length(age) != 2L) stop("Argument `age` not of length 2.")
  agename <- age[1L]
  if (!agename %in% names(data))  stop("Variable", agename, "not found")
  a <- switch(age[2L],
              decimal = data[, agename],
              months  = data[, agename] / 12,
              days    = data[, agename] / 365.25,
              rep(NA, nrow(data)))
  
  # obtain difficulty estimates
  key <- data.frame(
    item = items,
    delta = gettau(items = items, itembank = itembank, lexicon = lexicon),
    stringsAsFactors = FALSE)
  if (metric == "logit") key$delta <- (key$delta - transform[1]) / transform[2]
  
  # subset items
  items <- intersect(items, names(data))
  items <- items[!is.na(key$delta)]
  
  data2 <- data %>%
    bind_cols(a = a) %>% 
    mutate(.rownum = 1:n()) %>%
    select(.data$.rownum, .data$a, items) %>% 
    gather(key = "item", value = "score", items, na.rm = TRUE) %>%
    arrange(.data$.rownum, .data$item) %>%
    left_join(key, by = "item")
  
  # only return eap in frame
  if (!full) {
    eap <- data2 %>%
      group_by(.data$.rownum, .data$a) %>%
      summarise(n = n(),
                b = round(calculate_posterior(scores = .data$score, 
                                              delta = .data$delta, 
                                              age = .data$a, 
                                              metric = metric)$eap, 
                          digits = dec[1L])) %>%
      ungroup() 
    
    data3 <- data.frame(.rownum = 1:nrow(data)) %>%
      left_join(eap, by = ".rownum") %>%
      mutate(n = recode(n, .missing = 0L)) %>%
      select(.data$n, .data$b)
    return(data3)
  }
  
  # return full posterior and eap as list
  data2s <- split(data2, data2$.rownum)
  post <- lapply(data2s, 
                 function(x) {
                    calculate_posterior(scores = x$score, 
                                             delta = x$delta, 
                                             age = x$a, 
                                             metric = metric)})
  return(post)
}
