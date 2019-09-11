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
#' @param xname A string with the name of the age variable in 
#' \code{data}. The default is \code{"age"}.
#' @param xunit A string specifying the unit in which age is measured 
#' (either \code{"decimal"}, \code{"days"} or \code{"months"}). 
#' The default (\code{"decimal"}) means decimal age in years.
#' @param lexicon Item naming scheme. The built-in itembank supports
#' lexicons \code{"gsed"} (default), \code{"gcdg"}, \code{"ghap"}, 
#' \code{"dutch1983"}, \code{"dutch1996"}, \code{"dutch2005"} 
#' and \code{"smocc"}.
#' @param model The model from which the difficulty estimates are taken.
#' The built-in itembank supports models \code{"807_17"} (default), 
#' \code{"565_18"}, \code{"75_0"} and \code{"57_0"}. See details.
#' @param itembank A \code{data.frame} that contains the item names 
#' (in various lexicons), the item label, and the item difficulty 
#' parameters \code{tau} (under various models). 
#' Lexicon column names start with \code{"lex_"}. 
#' The default uses the built-in \code{dscore::itembank} object.
#' @param metric A string, either \code{"dscore"} (default) or 
#' \code{"logit"}, signalling the metric in which ability is estimated.
#' See details.
#' @param prior_mean A string specifying a column name in \code{data} 
#' with the mean of the prior for the D-score calculation. 
#' The default \code{prior_mean = ".gcdg"} calculates an age-dependent 
#' prior mean internally according to function 
#' \code{dscore:::count_mu_gcdg()}.
#' reference. The alternative choice \code{prior_mean = ".dutch"} 
#' calculates \code{prior_mean} from the Count model coded in 
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
                   xname = "age", 
                   xunit = c("decimal", "days", "months"),
                   lexicon = "gsed",
                   model = "807_17", 
                   itembank = dscore::itembank,
                   metric = c("dscore", "logit"),
                   prior_mean = ".gcdg",
                   prior_sd = NULL,
                   transform = NULL,
                   qp = -10:100,
                   full = FALSE,
                   dec = c(3L, 3L)) {
  
  xunit   <- match.arg(xunit)
  metric  <- match.arg(metric)
  
  # get decimal age
  if (!xname %in% names(data))  stop("Variable", xname, "not found")
  a <- switch(xunit,
              decimal = round(data[, xname], 3),
              months  = round(data[, xname] / 12, 3),
              days    = round(data[, xname] / 365.25, 3),
              rep(NA, nrow(data)))
  
  # obtain difficulty estimates
  key <- data.frame(
    item = items,
    delta = gettau(items = items, lexicon = lexicon, itembank = itembank),
    stringsAsFactors = FALSE)
  
  # subset items
  items <- intersect(items, names(data))
  items <- items[!is.na(key$delta)]
  
  # determine mu for the prior
  mu <- rep(NA, nrow(data))
  if (prior_mean == ".gcdg") mu <- count_mu_gcdg(a)
  else if (prior_mean == ".dutch") mu <- count_mu_dutch(a)
  else if (prior_mean %in% names(data)) mu <- data[, prior_mean]
  
  # determine sd for the prior
  sd <- rep(5, nrow(data))
  if (is.character(prior_sd) && prior_sd %in% names(data)) sd <- data[, prior_sd]
  
  # determine transform if needed
  if (is.null(transform) && metric == "logit") {
    if (prior_mean == ".gcdg")  transform <- c(66.174355, 2.073871)
    if (prior_mean == ".dutch") transform <- c(38.906, 2.1044)  # van buuren 2014
  }
  
  # setup for logit scale
  if (metric == "logit") {
    key$delta <- (key$delta - transform[1]) / transform[2]
    qp <- (qp - transform[1]) / transform[2]
    mu <- (mu - transform[1]) / transform[2]
    sd <- sd / transform[2]
  }
  
  # bind difficulty estimates to data
  data2 <- data %>%
    bind_cols(a = a) %>% 
    mutate(mu = mu, 
           sd = sd,
           .rownum = 1:n()) %>%
    select(.data$.rownum, .data$a, .data$mu, .data$sd, items) %>% 
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
                                              qp  = qp,
                                              mu  = .data$mu,
                                              sd  = .data$sd)$eap, 
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
                                       mu = x$mu,
                                       sd = x$sd)})
  return(post)
}
