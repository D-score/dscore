#' D-score estimation
#' 
#' This function estimates the D-score, a numeric score that measures
#' child development, from PASS/FAIL observations on milestones. 
#' 
#' @param data  A \code{data.frame} with the data. 
#' A row collects all observations made on a child on a set of 
#' milestones administered at a given age. The function calculates 
#' a D-score for each row. Different rows correspond to different
#' children or different ages.
#' @param items A character vector containing names of items to be 
#' included into the D-score calculation. Milestone scores are coded 
#' numerically as \code{1} (pass) and \code{0} (fail). By default, 
#' D-score calculation is done on all items found in the data 
#' that have a difficulty parameter under the specified \code{key}.
#' @param xname A string with the name of the age variable in 
#' \code{data}. The default is \code{"age"}.
#' @param xunit A string specifying the unit in which age is measured 
#' (either \code{"decimal"}, \code{"days"} or \code{"months"}).
#' The default (\code{"decimal"}) means decimal age in years.
#' @param key A string that sets the key, the set of difficulty 
#' estimates from a fitted Rasch model.
#' The built-in keys are: \code{"gsed"} (default), \code{"gcdg"}, 
#' and \code{"dutch"}. Use \code{key = ""} to use all item names, 
#' which should only be done if there are no duplicate itemnames.
#' @param itembank A \code{data.frame} with columns
#' \code{key}, \code{item}, \code{tau}, \code{instrument}, \code{domain}, 
#' \code{mode}, \code{number} and \code{label}. Only columns \code{item}
#' and \code{tau} are required. 
#' The function uses \code{dscore::builtin_itembank} by 
#' default.
#' @param metric A string, either \code{"dscore"} (default) or 
#' \code{"logit"}, signalling the metric in which ability is estimated.
#' @param prior_mean A string specifying a column name in \code{data} 
#' with the mean of the prior for the D-score calculation. 
#' The default \code{prior_mean = ".gcdg"} calculates an age-dependent 
#' prior mean internally according to function 
#' \code{dscore:::count_mu_gcdg()}.
#' The choice \code{prior_mean = ".dutch"} 
#' calculates \code{prior_mean} from the Count model coded in 
#' \code{dscore:::count_mu_dutch()}).
#' @param prior_sd A string specifying a column name in \code{data} 
#' with the standard deviation of the prior for the D-score calculation. 
#' If not specified, the standard deviation is taken as 5.
#' @param transform Vector of length 2, signalling the intercept 
#' and slope respectively of the linear transform that converts an 
#' observation in the logit scale to the the D-score scale. Only 
#' needed if \code{metric == "logit"}.
#' @param qp Numeric vector of equally spaced quadrature points.
#' This vector should span the range of all D-score values. The default
#' (\code{qp = -10:100}) is suitable for age range 0-4 years.
#' @param population A string describing the population. Currently 
#' supported are \code{"dutch"} and \code{"gcdg"} (default).
#' @param dec Integer specifying the number of decimals for 
#' rounding the ability estimates and the DAZ. The default is 
#' \code{dec = 3}.
#' @return 
#' A \code{data.frame} with \code{nrow(data)} rows and the following 
#' columns:
#' \describe{
#' \item{\code{a}}{Decimal age}
#' \item{\code{n}}{Number of items with valid (0/1) data}
#' \item{\code{p}}{Percentage of passed milestones}
#' \item{\code{d}}{Ability estimate, mean of posterior}
#' \item{\code{sem}}{Standard error of measurement, standard deviation of the posterior}
#' \item{\code{daz}}{D-score corrected for age, calculated in D-score metric}
#' }
#' 
#' @details
#' The algorithm is based on the method by Bock and Mislevy (1982). The 
#' method uses Bayes rule to update a prior ability into a posterior
#' ability. 
#' 
#' The item names should correspond to the \code{"gsed"} lexicon.
#' 
#' The built-in itembank (object \code{builtin_keys}) supports 
#' keys \code{"gsed"} (default), \code{"gcdg"} and \code{"dutch"}. 
#' A key is defined by the set of estimated item difficulties.
#' 
#' \tabular{llcccl}{
#'   Key \tab Model \tab Quadrature \tab Instruments \tab Direct/Caregiver \tab Reference\cr
#'   \cr
#'   gsed  \tab \code{807_17} \tab -10:100 \tab 20  \tab mixed  \tab GSED Team, 2019\cr
#'   gcdg  \tab \code{565_18} \tab -10:100 \tab 14  \tab direct \tab Weber, 2019\cr
#'   dutch \tab \code{75_0}   \tab -10:80  \tab 1   \tab direct \tab Van Buuren, 2014/2019
#' }
#' 
#' As a general rule, one should only compare D-scores 
#' that are calculated using the same key and the same
#' set of quadrature points. For calculating D-scores on new data, 
#' the advice is to use the most general key: (\code{gsed}).
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
#' GSED Team (2019).
#' \href{https://earlychildhoodmatters.online/2019/the-global-scale-for-early-development-gsed/?ecm2019}{The Global Scale for Early Development (GSED)}. 
#' Early Childhood Matters 2019-14, 80-84.
#' 
#' Van Buuren S (2014).
#' \href{https://stefvanbuuren.name/publication/2014-01-01_vanbuuren2014gc/}{Growth charts of human development}.
#' Stat Methods Med Res, 23(4), 346-368.
#' 
#' Van Buuren S, Dusseldorp E, Doove B (2017).
#' D-scores and references for ages 2-4 years. The Netherlands. 
#' In preparation, 2017-11-27.
#' 
#' Weber AM, Rubio-Codina M, Walker SP, van Buuren S, Eekhout I, 
#' Grantham-McGregor S, Caridad Araujo M, Chang SM, Fernald LCH, 
#' Hamadani JD, Hanlon A, Karam SM, Lozoff B, Ratsifandrihamanana L, 
#' Richter L, Black MM (2019). The D-score: a metric for interpreting 
#' the early development of infants and toddlers across global settings. 
#' BMJ Global Health, accepted for publication.
#' 
#' @author Stef van Buuren, Iris Eekhout, Arjan Huizing (2019)
#' @seealso \code{\link{get_tau}}, 
#' \code{\link{builtin_itembank}}, \code{\link{posterior}},
#' \code{\link{builtin_references}}
#' @examples 
#' \dontrun{
#'data <- ddata::get_gcdg(study="Netherlands 1", adm=TRUE)  
#'data$age <- data$age/12    
#'items <- dmetric::prepare_items(study="Netherlands 1")$items
#'dscore(data=data, items=items, itembank=gcdg_itembank)
#'}
#' @export
dscore <- function(data, 
                   items = names(data),
                   xname = "age", 
                   xunit = c("decimal", "days", "months"),
                   key = "gsed",
                   itembank = dscore::builtin_itembank,
                   metric = c("dscore", "logit"),
                   prior_mean = ".gcdg",
                   prior_sd = NULL,
                   transform = NULL,
                   qp = -10:100,
                   population = key,
                   dec = 3L) {
  
  xunit   <- match.arg(xunit)
  metric  <- match.arg(metric)
  
  # get decimal age
  if (!xname %in% names(data)) stop("Variable", xname, "not found")
  a <- switch(xunit,
              decimal = round(data[[xname]], 3L),
              months  = round(data[[xname]] / 12, 3L),
              days    = round(data[[xname]] / 365.25, 3L),
              rep(NA, nrow(data)))
  
  # obtain difficulty estimates
  ib <- data.frame(
    item = items,
    tau = get_tau(items = items, key = key, itembank = itembank),
    stringsAsFactors = FALSE)
  
  # subset items
  items <- intersect(items, names(data))
  items <- items[!is.na(ib$tau)]
  
  # handle case where not a single tau is found
  if (length(items) == 0L) return(
    data.frame(a = a,
               n = 0L,
               p = NA,
               d = NA,
               sem = NA,
               daz = NA))
  
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
    ib$tau <- (ib$tau - transform[1L]) / transform[2L]
    qp <- (qp - transform[1L]) / transform[2L]
    mu <- (mu - transform[1L]) / transform[2L]
    sd <- sd / transform[2L]
  }
  
  # bind difficulty estimates to data
  data2 <- data %>%
    bind_cols(a = a) %>% 
    mutate(mu = mu, 
           sd = sd,
           .rownum = 1L:n()) %>%
    select(.data$.rownum, .data$a, .data$mu, .data$sd, items) %>% 
    gather(key = "item", value = "score", items, na.rm = TRUE) %>%
    arrange(.data$.rownum, .data$item) %>%
    left_join(ib, by = "item")
  
  # summarise n, p, d and sem
  data3 <- data2 %>%
    group_by(.data$.rownum, .data$a) %>%
    summarise(
      n = n(),
      p = round(mean(.data$score), digits = dec),
      x = list(qp),
      w = list(calculate_posterior(scores = .data$score, 
                                   tau = .data$tau, 
                                   qp  = qp,
                                   mu  = (.data$mu)[1],
                                   sd  = (.data$sd)[1])$posterior)) %>% 
    unnest(cols = c("x", "w")) %>%
    group_by(.data$.rownum, .data$a, .data$n, .data$p) %>% 
    summarise(
      d = weighted.mean(x = .data$x, w = .data$w),
      sem = sqrt(sum(.data$w * (.data$x - .data$d)^2))
    )
  
  # add daz, shape end result
  data.frame(.rownum = 1L:nrow(data)) %>%
    left_join(data3, by = ".rownum") %>%
    mutate(n = recode(.data$n, .missing = 0L),
           daz = daz(d = .data$d, x = .data$a, 
                     reference = get_reference(population),
                     dec = dec),
           daz = ifelse(is.nan(.data$daz), NA, .data$daz),
           d = round(.data$d, digits = dec)) %>% 
    select(.data$a, .data$n, .data$p, .data$d, .data$sem, .data$daz)
}
