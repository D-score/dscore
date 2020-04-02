#' D-score estimation
#'
#' The function `dscore()` function estimates the D-score,
#' a numeric score that measures child development, from PASS/FAIL
#' observations on milestones.
#'
#' @rdname dscore
#' @param data  A `data.frame` with the data.
#' A row collects all observations made on a child on a set of
#' milestones administered at a given age. The function calculates
#' a D-score for each row. Different rows correspond to different
#' children or different ages.
#' @param items A character vector containing names of items to be
#' included into the D-score calculation. Milestone scores are coded
#' numerically as `1` (pass) and `0` (fail). By default,
#' D-score calculation is done on all items found in the data
#' that have a difficulty parameter under the specified `key`.
#' @param xname A string with the name of the age variable in
#' `data`. The default is `"age"`.
#' @param xunit A string specifying the unit in which age is measured
#' (either `"decimal"`, `"days"` or `"months"`).
#' The default (`"decimal"`) means decimal age in years.
#' @param key A string that sets the key, the set of difficulty
#' estimates from a fitted Rasch model.
#' The built-in keys are: `"gsed"` (default), `"gcdg"`,
#' and `"dutch"`. Use `key = ""` to use all item names,
#' which should only be done if there are no duplicate itemnames.
#' @param itembank A `data.frame` with columns
#' `key`, `item`, `tau`, `instrument`, `domain`,
#' `mode`, `number` and `label`. Only columns `item`
#' and `tau` are required.
#' The function uses `dscore::builtin_itembank` by
#' default.
#' @param metric A string, either `"dscore"` (default) or
#' `"logit"`, signalling the metric in which ability is estimated.
#' @param prior_mean A string specifying a column name in `data`
#' with the mean of the prior for the D-score calculation.
#' The default depends on the `key`. If `key == "dutch"` then
#' `prior_mean = "dutch"`, else it is `".gcdg"`. These settings
#' calculate an age-dependent prior mean internally according to function
#' `dscore:::count_mu_gcdg()`.
#' The choice `prior_mean = ".dutch"`
#' calculates `prior_mean` from the Count model coded in
#' `dscore:::count_mu_dutch()`).
#' @param prior_sd A string specifying a column name in `data`
#' with the standard deviation of the prior for the D-score calculation.
#' If not specified, the standard deviation is taken as 5.
#' @param transform Vector of length 2, signalling the intercept
#' and slope respectively of the linear transform that converts an
#' observation in the logit scale to the the D-score scale. Only
#' needed if `metric == "logit"`.
#' @param qp Numeric vector of equally spaced quadrature points.
#' This vector should span the range of all D-score values. The default
#' (`qp = -10:100`) is suitable for age range 0-4 years.
#' @param population A string describing the population. Currently
#' supported are `"dutch"` and `"gcdg"` (default).
#' @param dec A vector of two integers specifying the number of
#' decimals for rounding the D-score and DAZ, respectively.
#' The default is `dec = c(2L, 3L)`.
#' @return
#' The `dscore()` function returns a `data.frame` with
#' `nrow(data)` rows and the following columns:
#'
#' Name | Label
#' ---  | ---------
#' `a`  | Decimal age
#' `n`  | Number of items with valid (0/1) data
#' `p`  | Percentage of passed milestones
#' `d`  | Ability estimate, mean of posterior
#' `sem` | Standard error of measurement, standard deviation of the posterior
#' `daz` | D-score corrected for age, calculated in Z-scale
#'
#' The `dscore_posterior()` function returns a numeric matrix with
#' `nrow(data)` rows and `length(qp)` columns with the
#' density at each quadrature point. The vector represents the full
#' posterior ability distribution. If no valid responses were obtained,
#' `dscore_posterior()` returns the prior.
#'
#' @details
#' The algorithm is based on the method by Bock and Mislevy (1982). The
#' method uses Bayes rule to update a prior ability into a posterior
#' ability.
#'
#' The item names should correspond to the `"gsed"` lexicon.
#'
#' The built-in itembank (object [builtin_itembank()]) supports
#' keys `"gsed"` (default), `"gcdg"` and `"dutch"`.
#' A key is defined by the set of estimated item difficulties.
#'
#' Key | Model | Quadrature | Instruments | Direct/Caregiver | Reference
#' --- | -----:| ----------:| ----------: |:----------------:|:---------
#' `"dutch"` | `75_0`   | `-10:80`  | 1   | direct | Van Buuren, 2014/2020
#' `"gcdg"`  | `565_18` | `-10:100` | 14  | direct | Weber, 2019
#' `"gsed"`  | `807_17` | `-10:100` | 20  | mixed  | GSED Team, 2019
#'
#' As a general rule, one should only compare D-scores
#' that are calculated using the same key and the same
#' set of quadrature points. For calculating D-scores on new data,
#' the advice is to use the most general key, `"gsed"`.
#'
#' The default starting prior is a mean calculated from a so-called
#' "Count model" that describes mean D-score as a function of age. The
#' Count models are stored as internal functions
#' `dscore:::count_mu_gcdg()` (default) and
#' `dscore:::count_mu_dutch()`. The spread of the starting prior
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
#' [pdf](https://stefvanbuuren.name/publications/2014\%20Growth\%20charts\%20for\%20development\%20-\%20SMMR.pdf)
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
#' BMJ Global Health, BMJ Global Health 4: e001724.
#' [pdf](https://gh.bmj.com/content/bmjgh/4/6/e001724.full.pdf).
#'
#' @author Stef van Buuren, Iris Eekhout, Arjan Huizing (2020)
#' @seealso [get_tau()],
#' [builtin_itembank()], [posterior()],
#' [builtin_references()]
#' @examples
#' data <- data.frame(
#'   age = rep(round(21 / 365.25, 4), 10),
#'   ddifmd001 = c(NA, NA, 0, 0, 0, 1, 0, 1, 1, 1),
#'   ddicmm029 = c(NA, NA, NA, 0, 1, 0, 1, 0, 1, 1),
#'   ddigmd053 = c(NA, 0, 0, 1, 0, 0, 1, 1, 0, 1)
#' )
#' items <- names(data)[2:4]
#'
#' # third item is not part of default key
#' get_tau(items)
#'
#' # calculate D-score
#' dscore(data)
#'
#' # calculate full posterior
#' p <- dscore_posterior(data)
#'
#' # plot posterior for row 7
#' plot(x = -10:100, y = p[7, ], type = "l", xlab = "D-score",
#'  ylab = "Density", xlim = c(0, 30))
#' @export
dscore <- function(data,
                   items = names(data),
                   xname = "age",
                   xunit = c("decimal", "days", "months"),
                   key = "gsed",
                   itembank = dscore::builtin_itembank,
                   metric = c("dscore", "logit"),
                   prior_mean = ifelse(key == "dutch", ".dutch", ".gcdg"),
                   prior_sd = NULL,
                   transform = NULL,
                   qp = -10:100,
                   population = key,
                   dec = c(2L, 3L)) {
  xunit <- match.arg(xunit)
  metric <- match.arg(metric)
  calc_dscore(
    data = data, items = items, xname = xname, xunit = xunit,
    key = key, itembank = itembank, metric = metric,
    prior_mean = prior_mean, prior_sd = prior_sd,
    transform = transform, qp = qp,
    population = population, dec = dec,
    posterior = FALSE
  )
}

#' The `dscore_posterior()` function returns the full posterior
#' distribution of the D-score.
#' @rdname dscore
#' @export
dscore_posterior <- function(data,
                             items = names(data),
                             xname = "age",
                             xunit = c("decimal", "days", "months"),
                             key = "gsed",
                             itembank = dscore::builtin_itembank,
                             metric = c("dscore", "logit"),
                             prior_mean = ifelse(key == "dutch", ".dutch", ".gcdg"),
                             prior_sd = NULL,
                             transform = NULL,
                             qp = -10:100,
                             population = key,
                             dec = c(2L, 3L)) {
  xunit <- match.arg(xunit)
  metric <- match.arg(metric)
  calc_dscore(
    data = data, items = items, xname = xname, xunit = xunit,
    key = key, itembank = itembank, metric = metric,
    prior_mean = prior_mean, prior_sd = prior_sd,
    transform = transform, qp = qp,
    population = population, dec = dec,
    posterior = TRUE
  )
}

calc_dscore <- function(data, items, xname, xunit,
                        key, itembank, metric,
                        prior_mean, prior_sd,
                        transform, qp,
                        population, dec,
                        posterior) {
  # handle zero rows
  if (nrow(data) == 0L) {
    return(
      data.frame(
        a = numeric(0),
        n = integer(0),
        p = numeric(0),
        d = numeric(0),
        sem = numeric(0),
        daz = numeric(0)
      )
    )
  }

  # get decimal age
  if (!xname %in% names(data)) stop("Variable `", xname, "` not found")
  a <- switch(xunit,
              decimal = round(data[[xname]], 4L),
              months  = round(data[[xname]] / 12, 4L),
              days    = round(data[[xname]] / 365.25, 4L),
              rep(NA, nrow(data))
  )

  # obtain difficulty estimates
  ib <- data.frame(
    item = items,
    tau = get_tau(items = items, key = key, itembank = itembank),
    stringsAsFactors = FALSE
  )

  # subset items
  items <- intersect(items, names(data))
  items <- items[!is.na(ib$tau)]

  # handle case where not a single tau is found
  if (length(items) == 0L) {
    return(
      data.frame(
        a = a,
        n = 0L,
        p = NA,
        d = NA,
        sem = NA,
        daz = NA
      )
    )
  }

  # determine mu for the prior
  mu <- rep(NA, nrow(data))
  if (prior_mean == ".gcdg") {
    mu <- count_mu_gcdg(a)
  } else if (prior_mean == ".dutch") {
    mu <- count_mu_dutch(a)
  } else if (prior_mean %in% names(data)) mu <- data[, prior_mean]

  # determine sd for the prior
  sd <- rep(5, nrow(data))
  if (is.character(prior_sd) && prior_sd %in% names(data))
    sd <- data[, prior_sd]

  # determine transform if needed
  if (is.null(transform) && metric == "logit") {
    if (prior_mean == ".gcdg") transform <- c(66.174355, 2.073871)
    if (prior_mean == ".dutch") transform <- c(38.906, 2.1044) # van buuren 2014
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
    mutate(
      mu = mu,
      sd = sd,
      .rownum = 1L:n()
    ) %>%
    select(.data$.rownum, .data$a, .data$mu, .data$sd, items) %>%
    pivot_longer(
      cols = items, names_to = "item",
      values_to = "score", values_drop_na = TRUE
    ) %>%
    arrange(.data$.rownum, .data$item) %>%
    left_join(ib, by = "item")

  # if dscore_posterior() was called
  if (posterior) {
    data3 <- data2 %>%
      group_by(.data$.rownum) %>%
      summarise(
        w = list(calculate_posterior(
          scores = .data$score,
          tau = .data$tau,
          qp = qp,
          mu = (.data$mu)[1L],
          sd = (.data$sd)[1L]
        )$posterior)
      )

    # unlist the posterior and store in proper row
    # return prior if calculate_posterior returned NULL
    data4 <- matrix(NA, nrow = nrow(data), ncol = length(qp))
    for (i in seq_len(nrow(data4))) {
      idx <- data3[, ".rownum", drop = TRUE] == i
      f <- unlist(data3[idx, "w"])
      if (!is.null(f)) {
        data4[i, ] <- f
      } else {
        data4[i, ] <- dnorm(qp,
                            mean = as.double(data2[i, "mu"]),
                            sd = as.double(data2[i, "sd"])
        )
      }
    }
    return(data4)
  }

  # if dscore() was called
  if (!posterior) {
    # summarise n, p, d and sem
    data3 <- data2 %>%
      group_by(.data$.rownum, .data$a) %>%
      summarise(
        n = n(),
        p = round(mean(.data$score), digits = 4L),
        x = list(qp),
        w = list(calculate_posterior(
          scores = .data$score,
          tau = .data$tau,
          qp = qp,
          mu = (.data$mu)[1L],
          sd = (.data$sd)[1L]
        )$posterior)
      )

    data4 <- data3 %>%
      group_by(.data$.rownum, .data$a, .data$n, .data$p) %>%
      summarise(
        d = weighted.mean(x = unlist(.data$x), w = unlist(.data$w)),
        sem = sqrt(sum(unlist(.data$w) * (unlist(.data$x) - unlist(.data$d))^2))
      )

    # add daz, shape end result
    data5 <- data.frame(.rownum = seq_len(nrow(data))) %>%
      left_join(data4, by = ".rownum") %>%
      mutate(
        n = recode(.data$n, .missing = 0L),
        d = round(.data$d, digits = dec[1L]),
        daz = daz(
          d = .data$d, x = .data$a,
          reference = get_reference(population),
          dec = dec[2L]),
        daz = ifelse(is.nan(.data$daz), NA, .data$daz)
      ) %>%
      select(.data$a, .data$n, .data$p, .data$d, .data$sem, .data$daz)
    return(data5)
  }
}
