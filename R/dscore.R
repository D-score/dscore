#' D-score estimation
#'
#' The `dscore()` function estimates the following quantities: *D-score*,
#' a numeric score that quantifies child development by one number,
#' *Development-for-Age Z-score (DAZ)* that corrects the D-score for age,
#' *standard error of measurement (SEM)* of the D-score.
#'
#' @param data  A `data.frame` or `matrix` with the data.
#' A row collects all observations made on a child on a set of
#' milestones administered at a given age. The function calculates
#' a D-score for each row. Different rows can correspond to different
#' children or ages.
#' @param items A character vector containing names of items to be
#' included into the D-score calculation. Milestone scores are coded
#' numerically as `1` (pass) and `0` (fail). By default,
#' D-score calculation is done on all items found in the data
#' that have a difficulty parameter under the specified `key`.
#' @param key String. They key identifies 1) the difficulty estimates
#' pertaining to a particular Rasch model, and 2) the prior mean and standard
#' deviation of the prior distribution for calculating the D-score.
#' The default key `NULL` sets `key = "gsed2406"`.
#' View `builtin_keys` for an overview of the available keys.
#' @param population String. The name of the reference population to calculate
#' DAZ.
#' Use `with(builtin_references, table(key, population))` to see which
#' built-in references are available for `key - population` combinations.
#' If not specified, the function set the default population as
#' `builtin_keys$base_population[key == builtin_keys$key]`.
#' @param itembank A `data.frame` with at least three columns named
#' `key`, `item` and `tau`. By default, the function uses
#' `dscore::builtin_itembank`. If you specify your own `itembank`,
#' then you should also provide the relevant `transform` and `qp` arguments.
#' @param xname A string with the name of the age variable in
#' `data`. The default is `"age"`. Do not round age.
#' @param xunit A string specifying the unit in which age is measured
#' (either `"decimal"`, `"days"` or `"months"`).
#' The default `"decimal"` corresponds to decimal age in years.
#' @param prepend Character vector with column names in `data` that will
#' be prepended to the returned data frame. This is useful for copying
#' columns from data into the result, e.g., for matching.
#' @param metric A string, either `"dscore"` (default) or
#' `"logit"`, signalling the metric in which ability is estimated.
#' `daz` is not calculated for the logit scale.
#' @param prior_mean `NULL` (default), a string, a numeric scalar, or
#' a numeric vector with  `nrow(data)` elements. The default value
#' `NULL` will consult the `base_population` field in `builtin_keys`,
#' and use the corresponding median of that reference as prior mean for
#' the D-score. The string should refer to a column name in `data`
#' that contains user-supplied values of the prior mean for each observation.
#' A numeric scalar will be expanded to all observations. A numeric vector
#' will be used as is.
#' @param prior_mean_NA `NULL` (default) or a scalar numeric, representing
#' the prior mean for observations with missing ages. By default, D-scores
#' with missing ages will we `NA`. We suggest setting
#' `prior_mean_NA = 50` as a reasonable choice for samples between 0-3
#' years. The argument is ignored if `prior_mean` is specified per
#' observation, which gives you full control of priors for observations
#' with missing ages.
#' @param prior_sd `NULL` (default), a string, a numeric scalar, or
#' a numeric vector with `nrow(data)` elements. The default (`NULL`)
#' uses a value of 5 for all ages. The string should refer to a column
#' name in `data` that contains user-supplied values of the prior sd
#' for each observation. A numeric scalar will be expanded to all
#' observations. A numeric vector will be used as is.
#' @param prior_sd_NA `NULL` (default) or a scalar numeric, representing
#' the prior sd for observations with missing ages. By default, D-scores
#' with missing ages will we `NA`. We suggest setting
#' `prior_sd_NA = 20` as a reasonable choice for samples between 0-3
#' years. The argument is ignored if `prior_sd` is specified per
#' observation, which gives you full control of priors for observations
#' with missing ages.
#' @param transform Numeric vector, length 2, containing the intercept
#' and slope of the linear transform from the logit scale into the
#' the D-score scale. The default (`NULL`) searches `builtin_keys`
#' for intercept and slope values.
#' @param qp Numeric vector of equally spaced quadrature points.
#' This vector should span the range of all D-score or logit values.
#' The default (`NULL`) creates `seq(from, to, by)` searching the
#' arguments from `builtin_keys`.
#' @param dec A vector of two integers specifying the number of
#' decimals for rounding the D-score and DAZ, respectively.
#' The default is `dec = c(2L, 3L)`.
#' @param relevance A numeric vector of length with the lower and
#' upper bounds of the relevance interval. The procedure calculates
#' a dynamic EAP for each item. If the difficulty level (tau) of the
#' next item is outside the relevance interval around EAP, the procedure
#' ignore the score on the item. The default is `c(-Inf, +Inf)` does not
#' ignore scores.
#' @param algorithm Computational method, for backward compatibility.
#' Either `"current"` (default) or `"1.8.7"` (deprecated).
#' @param verbose Logical. Print settings.
#'
#' @return The `dscore()` function returns a `data.frame` with `nrow(data)` rows.
#' Optionally, the first block of columns can be copied to the
#' result by using `prepend`. The second block consists of the
#' following columns:
#'
#' Name | Label
#' ---  | ---------
#' `a`  | Decimal age (years)
#' `n`  | Number of items with valid (0/1) data
#' `p`  | Percentage of passed milestones
#' `d`  | D-score, mean of posterior distribution
#' `sem` | Standard error of measurement, standard deviation of the posterior
#' `daz` | D-score corrected for age, calculated in Z-scale (for metric `"dscore"`)
#'
#' The D-score in column `d` is a linear scale, with values usually ranging
#' from 0 to 100. The D-score is `NA` if age is missing or if age is lower
#' than -1/12. It is possible to calculate D-scores for cases with missing ages
#' by setting `prior_mean_NA` and `prior_sd_NA` to some reasonable value, e.g.,
#' `prior_mean_NA = 50` and `prior_sd_NA = 20`, for the sample at hand.
#'
#' The SEM is a positive number that quantifies the uncertainty of the D-score.
#' It is `NA` if the D-score is `NA`.
#'
#' The DAZ in column `daz` is a Z-score that corrects the D-score for age. It
#' is `NA` when there are no reference values for the given age, or when
#' the D-score is extremely unlikely to be valid at the given age.
#'
#' Advanced applications: The `dscore_posterior()` function returns a
#' data frame with `nrow(data)` rows and `length(qp)` plus prepended columns
#' with the full posterior density of the D-score at each quadrature point.
#' If no valid responses are found, `dscore_posterior()` returns the
#' prior density. Versions prior to 1.8.5 returned a `matrix` (instead of
#' a `data.frame`). Code that depends on the result being a `matrix` may break
#' and may need adaptation.
#'
#' @details
#' The scoring algorithm is based on the method by Bock and Mislevy (1982).
#' The method uses Bayes rule to update a prior ability into a posterior
#' ability.
#'
#' The item names should correspond to the `"gsed"` lexicon.
#'
#' A key is defined by the set of estimated item difficulties.
#'
#' Key | Model | Quadrature | Instruments | Direct/Caregiver | Reference
#' --- | -----:| ----------:| ----------: |:----------------:|:---------
#' `"dutch"` | `75_0`   | `-10:80`  | 1   | direct | Van Buuren, 2014/2020
#' `"gcdg"`  | `565_18` | `-10:100` | 13  | direct | Weber, 2019
#' `"gsed1912"`  | `807_17` | `-10:100` | 21  | mixed  | GSED Team, 2019
#' `"293_0"`     | `293_0` | `-10:100` | 2   | mixed  | GSED Team, 2022
#' `"gsed2212"`  | `818_6` | `-10:100` | 27  | mixed  | GSED Team, 2022
#' `"gsed2406"`  | `818_6` | `-10:100` | 27  | mixed  | GSED Team, 2024
#'
#' As a general rule, one should only compare D-scores
#' that are calculated using the same key and the same
#' set of quadrature points. For calculating D-scores on new data,
#' the advice is to use the default, which currently is `"gsed2406"`.
#'
#' The default starting prior is a mean calculated from a so-called
#' "Count model" that describes mean D-score as a function of age. The
#' The Count models are implemented in the function `[get_mu()]`.
#' By default, the spread of the starting prior
#' is 5 D-score points around the mean D-score, which corresponds to
#' approximately 1.5 to 2 times the normal spread of child of a given age. The
#' starting prior is informative for very short test (say <5 items), but has
#' little impact on the posterior for larger tests.
#'
#' @references
#' Bock DD, Mislevy RJ (1982).
#' Adaptive EAP Estimation of Ability in a Microcomputer Environment.
#' Applied Psychological Measurement, 6(4), 431-444.
#'
#' Van Buuren S (2014). Growth charts of human development.
#' Stat Methods Med Res, 23(4), 346-368.
#' <https://stefvanbuuren.name/publication/van-buuren-2014-gc/>
#'
#' Weber AM, Rubio-Codina M, Walker SP, van Buuren S, Eekhout I,
#' Grantham-McGregor S, Caridad Araujo M, Chang SM, Fernald LCH,
#' Hamadani JD, Hanlon A, Karam SM, Lozoff B, Ratsifandrihamanana L,
#' Richter L, Black MM (2019). The D-score: a metric for interpreting
#' the early development of infants and toddlers across global settings.
#' BMJ Global Health, BMJ Global Health 4: e001724.
#' <https://gh.bmj.com/content/bmjgh/4/6/e001724.full.pdf>
#'
#' @author Stef van Buuren, Iris Eekhout, Arjan Huizing (2022)
#' @seealso [builtin_keys()], [builtin_itembank()], [builtin_itemtable()],
#' [builtin_references()], [get_tau()], [posterior()], [milestones()]
#' @examples
#' # using all defaults and properly formatted data
#' ds <- dscore(milestones)
#' head(ds)
#'
#' # step-by-step example
#' data <- data.frame(
#'   id = c(
#'     "Jane", "Martin", "ID-3", "No. 4", "Five", "6",
#'     NA_character_, as.character(8:10)
#'   ),
#'   age = rep(round(21 / 365.25, 4), 10),
#'   ddifmd001 = c(NA, NA, 0, 0, 0, 1, 0, 1, 1, 1),
#'   ddicmm029 = c(NA, NA, NA, 0, 1, 0, 1, 0, 1, 1),
#'   ddigmd053 = c(NA, 0, 0, 1, 0, 0, 1, 1, 0, 1)
#' )
#' items <- names(data)[3:5]
#'
#' # third item is not part of the default key
#' get_tau(items, verbose = TRUE)
#'
#' # calculate D-score
#' dscore(data)
#'
#' # prepend id variable to output
#' dscore(data, prepend = "id")
#'
#' # or prepend all data
#' # dscore(data, prepend = colnames(data))
#'
#' # calculate full posterior
#' p <- dscore_posterior(data)
#'
#' # check that rows sum to 1
#' rowSums(p)
#'
#' # plot full posterior for measurement 7
#' barplot(as.matrix(p[7, 12:36]),
#'   names = 1:25,
#'   xlab = "D-score", ylab = "Density", col = "grey",
#'   main = "Full D-score posterior for measurement in row 7",
#'   sub = "D-score (EAP) = 11.58, SEM = 3.99")
#'
#' # plot P10, P50 and P90 of D-score references
#' g <- expand.grid(age = seq(0.1, 4, 0.1), p = c(0.1, 0.5, 0.9))
#' d <- zad(z = qnorm(g$p), x = g$age, verbose = TRUE)
#' matplot(
#'   x = matrix(g$age, ncol = 3), y = matrix(d, ncol = 3), type = "l",
#'   lty = 1, col = "blue", xlab = "Age (years)", ylab = "D-score",
#'   main = "D-score preliminary standards: P10, P50 and P90")
#' abline(h = seq(10, 80, 10), v = seq(0, 4, 0.5), col = "gray", lty = 2)
#'
#' # add measurements made on very preterms, ga < 32 weeks
#' ds <- dscore(milestones)
#' points(x = ds$a, y = ds$d, pch = 19, col = "red")
#' @export
dscore <- function(data,
                   items = names(data),
                   key = NULL,
                   population = NULL,
                   xname = "age",
                   xunit = c("decimal", "days", "months"),
                   prepend = NULL,
                   itembank = NULL,
                   metric = c("dscore", "logit"),
                   prior_mean = NULL,
                   prior_mean_NA = NULL,
                   prior_sd = NULL,
                   prior_sd_NA = NULL,
                   transform = NULL,
                   qp = NULL,
                   dec = c(2L, 3L),
                   relevance = c(-Inf, Inf),
                   algorithm = c("current", "1.8.7"),
                   verbose = FALSE) {
  xunit <- match.arg(xunit)
  metric <- match.arg(metric)
  algorithm <- match.arg(algorithm)
  data <- as.data.frame(data)

  calc_dscore(
    data = data, items = items, key = key, population = population,
    xname = xname, xunit = xunit, prepend = prepend,
    itembank = itembank, metric = metric,
    prior_mean = prior_mean, prior_mean_NA = prior_mean_NA,
    prior_sd = prior_sd, prior_sd_NA = prior_sd_NA,
    transform = transform, qp = qp, dec = dec,
    posterior = FALSE,
    relevance = relevance,
    algorithm = algorithm,
    verbose = verbose
  )
}

#' The `dscore_posterior()` function returns the full posterior
#' distribution of the D-score.
#' @rdname dscore
#' @export
dscore_posterior <- function(data,
                             items = names(data),
                             key = NULL,
                             population = NULL,
                             xname = "age",
                             xunit = c("decimal", "days", "months"),
                             prepend = NULL,
                             itembank = NULL,
                             metric = c("dscore", "logit"),
                             prior_mean = NULL,
                             prior_mean_NA = NULL,
                             prior_sd = NULL,
                             prior_sd_NA = NULL,
                             transform = NULL,
                             qp = NULL,
                             dec = c(2L, 3L),
                             relevance = c(-Inf, Inf),
                             algorithm = c("current", "1.8.7"),
                             verbose = FALSE) {
  xunit <- match.arg(xunit)
  metric <- match.arg(metric)
  algorithm <- match.arg(algorithm)
  data <- as.data.frame(data)

  calc_dscore(
    data = data, items = items, key = key, population = population,
    xname = xname, xunit = xunit, prepend = prepend,
    itembank = itembank, metric = metric,
    prior_mean = prior_mean, prior_mean_NA = prior_mean_NA,
    prior_sd = prior_sd, prior_sd_NA = prior_sd_NA,
    transform = transform, qp = qp, dec = dec,
    posterior = TRUE,
    relevance = relevance,
    algorithm = algorithm,
    verbose = verbose
  )
}

calc_dscore <- function(data, items, key, population,
                        xname, xunit, prepend,
                        itembank, metric,
                        prior_mean, prior_mean_NA,
                        prior_sd, prior_sd_NA,
                        transform, qp, dec,
                        posterior,
                        relevance,
                        algorithm,
                        verbose) {
  stopifnot(length(relevance) == 2L)

  init <- init_key(key, population, transform, qp)
  key <- init$key
  population <- init$population
  transform <- init$transform
  qp <- init$qp

  if (verbose) {
    cat("key:        ", key, "\n")
    cat("population: ", population, "\n")
    cat("transform:  ", transform, "\n")
    cat("qp range:   ", range(qp), "\n")
    cat("algorithm:  ", algorithm, "\n")
  }

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

  # check the itembank
  if (is.null(itembank)) {
    itembank <- dscore::builtin_itembank
  } else {
    if (!all(c("key", "item", "tau") %in% colnames(itembank))) {
      stop("itembank must have columns 'key', 'item' and 'tau'")
    }
  }

  # obtain difficulty estimates
  ib <- data.frame(
    item = items,
    tau = get_tau(items = items, key = key, itembank = itembank),
    stringsAsFactors = FALSE
  )

  # subset items
  items <- items[!is.na(ib$tau)]
  items <- intersect(items, names(data))

  # handle case where not a single tau is found
  if (length(items) == 0L) {
    return(
      data.frame(
        a = a,
        n = 0L,
        p = NA_real_,
        d = NA_real_,
        sem = NA_real_,
        daz = NA_real_
      )
    )
  }

  # initialize prior mean mu and standard deviation sd
  mu <- init_mu(data, key, a, prior_mean, prior_mean_NA)
  sd <- init_sd(data, key, a, prior_sd, prior_sd_NA)

  # In D-score scale, set scale expansion
  scale <- switch(algorithm,
                  "current" = transform[2L],
                  "1.8.7" = 1,
                  transform[2L]
  )

  # setup for logit scale
  if (metric == "logit") {
    ib$tau <- (ib$tau - transform[1L]) / transform[2L]
    qp <- (qp - transform[1L]) / transform[2L]
    mu <- (mu - transform[1L]) / transform[2L]
    sd <- sd / transform[2L]
    scale <- switch(algorithm,
                    "current" = 1,
                    "1.8.7" = 1 / transform[2],
                    1
    )
  }

  # bind difficulty estimates to data
  data2 <- data |>
    mutate(
      a = a,
      mu = mu,
      sd = sd,
      .rownum = 1L:n()
    ) |>
    select(all_of(c(".rownum", "a", "mu", "sd", items))) |>
    pivot_longer(
      cols = all_of(items), names_to = "item",
      values_to = "score", values_drop_na = TRUE
    ) |>
    arrange(.data$.rownum, .data$item) |>
    left_join(ib, by = "item")

  # if dscore_posterior() was called
  if (posterior) {
    data3 <- data2 |>
      group_by(.data$.rownum) |>
      summarise(
        w = list(calculate_posterior(
          scores = .data$score,
          tau = .data$tau,
          qp = qp,
          scale = scale[1L],
          mu = (.data$mu)[1L],
          sd = (.data$sd)[1L],
          relhi = relevance[2L],
          rello = relevance[1L]
        )$posterior)
      )

    # unlist the posterior and store in proper row
    # return prior if calculate_posterior returned NULL
    data4 <- matrix(NA, nrow = nrow(data), ncol = length(qp))
    colnames(data4) <- paste("qp", as.character(qp), sep = "_")
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
    data5 <- as.data.frame(data4)
  }

  # if dscore() was called
  if (!posterior) {
    # summarise n, p, d and sem
    data3 <- data2 |>
      group_by(.data$.rownum, .data$a) |>
      summarise(
        n = n(),
        p = round(mean(.data$score), digits = 4L),
        x = list(qp),
        w = list(calculate_posterior(
          scores = .data$score,
          tau = .data$tau,
          qp = qp,
          scale = scale[1L],
          mu = (.data$mu)[1L],
          sd = (.data$sd)[1L],
          relhi = relevance[2L],
          rello = relevance[1L]
        )$posterior)
      )

    data4 <- data3 |>
      group_by(.data$.rownum, .data$a, .data$n, .data$p) |>
      summarise(
        d = weighted.mean(x = unlist(.data$x), w = unlist(.data$w)),
        sem = sqrt(sum(unlist(.data$w) * (unlist(.data$x) - unlist(.data$d))^2))
      )

    # add n and d
    data5 <- data.frame(.rownum = seq_len(nrow(data))) |>
      left_join(data4, by = ".rownum") |>
      mutate(
        n = recode(.data$n, .missing = 0L),
        d = round(.data$d, digits = dec[1L]))

    # add n and d daz, shape end result
    reference_table <- get_reference(population = population, key = key)
    if (nrow(reference_table)) {
      data5$daz <- daz(
        d = data5$d, x = data5$a,
        reference_table = reference_table,
        dec = dec[2L])
    } else {
      data5$daz = NA_real_
    }
    data5 <- select(data5, all_of(c("a", "n", "p", "d", "sem", "daz")))
    data5$sem[is.nan(data5$sem)] <- NA_real_
  }

  # prepend administrative variables from data
  nfo <- setdiff(prepend, colnames(data))
  if (length(nfo)) {
    warning("Not found: ",
            paste(nfo, collapse = ", "),
            call. = FALSE
    )
  }
  adm <- intersect(colnames(data), prepend)
  dup <- intersect(colnames(data5), adm)
  if (length(dup)) {
    warning("Overwrites column(s): ",
            paste(dup, collapse = ", "),
            call. = FALSE
    )
    adm <- setdiff(adm, dup)
  }
  if (length(adm)) {
    data5 <- data.frame(data[, adm, drop = FALSE], data5)
  }

  return(data5)
}
