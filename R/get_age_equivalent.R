#' Get age equivalents of items that have a difficulty estimate
#'
#' This function calculates the ages at which a certain percent
#' in the reference population passes the items.
#'
#' @note
#' The function internally defines a scale factor given the key.
#'
#' @inheritParams dscore
#' @param pct Numeric vector with requested percentiles (0-100). The
#' default is `pct = c(10, 50, 90)`.
#' @inheritParams dscore
#' @return `data.frame` with four columns: `item`, `d` (D-score),
#' `pct` (percentile), and `a` (age-equivalent, in `xunit` units).
#' @examples
#' get_age_equivalent(c("gpagmc018", "gtogmd026", "ddicmm050"))
#' @export
get_age_equivalent <- function(items,
                               pct = c(10, 50, 90),
                               key = NULL,
                               population = NULL,
                               transform = NULL,
                               itembank = dscore::builtin_itembank,
                               xunit = c("decimal", "days", "months"),
                               verbose = FALSE) {
  xunit <- match.arg(xunit)

  init <- init_key(key, population, transform, qp = NULL)
  key <- init$key
  population <- init$population
  transform <- init$transform

  if (verbose) {
    cat("key:        ", key, "\n")
    cat("population: ", population, "\n")
    cat("transform:  ", transform, "\n")
  }

  # obtain difficulty estimates
  ib <- data.frame(
    item = items,
    d = get_tau(items = items, key = key, itembank = itembank)
  )

  # get reference
  rt <- get_reference(population = population, key = key)

  # calculate age-equivalent percentiles
  ib <- ib |>
    slice(rep(seq_along(items), each = length(pct))) |>
    mutate(
      pct = rep(pct, length(items)),
      d = .data$d + qlogis(.data$pct / 100, scale = transform[2]),
      a = approx(x = rt$mu, y = rt$age, xout = .data$d)$y
    )

  # convert to requested age unit
  if (xunit == "days") ib$a <- round(ib$a * 365.25)
  if (xunit == "months") ib$a <- round(ib$a * 12, 4L)

  return(ib)
}
