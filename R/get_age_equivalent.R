#' Get age equivalents of items that have a difficulty estimate
#'
#' This function calculates the ages at which a certain percent
#' in the reference population passes the items.
#' @inheritParams dscore
#' @param pct Numeric vector with requested percentiles (0-100). The
#' default is `pct = c(10, 50, 90)`.
#' @param key A string that sets the key, the set of difficulty
#' estimates from a fitted Rasch model.
#' The built-in keys are: `"gsed"` (default), `"gcdg"`,
#' and `"dutch"`.
#' @param itembank A `data.frame` with columns named `key`, `item`
#' and `tau`. The function uses `dscore::builtin_itembank` by
#' default.
#' @return Tibble with four columns: `item`, `d` (*D*-score),
#' `pct` (percentile), and `a` (age-equivalent, in `xunit` units).
#' @examples
#' get_age_equivalent(c("ddicmm030", "ddicmm050"), key = "dutch")
#' @export
get_age_equivalent <- function(items,
                               pct = c(10, 50, 90),
                               key = "gsed",
                               itembank = dscore::builtin_itembank,
                               population = key,
                               xunit = c("decimal", "days", "months")) {
  xunit <- match.arg(xunit)

  # obtain difficulty estimates
  ib <- tibble(
    item = items,
    d = get_tau(items = items, key = key, itembank = itembank))

  # get reference
  reference <- get_reference(population)

  # calculate age-equivalent percentiles
  ib <- ib %>%
    slice(rep(seq_along(items), each = length(pct))) %>%
    mutate(
      pct = rep(pct, length(items)),
      d = .data$d + qlogis(.data$pct / 100),
      a = approx(x = reference$mu, y = reference$age, xout = .data$d)$y
    )

  # convert to requested age unit
  if (xunit == "days") ib$a <- round(ib$a * 365.25)
  if (xunit == "months") ib$a <- round(ib$a * 12, 4L)

  ib
}
