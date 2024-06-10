#' Get age equivalents of items that have a difficulty estimate
#'
#' This function calculates the ages at which a certain percent
#' in the reference population passes the items.
#'
#' The function internally defines a scale factor given the key.
#'
#' @inheritParams dscore
#' @param pct Numeric vector with requested percentiles (0-100). The
#' default is `pct = c(10, 50, 90)`.
#' @inheritParams dscore
#' @return Tibble with four columns: `item`, `d` (D-score),
#' `pct` (percentile), and `a` (age-equivalent, in `xunit` units).
#' @examples
#' get_age_equivalent(c("gpagmc018", "gtogmd026", "ddicmm050"))
#' @export
get_age_equivalent <- function(items,
                               pct = c(10, 50, 90),
                               key = NULL,
                               transform = NULL,
                               qp = NULL,
                               itembank = dscore::builtin_itembank,
                               population = NULL,
                               xunit = c("decimal", "days", "months")) {
  xunit <- match.arg(xunit)

  init <- init_key(key, transform, qp)
  key <- init$key
  transform <- init$transform

  # set default reference population for DAZ
  if (is.null(population)) {
    if (key %in% c("gsed2212", "gsed2208", "293_0")) {
      population <- "phase1"
    }
    if (key %in% c("gcdg", "gsed1912", "gsed2206", "lf2206", "sf2206", "294_0")) {
      population <- "gcdg"
    }
    if (key %in% c("dutch")) {
      population <- "dutch"
    }
    if (is.null(population)) {
      population <- "phase1"
      warning("Could not set population argument. Uses phase1.")
    }
  }

  # obtain difficulty estimates
  ib <- tibble(
    item = items,
    d = get_tau(items = items, key = key, itembank = itembank)
  )

  # get reference
  reference <- get_reference(population)

  # calculate age-equivalent percentiles
  ib <- ib |>
    slice(rep(seq_along(items), each = length(pct))) |>
    mutate(
      pct = rep(pct, length(items)),
      d = .data$d + qlogis(.data$pct / 100, scale = transform[2]),
      a = approx(x = reference$mu, y = reference$age, xout = .data$d)$y
    )

  # convert to requested age unit
  if (xunit == "days") ib$a <- round(ib$a * 365.25)
  if (xunit == "months") ib$a <- round(ib$a * 12, 4L)

  ib
}
