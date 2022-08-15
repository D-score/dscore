# Two functions adapted from gamlss.dist to work better with out-of-range data
#
# To cite package ‘gamlss.dist’ in publications use:
# Stasinopoulos M, Rigby R (2022). _gamlss.dist: Distributions for
# Generalized Additive Models for Location Scale and Shape_. R package version 6.0-3,
# <https://CRAN.R-project.org/package=gamlss.dist>.
#
# @Manual{,
#   title = {gamlss.dist: Distributions for Generalized Additive Models for Location Scale
#     and Shape},
#   author = {Mikis Stasinopoulos and Robert Rigby},
#   year = {2022},
#   note = {R package version 6.0-3},
#   url = {https://CRAN.R-project.org/package=gamlss.dist}}

qBCT <- function (p, mu = 5, sigma = 0.1, nu = 1, tau = 2, lower.tail = TRUE,
                  log.p = FALSE, na.rm = TRUE)
{
  if (any(mu < 0, na.rm = na.rm))
    stop(paste("mu must be positive", "\n", ""))
  if (any(sigma < 0, na.rm = na.rm))
    stop(paste("sigma must be positive", "\n", ""))
  if (any(tau < 0, na.rm = na.rm))
    stop(paste("tau must be positive", "\n", ""))
  if (log.p == TRUE)
    p <- exp(p)
  else p <- p
  if (any(p <= 0, na.rm = na.rm) | any(p >= 1, na.rm = na.rm))
    stop(paste("p must be between 0 and 1", "\n", ""))
  if (lower.tail == TRUE)
    p <- p
  else p <- 1 - p
  if (length(nu) > 1) {
    z <- ifelse((nu <= 0), qt(p * pt(1/(sigma * abs(nu)),
                                     tau), tau), qt(1 - (1 - p) * pt(1/(sigma * abs(nu)),
                                                                     tau), tau))
  }
  else {
    z <- if (nu <= 0)
      qt(p * pt(1/(sigma * abs(nu)), tau), tau)
    else qt(1 - (1 - p) * pt(1/(sigma * abs(nu)), tau), tau)
  }
  if (length(nu) > 1)
    ya <- ifelse(nu != 0, mu * (nu * sigma * z + 1)^(1/nu),
                 mu * exp(sigma * z))
  else if (nu != 0)
    ya <- mu * (nu * sigma * z + 1)^(1/nu)
  else ya <- mu * exp(sigma * z)
  # Round up D-scores < 1 to 1.0
  ya <- ya[ya < 1.0 & !is.na(ya)] <- 1.0
  ya
}


pBCT <- function (q, mu = 5, sigma = 0.1, nu = 1, tau = 2, lower.tail = TRUE,
                  log.p = FALSE, na.rm = TRUE)
{
  if (any(mu < 0, na.rm = na.rm))
    stop(paste("mu must be positive", "\n", ""))
  if (any(sigma < 0, na.rm = na.rm))
    stop(paste("sigma must be positive", "\n", ""))
  if (any(tau < 0, na.rm = na.rm))
    stop(paste("tau must be positive", "\n", ""))
  # recode D-score < 1 to 1.
  q[q < 1.0 & !is.na(q)] <- 1.0
  if (length(nu) > 1)
    z <- ifelse(nu != 0, (((q/mu)^nu - 1)/(nu * sigma)),
                log(q/mu)/sigma)
  else if (nu != 0)
    z <- (((q/mu)^nu - 1)/(nu * sigma))
  else z <- log(q/mu)/sigma
  FYy1 <- pt(z, tau)
  if (length(nu) > 1)
    FYy2 <- ifelse(nu > 0, pt(-1/(sigma * abs(nu)), df = tau),
                   0)
  else if (nu > 0)
    FYy2 <- pt(-1/(sigma * abs(nu)), df = tau)
  else FYy2 <- 0
  FYy3 <- pt(1/(sigma * abs(nu)), df = tau)
  FYy <- (FYy1 - FYy2)/FYy3
  if (lower.tail == TRUE)
    FYy <- FYy
  else FYy <- 1 - FYy
  if (log.p == FALSE)
    FYy <- FYy
  else FYy <- log(FYy)
  FYy
}
