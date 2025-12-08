init_key <- function(key, population, transform, qp) {
  key <- set_default_key(key)
  idx <- which(dscore::builtin_keys$key == key)

  population <- set_default_population(population, idx)
  transform <- set_default_transform(transform, idx)
  qp <- set_default_qp(qp, idx)

  result <- list(
    key = key,
    population = population,
    transform = transform,
    qp = qp
  )

  return(result)
}

init_mu <- function(data, key, a, prior_mean, prior_mean_NA) {
  if (is.null(prior_mean_NA)) {
    prior_mean_NA <- NA_real_
  }
  n <- length(a)

  # set prior mean mu per observation
  if (is.null(prior_mean)) {
    # default, use the key, handle NA
    return(get_mu(a, key, prior_mean_NA))
  }
  if (is.numeric(prior_mean) && length(prior_mean) == 1L) {
    # scalar method, handle NA
    mu <- rep(prior_mean, n)
    mu[is.na(a)] <- prior_mean_NA
    return(mu)
  }
  if (is.character(prior_mean) && prior_mean %in% names(data)) {
    # column method
    return(data[[prior_mean]])
  }
  if (is.numeric(prior_mean) && length(prior_mean) == n) {
    # vector method
    return(prior_mean)
  }
  return(rep(NA_real_, n))
}

init_sd <- function(data, key, a, prior_sd, prior_sd_NA) {
  if (is.null(prior_sd_NA)) {
    prior_sd_NA <- NA_real_
  }
  n <- length(a)

  # set prior sd per observation
  if (is.null(prior_sd)) {
    # default, use fixed value, handle NA
    sd <- rep(5, n)
    sd[is.na(a)] <- prior_sd_NA
    return(sd)
  }
  if (is.numeric(prior_sd) && length(prior_sd) == 1L) {
    # scalar method, handle NA
    sd <- rep(prior_sd, n)
    sd[is.na(a)] <- prior_sd_NA
    return(sd)
  }
  if (is.character(prior_sd) && prior_sd %in% names(data)) {
    # column method
    return(data[[prior_sd]])
  }
  if (is.numeric(prior_sd) && length(prior_sd) == n) {
    # vector method
    return(prior_sd)
  }
  return(rep(NA_real_, n))
}
