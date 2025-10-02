set_default_key <- function(key) {
  if (is.null(key) || key == "gsed") {
    key <- "gsed2510"
  }
  return(key)
}

set_default_population <- function(population, idx) {
  if (is.null(population) && length(idx)) {
    return(dscore::builtin_keys$base_population[idx])
  }
  if (is.null(population) && !length(idx)) {
    # warning("No population specified, using 'preliminary_standards'.")
    return("preliminary_standards")
  }
  return(population)
}

set_default_transform <- function(transform, idx) {
  if (is.null(transform) && length(idx)) {
    return(c(
      dscore::builtin_keys$intercept[idx],
      dscore::builtin_keys$slope[idx]
    ))
  }
  if (is.null(transform) && !length(idx)) {
    return(c(55.724132, 3.603965))
  }
  return(transform)
}

set_default_qp <- function(qp, idx) {
  if (is.null(qp) && length(idx)) {
    return(seq(
      from = dscore::builtin_keys$from[idx],
      to = dscore::builtin_keys$to[idx],
      by = dscore::builtin_keys$by[idx]
    ))
  }
  if (is.null(qp) && !length(idx)) {
    return(-10:125)
  }
  return(qp)
}
