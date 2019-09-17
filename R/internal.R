count_mu_dutch <- function(t) {44.35 - 1.8 * t + 28.47 * log(t + 0.25)}
count_mu_gcdg  <- function(t) {47.65 - 3.05 * t + 26.70 * log(t + 0.19)}

set_reference <- function(pop = "gcdg") {
  if (pop == "gsed") pop <- "gcdg"
  dscore::builtin_references[dscore::builtin_references$pop == pop, ]
}
