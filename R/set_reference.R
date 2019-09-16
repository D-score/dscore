set_reference <- function(key = "gsed") {
  if (key == "gsed") key <- "gcdg"
  dscore::builtin_references[dscore::builtin_references$pop == key, ]
}
