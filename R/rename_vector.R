#' Rename character vector
#'
#' Translates names between different lexicons (naming schema).
#' @param input    A character vector with names to be translated
#' @param lexin    A string indicating the input lexicon. One of `"phase1"`,
#' `"phase2"`, `"short1"`, `"short2"`, `"gsed"`, `"gsed2"`, `"gsed3"` or
#' `"gsed4"`
#' Default is `"phase2"`, which orders item names according to the
#' published 2023 version of the SF and LF instruments.
#' @param lexout   A string indicating the output lexicon. One of `"phase1"`,
#' `"phase2"`, `"short1"`, `"short2"`, `"gsed"`, `"gsed2"`, `"gsed3"` or
#' `"gsed4"`.
#' Default is `"gsed4"`. The default output `"gsed4"` applies instrument
#' codes `sf_` and `lf_`, which can be understood by the `dscore` package.
#' @param notfound A string indicating what to do some input value is not found
#' @param contains A string to filter the translation table prior to matching.
#' Needed to prevent double matches. The default ("") does not filter.
#' @param underscore Replaces space (" ") and dash ("-") by underscore ("_")
#' @param trim A substring to be removed from `input`. Defaults to "Ma_".
#' @param lowercase Sets all variables in lower case.
#' in `lexin`? The default `notfound = "copy"` copies the input values into the
#' output value. In other cases (e.g. `""` or `NA_character_`), the function
#' uses the string specified in `notfound` as a replacement value.
#' @param force_subjid_agedays If `TRUE`, forces the output to have `"subjid"`
#' and `"agedays"` as names for the `"ID"` and `"age"`, respectively.
#' @return A character vector of the same length as `input` with processed
#' names.
#' @details
#' The recommended approach for reading new data is to name the columns
#' according to the names defined by `"short2"` and the apply `rename_vector()`
#' to translate the names to the `"gsed4"` lexicon.
#'
#' The lexicons `"phase1"`, `"short1"`, `"gsed"` and `"gsed2"` are included
#' for backward compatibility, and are not recommended for use with new
#' data.
#' @examples
#' # Using Ma_SF_Cxx as input names, 2023 SF/LF version
#' input <- c("file", "GSED_ID", "Ma_SF_Parent ID", "Ma_SF_C01", "Ma_SF_C02")
#' rename_vector(input)
#' rename_vector(input, lexout = "short2", lowercase = FALSE)
#' rename_vector(input, lexout = "gsed3", trim = "Ma_SF_")
#'
#' # Convert short names to gsed names
#' input <- c("file", "GSED_ID", "Ma_SF_Parent ID", paste0("SF00", 1:3))
#' rename_vector(input, lexin = "short2", lowercase = TRUE)
#' @export
rename_vector <- function(
  input,
  lexin = c(
    "phase2",
    "phase1",
    "short1",
    "short2",
    "gsed",
    "gsed2",
    "gsed3",
    "gsed4"
  ),
  lexout = c(
    "gsed4",
    "gsed3",
    "gsed2",
    "gsed",
    "short2",
    "short1",
    "phase1",
    "phase2"
  ),
  notfound = "copy",
  contains = c("", "Ma_SF_", "Ma_LF_", "bsid_"),
  underscore = TRUE,
  trim = "Ma_",
  lowercase = TRUE,
  force_subjid_agedays = FALSE
) {
  lexin <- match.arg(lexin)
  lexout <- match.arg(lexout)
  contains <- match.arg(contains)

  # rename itemnames
  colin <- switch(
    lexin,
    phase1 = "phase1",
    phase2 = "phase2",
    short1 = "short1",
    short2 = "short2",
    gsed = "gsed",
    gsed2 = "gsed2",
    gsed3 = "gsed3",
    gsed4 = "gsed4",
    "notfound"
  )
  colout <- switch(
    lexout,
    phase1 = "phase1",
    phase2 = "phase2",
    short1 = "short1",
    short2 = "short2",
    gsed = "gsed",
    gsed2 = "gsed2",
    gsed3 = "gsed3",
    gsed4 = "gsed4",
    "notfound"
  )
  if (colin == "notfound") {
    stop("Lexicon not found: ", lexin)
  }
  if (colout == "notfound") {
    stop("Lexicon not found: ", lexout)
  }

  output <- input
  mt <- dscore::builtin_translate
  v <- mt[match(input, pull(mt, colin)), colout, drop = TRUE]
  output[!is.na(v)] <- v[!is.na(v)]
  if (is.na(notfound[1L]) || notfound[1L] != "copy") {
    output[is.na(v)] <- notfound[1L]
  }

  # prettify
  if (underscore) {
    output <- sub(" ", "_", output)
    output <- sub("-", "_", output)
  }
  output <- sub(trim, "", output)
  if (lowercase) {
    output <- tolower(output)
  }

  # force subjid and agedays names
  if (force_subjid_agedays) {
    output <- sub("gsed_id", "subjid", output)
    output <- sub("age", "agedays", output)
  }

  return(output)
}
