#' D-score and DAZ calculation according to GCDG-itembank and reference
#' 
#' @param data A data frame with column that correspond to item 
#' responses. Scores are coded 
#' numerically as \code{pass = 1} and \code{fail = 0}. 
#' @param items A character vector with item names in the chosen \code{lexicon}. 
#' The default selects all numeric variables in \code{data}. Variables
#' not found the the itembank are skipped, as well as response that 
#' are \code{NA}. 
#' @param split A vector of names that defines the group for 
#' which the D-score and DAZ should be calculated. If the rows 
#' correspond to subject-time combinations, then we would normally 
#' specify something as \code{split = c("id", age)}, where \code{"id"}
#' and \code{"age"} are names in \code{data}.
#' @param agevar Name of the variable in the data specifying decimal 
#' age, i.e., age in years, rounded to three digits. This information 
#' is used 1) to break up calculations into separate D-scores per age, 
#' and 2) to specify age-dependent priors. 
#' @param itembank Name of the object that contains the item bank. By
#' default, this is set to \code{dscore::gcdg_itembank}, which 
#' corresponds to model 565_18 from the Global Child Development 
#' Group.
#' @param lexicon Name of the lexicon (variable names) that are 
#' used to identify item. The default is \code{lexicon = "gcdg"}.
#' For example, BSID-III items in the cognitive domain in 
#' this lexicon are named: \code{b3c1, b3c2, ...}.
#' @param reference The age-conditional reference used for 
#' calculating the age-adjusted Z scores, or \code{daz}
#' @param \dots Parameters passed down to lower-level functions 
#' \code{dscore()} and \code{daz}.
#' @details 
#' For efficiency, many developmental tests tailor the difficulty
#' of the items to the age of the child.  The calculations 
#' of the D-score are only corrrect if they are calculated 
#' from scores on items that were actually administered. 
#' Values of items that were not administered should be set 
#' to \code{NA}.
#' 
#' If more than one age is found within a subgroups defined 
#' by \code{split} the calculations will fail with the message
#' \code{Error in summarise_impl(.data, dots) :} 
#' \code{Column `d` must be length 1 (a summary value), not 3}.
#' This indicates that in one or more subgroups, there were 
#' three ages, not one. This error can be solved by redefining
#' \code{split} so that each subgroup contains only one age.
#' A quick way to do that is that set the last element in 
#' \code{split} equal to \code{agevar}.
#' @author Stef van Buuren 2016
#' @seealso \code{\link{dscore}}, \code{\link{daz}}, 
#' \code{\link{gcdg_itembank}}, \code{\link{adp}},
#' \code{\link{gcdg_reference}}
#' @examples 
#' data <- data.frame(
#'   id = c("Billy", "Robert", "Robert", "Robert", "Mary"),
#'   wave = c(2, 1, 2, 3, 1),
#'   age = c(1, 0.5, 1.03, 1.3, 0.6),
#'   b3c36 = c(1, 0, 1, NA, 1),
#'   b3c47 = c(0, 0, 1, NA, NA),
#'   b3g40 = c(NA, NA, 0, 1, NA))
#' items <- names(data)[4:6]
#' result1 <- dscore_gcdg(data, items = items, split = c("id", "age"))
#' result1
#' 
#' # use wave: incorrect since wave is not age in years
#' result2 <- dscore_gcdg(data, items = items, split = c("id", "wave"))
#' result2
#' 
#' # use both wave and age: correct
#' result3 <- dscore_gcdg(data, items = items, split = c("id", "wave"), agevar = "age")
#' result3
#' 
#' @export
dscore_gcdg <- function(data,
                        items = character(0),
                        split = c("id", "age"),
                        agevar = NULL,
                        itembank = dscore::gcdg_itembank,
                        lexicon = "gcdg",
                        reference = dscore::gcdg_reference,
                        ...) {
  # name manipulations
  if (is.null(agevar)) agevar <- split[length(split)]
  splitage <- unique(c(split, agevar))
  if (length(items) == 0L) items <- colnames(data)[sapply(data, is.numeric)]
  
  # calculate D and DAZ
  dz <- data %>%
    select(splitage, items) %>%
    gather(item, score, -splitage, na.rm = TRUE) %>%
    mutate(temp_ages = UQ(rlang::sym(agevar))) %>%
    arrange(!!!rlang::syms(splitage)) %>%
    group_by(!!!rlang::syms(splitage)) %>%
    summarise(d = dscore(scores = score, items = item,
                         ages = temp_ages, itembank = itembank,
                         lexicon = lexicon, ...)) %>%
    ungroup() %>% 
    mutate(temp_ages = UQ(rlang::sym(agevar)),
           daz = daz(d, temp_ages, ref = reference, ...)) %>%
    select(-temp_ages)
  
  # take care that result uses same rows as data
  left_join(x = select(data, split), y = dz, by = split)
}
