#' Calculate the D-score from broad data
#'
#' @param data A \code{tbl_df} or \code{data.frame} containing study, 
#' id and age (in days) variables, as well as the item responses, with 
#' each item coded in a different column. See details.
#' @param items Character vector with active item names in \code{data}.
#' @param adm   Character vector with administrative variables in \code{data}.
#' The default is \code{adm = c("country", "study", "id", "wave", "age")}.
#' @param itembank The itembank with item difficulties, usually calculated
#' by \code{calculate_itembank()}
#' @param lexicon Item names dictionary. Can be one of 
#' \code{"gsed"}, \code{"gcdg"}, \code{"ghap"}, \code{dutch1983}, \code{dutch1996}, 
#' \code{"dutch2005"} or \code{"smocc"}.
#' @param model Model name for calculating D-score. Currently not used.
#' @param mu_count A string indicating the name of the Count function for
#' \code{mu} in choosing the age-dependent prior through \code{dscore::adp()}. The
#' default is \code{mu_count = "gcdg"}.
#' @details 
#' 
#' The items are coded as integer 0/1 (0 = fail, 1 = pass) variables. 
#' Logical and numeric variables are recoded by \code{as.integer()}. 
#' Missing values are allowed. The procedure does not handle polytomous 
#' responses. Any values different from 0 or 1 are set to \code{NA}.
#' 
#' The names of the items should conform to the specified lexicon. Item 
#' names that are not present in the lexicon are ignored in the D-score
#' calculation. Please check the column \code{"n"} in the result 
#' to infer whether item names are properly recognized. 
#' Names are case-sensitive. Common practice is to use lowercase only.
#' @return A data frame with columns named \code{"study", "id", "agedays", "d", "n"}.
#' @export
calculate_dscore <- function(data, items,
                             adm = c("country", "study", "id", "wave", "age"),
                             itembank, 
                             lexicon = c("gsed", "gcdg", "ghap", "dutch1983", 
                                         "dutch1996", "dutch2005", "smocc"),
                             model = NULL, mu_count = "gcdg") {
  
  # calculate d-scores
  dscore <- data %>%
    select(one_of(c(adm, items))) %>%
    gather(item, score, -one_of(adm), na.rm = TRUE) %>%
    arrange(.data$country, .data$study, .data$id, .data$age) %>%
    group_by(.data$study, .data$id, .data$age) %>%
    summarise(d = dscore::dscore_vector(scores = score, items = item,
                                        ages = .data$age / 12, mu = mu_count,
                                        itembank = itembank, lexicon = lexicon)) %>%
    ungroup()
  
  return(dscore)
}
