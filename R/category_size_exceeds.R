#' Select_if helpers
#'
#' Helper functions for the \code{select_if()} function.
#' @param x Numeric vector. This argument can be left unspecified when the
#' function is called by \code{select_if()}.
#' @param k Minimum number per category needed.
#' @return Logical
#' @examples
#' library("ddata")
#' library("dplyr")
#' data <- ddata::gcdg %>%
#'  filter(study == "Netherlands 2") %>%
#'  select_(.dots = item_names("NL2"))
#' dim(data)
#'
#' # take only columns with at least 100 observations per category
#' data <- data %>% select_if(category_size_exceeds, 100)
#' dim(data)
#' @export
category_size_exceeds <- function(x, k = 10) {
  f <- table(x)
  if (length(f) < 2) return(FALSE)
  return(min(f) >= k)
}
