#' Anchor D-score scale
#'
#' This function calculates the tranformation of difficulty estimates
#' to a scale with fixed left and right anchors, and return the
#' transformed difficulty estimas.
#' @param beta Named numerical vector with difficulty estimates.
#' @param items Character vector of length two with the labels of left
#' and right anchor item. The default is \code{c("v1445", "v1463")}.
#' Item \code{"v1445"} is `Holds head up 45 degrees in prone position` and
#' item \code{"v1463"} is `Sits without support`.
#' @param values Character vector of length two with the labels of left
#' and right anchor item. The default is \code{(20, 40)}.
#' @return A vector with \code{length(beta)} elements with transformed
#' difficulty estimate
#' @details
#' This function scales the difficulty levels by a linear transformation
#' such that the difficulty levels of the two anchor items are set to
#' fixed values. The default setting produces the scale recommended in
#' Van Buuren (2014).
#' @references
#' van Buuren S (2014). Growth charts of human development. \emph{Statistical
#' Methods in Medical Research}, 23(4), 346-368.
#' @author Stef van Buuren, 2016
#' @examples
#' beta <- c(-1.5, 0, -0.4, 1, 1.2)
#' names(beta) <- paste0("item", 1:5)
#' anchor(beta, items = c("item2", "item4"))
#' @export
anchor <- function(beta, items = c("v1445", "v1463"),
                          values = c(20, 40)) {
  if (!all(items %in% names(beta))) stop("Anchor items not found")
  d <- data.frame(y = NA,
                  x = beta)
  d[items[1], "y"] <- values[1]
  d[items[2], "y"] <- values[2]
  fit <- lm(y ~ x, data = na.omit(d))
  return(predict(fit, newdata = d))
}
