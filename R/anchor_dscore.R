#' Anchor D-score scale
#'
#' This function calculates the tranformation of difficulty estimates
#' to a scale with fixed left and right anchors, and return the
#' transformed difficulty estimas.
#' @param beta Named numerical vector with difficulty estimates.
#' @param anchor_items Character vector of length two with the labels of left
#' and right anchor item. The default is \code{c("v1445", "v1463")}.
#' Item \code{"v1445"} is `Holds head up 45 degrees in prone position` and
#' item \code{"v1463"} is `Sits without support`.
#' @param anchor_values Character vector of length two with the labels of left
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
#' anchor_dscore(beta, anchor_items = c("item2", "item4"))
#' @export
anchor_dscore <- function(beta, anchor_items = c("v1445", "v1463"),
                          anchor_values = c(20, 40)) {
  if (!all(anchor_items %in% names(beta))) stop("Anchor items not found")
  d <- data.frame(y = NA,
                  x = beta)
  d[anchor_items[1], "y"] <- anchor_values[1]
  d[anchor_items[2], "y"] <- anchor_values[2]
  fit <- lm(y ~ x, data = na.omit(d))
  return(predict(fit, newdata = d))
}
