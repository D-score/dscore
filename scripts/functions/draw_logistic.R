draw_logistic <- function(plot, location = 20, scale = 2.1044, ...) {
  # function assumes that location is scalar and plot is ggplot
  if (!is.ggplot(plot)) stop("Argument plot not a ggplot.")
  if (is.na(location)) return(plot)
  x <- seq(location - 7 * scale, location + 7 * scale, by = 0.1 * scale)
  y <- 100 * plogis(x, location = location, scale = scale)
  plot <- plot + 
    geom_line(aes(x = x, y = y, group = NULL, colour = NULL), 
              data = data.frame(x, y), ...)
  plot
}
