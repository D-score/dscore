show_logistic_curve <- function(plot, location, 
                                colour = "grey50", 
                                size = 0.5, linetype = "dashed", ...) {
  if (is.ggplot(plot)) return(draw_logistic(plot, location = location[1], 
                                            colour = colour, size = size, linetype = linetype, 
                                            ...))
  if (is.list(plot)) {
    if (length(location) > 1 & length(location) != length(plot))
      stop("tau and plot are of incompatible length")
    for (i in 1:length(plot)) {
      plot[[i]] <- draw_logistic(plot[[i]], 
                                 location = location[i], 
                                 colour = colour, 
                                 size = size, 
                                 linetype = linetype, 
                                 ...)
    }
  }
  return(plot)
}
