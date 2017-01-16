annotate_item_fit <- function(plot, itemfit) {
  # function assumes that location is scalar and plot is ggplot
  if (!is.ggplot(plot)) stop("Argument plot not a ggplot.")
  if (is.na(itemfit[1])) return(plot)
  plot <- plot + 
    annotate("text", x = 1, y = 97, hjust = 0,
             label = paste0("Outfit ", 
                            round(itemfit$outfit, 2),
                            "(", 
                            round(itemfit$outfit_z, 2),
                            ")"))
  plot <- plot + 
    annotate("text", x = 1, y = 92, hjust = 0,
             label = paste0("Infit  ", 
                            round(itemfit$infit, 2),
                            "(", 
                            round(itemfit$infit_z, 2),
                            ")"))
  plot
}
