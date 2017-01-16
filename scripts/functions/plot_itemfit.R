plot_itemfit <- function(model, threshold = 20) {
  data <- model$item_fit
  data <- data[data$infit_z > 15,]
  plot <- ggplot(data, aes(x = infit_z, y = reorder(item, infit_z))) + 
    geom_point(size = 2) + 
    theme_bw() + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey60", linetype = "dashed"))
  plot
}

