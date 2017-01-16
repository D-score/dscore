plot_age_one_item <- function(pass, 
                              by_name,
                              by_value,
                              i = 0,
                              min_n = 10, 
                              model_name = "", 
                              ...) {
  filter_criteria <- lazyeval::interp(~ which_column == by_value & rug == FALSE, 
                                      which_column = as.name(by_name))
  data_plot <- pass %>%
    filter_(filter_criteria)
  the_label <- data_plot$label[1]
  data_plot <- data_plot %>%
    filter(n >= min_n)
  
  studies <- unique(data_plot$study)
  rug <- pass %>%
    filter(rug & study %in% studies)
  
  plot <- ggplot(data_plot, aes(d, p, group = study, colour = study)) + 
    scale_x_continuous(paste0("D-score (", model_name,")"), 
                       limits = c(0, 80),
                       breaks = seq(0, 80, 10)) +
    scale_y_continuous("% pass", breaks = seq(0, 100, 20), 
                       limits = c(0, 100)) +
    scale_colour_manual(values = get_palette("study"), na.value = "grey")
  
  # add rugs 
  if (nrow(rug) >= 1)
    plot <- plot + 
    geom_rug(aes(x = d, y = 0, group = study, colour = study),
             data = rug,
             position = "jitter", sides = "b", size = 0.2)
  
  # add proportions
  if (nrow(data_plot) >= 1)
    plot <- plot +
    geom_line() + geom_point()
  
  # annotations
  plot <- plot + 
    theme(legend.position = c(0.95, 0.05), legend.justification = c(1, 0)) + 
    guides(fill = guide_legend(title = NULL)) + 
    annotate("text", x = 1, y = 7, hjust = 0,
             label = paste(as.character(i), by_value, sep = "  ")) 
  if (!is.na(the_label)) 
    plot <- plot + 
    annotate("text", x = 1, y = 2, hjust = 0, label = the_label)
  
  return(plot)
}
