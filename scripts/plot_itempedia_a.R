# Creates itempedia_a.pdf

library("ddata")
library("dplyr", warn.conflicts = FALSE)
library("tidyr")
library("ggplot2")
library("dscore")

# Obtain list of items that 1) belong to an instrument 
# that is linked, and 2) for which we have at least 
# 1 observations in each category
data <- ddata::gcdg %>%
  select_(.dots = item_names()) %>%
  select_if(category_size_exceeds, 1)
items <- intersect(unique(itemtable$item), names(data))

# define data for rug plot
data_rug <- ddata::gcdg %>%
  select(study, age, one_of(items)) %>%
  gather(key = item, value = value, -age, -study) %>%
  drop_na(value, age) %>% 
  mutate(rug = TRUE)

# proportion pass per month (p), age per month (a)
# observations per months (n) by study and item
pass <- data_rug %>%
  mutate(agegp = cut(age, breaks = seq(0, 60, 1))) %>%
  group_by(item, study, agegp) %>%
  summarise(p = round(100 * mean(value)),
            a = mean(age),
            n = n()) %>%
  ungroup %>%
  mutate(rug = FALSE) %>%
  left_join(ddata::itemtable, by = "item")

pass <- bind_rows(pass, data_rug)

plot_age_item <- function(pass, by_name = "item", ...) {
  # pre-allocate list of ggplots
  items <- gtools::mixedsort(unique(pass$item))
  plot_list <- vector("list", length(items))
  names(plot_list) <- items
  
  # loop over plots
  for (i in 1:length(plot_list)) {
    cat("Item: ", as.character(i), items[i], "\n")
    plot_list[[i]] <- plot_age_one_item(pass, by_name = by_name, 
                                        by_value = items[i],
                                        i = i,
                                        ...)
  }
  
  return(plot_list)
}

plot_age_one_item <- function(pass, 
                              by_name,
                              by_value,
                              i = 0,
                              min_n = 10, ...) {
  filter_criteria <- lazyeval::interp(~ which_column == by_value & rug == FALSE, 
                                      which_column = as.name(by_name))
  filter_rug      <- lazyeval::interp(~ which_column == by_value & rug == TRUE, 
                                      which_column = as.name(by_name))
  data_plot <- pass %>%
    filter_(filter_criteria)
  the_label <- data_plot$label[1]
  data_plot <- data_plot %>%
    filter(n >= min_n)
  
  rug <- pass %>%
    filter_(filter_rug)
  #  %>%
  #    mutate(item =  factor(item, levels = all_items)) %>%
  #    arrange(item)
  
  plot <- ggplot(data_plot, aes(a, p, group = study, colour = study)) + 
    scale_x_continuous("Age (in months)", limits = c(0, 60),
                       breaks = seq(0, 60, 6)) +
    scale_y_continuous("% pass", breaks = seq(0, 100, 20), 
                       limits = c(0, 100)) +
    scale_colour_manual(values = get_palette("study"), na.value = "grey")
    
  
  # add rugs 
  if (nrow(rug) >= 1)
    plot <- plot + 
    geom_rug(aes(x = age, y = value, group = study, colour = study),
             data = subset(rug, value == 0),
             position = "jitter", sides = "b", size = 0.2) +
    geom_rug(aes(x = age, y = value, group = study, colour = study),
             data = subset(rug, value == 1),
             position = "jitter", sides = "t", size = 0.2)
    
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

theme_set(theme_light())
plots <- plot_age_item(pass)

pdf_file <- file.path(getwd(), "results", "itempedia_a.pdf")
pdf(pdf_file, onefile = TRUE, width = 10, height = 5)
for (i in seq(length(plots))) {
  print(plots[[i]])
}
dev.off()
