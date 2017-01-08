# Create item-by-age plots per equate group

library("ddata")
library("dplyr", warn.conflicts = FALSE)
library("tidyr")
library("ggplot2")
library("dscore")

# take items
# 1) from a registered instrument
# 2) for which we have at least one observation in each category
items <- names(gcdg)[names(gcdg) %in% itemtable$item]
data <- ddata::gcdg %>%
  select_(.dots = items) %>%
  select_if(category_size_exceeds, 1)
items <- names(data)

# select only items that are part of an equate group
items_eq <- unique(itemtable$item[!is.na(itemtable$equate)])
items <- intersect(items, items_eq)

# proportion pass per month (p), age per month (a)
# observations per months (n) by study and item
pass <- ddata::gcdg %>%
  select(study, age, one_of(items)) %>%
  gather(key = item, value = value, -age, -study) %>%
  drop_na(value, age) %>%
  mutate(agegp = cut(age, breaks = seq(0, 60, 1))) %>%
  group_by(item, study, agegp) %>%
  summarise(p = round(100 * mean(value)),
            a = mean(age),
            n = n()) %>%
  ungroup %>%
  left_join(ddata::itemtable, by = "item")

plot_age_by_grp <- function(pass, by_name = "equate", ...) {
  # pre-allocate list of ggplots
  by_grp <- gtools::mixedsort(unique(pass$equate))
  plot_list <- vector("list", length(by_grp))
  names(plot_list) <- by_grp
  
  # loop over plots
  for (i in 1:length(plot_list)) {
    cat("Group: ", as.character(i), by_grp[i], "\n")
    plot_list[[i]] <- plot_age_one_grp(pass, by_name = by_name, 
                                         by_value = by_grp[i],
                                         i = i,
                                         ...)
  }
  
  return(plot_list)
}

plot_age_one_grp <- function(pass, 
                               by_name,
                               by_value,
                               i = 0,
                               min_n = 10, ...) {
  filter_criteria <- lazyeval::interp(~ which_column == by_value, 
                            which_column = as.name(by_name))
  data_plot <- pass %>%
    filter_(filter_criteria) %>%
    filter(n >= min_n)
#  %>%
#    mutate(item =  factor(item, levels = all_items)) %>%
#    arrange(item)
  
  plot <- ggplot(data_plot, aes(a, p, group = study, colour = study)) + 
    scale_x_continuous("Age (in months)", limits = c(0, 60),
                       breaks = seq(0, 60, 6)) +
    scale_y_continuous("% pass", breaks = seq(0, 100, 20), 
                       limits = c(0, 100)) +
    scale_colour_manual(values = get_palette("study"), na.value = "grey") +
    geom_line() + geom_point() +
    theme(legend.position = c(0.95,0.05), legend.justification = c(1, 0)) + 
    guides(fill = guide_legend(title = NULL)) + 
    annotate("text", x = 1, y = 7, hjust = 0, 
             label = as.character(data_plot$equate[1])) +
    annotate("text", x = 1, y = 2, hjust = 0,
               label = paste(unique(data_plot$item), collapse = " - "))
  return(plot)
}

theme_set(theme_light())
plots <- plot_age_by_grp(pass, by_name = "equate")

pdf_file <- file.path(getwd(), "results", "equate_age.pdf")
pdf(pdf_file, onefile = TRUE, width = 10, height = 5)
for (i in seq(length(plots))) {
    print(plots[[i]])
}
dev.off()
