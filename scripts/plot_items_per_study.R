# Creates /results/items_per_study.pdf

library("ddata")
library("dplyr", warn.conflicts = FALSE)
library("tidyr")
library("ggplot2")
library("dscore")

# Obtain list of all items in gcdg
# all_items <- unique(unlist(sapply(ddata::gcdg_meta, "[", "item")))
# all_items <- gtools::mixedsort(all_items)
all_items <- item_names()

# select all items that are in range 0-1
# out_of_range <- check_items(ddata::gcdg, all_items)
# inrange_items <- dplyr::setdiff(all_items, out_of_range$item)
inrange_items <- all_items

# proportion pass per month (p), age per month (a)
# observations per months (n) by study and item
pass <- ddata::gcdg %>%
  select(study, age, one_of(inrange_items)) %>%
  gather(key = item, value = value, -age, -study) %>%
  drop_na(value, age) %>%
  mutate(agegp = cut(age, breaks = seq(0, 60, 1))) %>%
  group_by(study, item, agegp) %>%
  summarise(p = round(100 * mean(value)),
            a = mean(age),
            n = n()) %>%
  ungroup() %>%
  mutate(item =  factor(item, levels = all_items)) %>%
  arrange(item) %>%
  mutate(item = as.character(item)) %>% 
  full_join(ddata::itemtable, by = "item") 

# The following transformation will flatten the curves at low ages
# so that the slopes become more uniform
# require(scales) # trans_new() is in the scales library
# log_age3_trans <- function() 
#   trans_new("log_age3", 
#             function(x) log(x + 3), 
#             function(x) exp(x) - 3,
#             breaks = function() {c(1:6, 9, seq(12, 48, 6))},
#             domain = c(0.5, 60))



plot_age_study <- function(pass, by_name = "study", ...) {
  # pre-allocate list of ggplots
  studies <- sort(unique(pass$study))
  plot_list <- vector("list", length(studies))
  names(plot_list) <- studies
  
  # loop over plots
  for (i in 1:length(plot_list)) {
    cat("Study: ", as.character(i), studies[i], "\n")
    plot_list[[i]] <- plot_age_one_study(pass, by_name = by_name, 
                                         by_value = studies[i],
                                         ...)
  }
  
  return(plot_list)
}

plot_age_one_study <- function(pass, 
                               by_name = "study",
                               by_value = "Bangladesh",
                               min_n = 10, ...) {
  filter_criteria <- lazyeval::interp(~ which_column == by_value, 
                            which_column = as.name(by_name))
  data_plot <- pass %>%
    filter_(filter_criteria) %>%
    filter(n >= min_n) %>%
    mutate(item =  factor(item, levels = all_items)) %>%
    arrange(item)
  
  plot <- ggplot(data_plot, aes(a, p, group = item, colour = item)) + 
    scale_x_continuous("Age (in months)", limits = c(0, 60),
                       breaks = seq(0, 60, 6)) +
    scale_y_continuous("% pass", breaks = seq(0, 100, 20), 
                       limits = c(0, 100)) +
    scale_colour_manual(values = get_palette("item"), na.value = "grey") +
    geom_line() + geom_point() +
    facet_grid(study ~ .) +
    theme(legend.position = "none")
    # guides(col = guide_legend(nrow = 3, byrow = FALSE))
  return(plot)
}

theme_set(theme_light())
plots <- plot_age_study(pass, min_n = 10)

pdf_file <- file.path(getwd(), "results", "items_per_study.pdf")
pdf(pdf_file, onefile = TRUE, width = 10, height = 5)
for (i in seq(length(plots))) {
  print(plots[[i]])
}
dev.off()
