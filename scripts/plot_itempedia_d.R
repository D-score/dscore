# Creates itempedia_d.pdf

library("ddata")
library("dplyr", warn.conflicts = FALSE)
library("tidyr")
library("ggplot2")
library("dscore")

# take items
# 1) from a registered instrument
# 2) for which we have at least one observation in each category
items <- names(gcdg)[names(gcdg) %in% itemtable$item]
adm <- c("study", "id", "age")
data <- ddata::gcdg %>%
  select_(.dots = c(adm, items)) %>%
  select_if(category_size_exceeds, 1)
items <- names(data)

# fetch dscore
fn <- file.path(getwd(), "store", "alldscore.RData")
load(file = fn)

# merge data to obtain d
data <- left_join(data, alldscore, by = c("study", "id", "age"))

# proportion pass per dscore group
# observations per months (n) by study and item
pass <- data %>%
  gather(key = item, value = value, -d, -age, -id, -study) %>%
  drop_na(value, d) %>%
  mutate(dgp = cut(d, breaks = seq(0, 80, 2))) %>%
  group_by(item, study, dgp) %>%
  summarise(p = round(100 * mean(value)),
            a = mean(age),
            d = mean(d),
            n = n()) %>%
  ungroup %>%
  mutate(rug = FALSE) %>%
  left_join(ddata::itemtable, by = "item")

# define data for rug plot
data_rug <- data %>%
  select(study, id, age, d) %>%
  group_by(study, id, age) %>%
  summarise(d = mean(d)) %>%
  ungroup %>%
  drop_na(d) %>%
  mutate(rug = TRUE,
         value = 0)

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
  
  data_plot <- pass %>%
    filter_(filter_criteria)
  the_label <- data_plot$label[1]
  data_plot <- data_plot %>%
    filter(n >= min_n)
  
  studies <- unique(data_plot$study)
  rug <- pass %>%
    filter(rug & study %in% studies)
  
  plot <- ggplot(data_plot, aes(d, p, group = study, colour = study)) + 
    scale_x_continuous("D-score", limits = c(0, 80),
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

theme_set(theme_light())
plots <- plot_age_item(pass)

pdf_file <- file.path(getwd(), "results", "itempedia_d.pdf")
pdf(pdf_file, onefile = TRUE, width = 10, height = 5)
for (i in seq(length(plots))) {
  print(plots[[i]])
}
dev.off()
