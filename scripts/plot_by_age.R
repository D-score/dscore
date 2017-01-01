library("ddata")
library("dplyr", warn.conflicts = FALSE)
library("tidyr")
library("ggplot2")

# source("define_colors.R")

# min and max category number (should either 0 or 1)
mm <- tbl_df(master) %>%
  select(age, one_of(itemtable$item)) %>%
  gather(key = item, value = value, -age) %>%
  drop_na(value, age) %>%
  group_by(item) %>%
  summarise(max = max(value), min = min(value))
table(mm$min, mm$max)
inrange <- mm$item[mm$max <= 1]


data <- tbl_df(master) %>%
  select(study, age, one_of(inrange))

# proportion pass per month (p), age per month (a)
# observations per months (n) by study and item
pass <- tbl_df(master) %>%
  select(study, age, one_of(inrange)) %>%
  gather(key = item, value = value, -age, -study) %>%
  drop_na(value, age) %>%
  mutate(agegp = cut(age, breaks = seq(0, 60, 1))) %>%
  group_by(study, item, agegp) %>%
  summarise(p = round(100 * mean(value)), 
            a = mean(age),
            n = n()) %>%
  ungroup() %>%
  mutate(item =  factor(item, levels = unique(itemtable$item))) %>%
  arrange(item) %>%
  mutate(item = as.character(item))

# The following transformation will flatten the curves at low ages
# so that the slopes become more uniform
# require(scales) # trans_new() is in the scales library
# log_age3_trans <- function() 
#   trans_new("log_age3", 
#             function(x) log(x + 3), 
#             function(x) exp(x) - 3,
#             breaks = function() {c(1:6, 9, seq(12, 48, 6))},
#             domain = c(0.5, 60))


library("lazyeval")



plot_age_study <- function(data, by_name = "study", ...) {
  
  pass <- tbl_df(data) %>%
    gather(key = item, value = value, -age, -study) %>%
    drop_na(value, age) %>%
    mutate(agegp = cut(age, breaks = seq(0, 60, 1))) %>%
    group_by(study, item, agegp) %>%
    summarise(p = round(100 * mean(value)), 
              a = mean(age),
              n = n()) %>%
    ungroup() %>%
    mutate(item =  factor(item, levels = unique(itemtable$item))) %>%
    arrange(item) %>%
    mutate(item = as.character(item))
  
  # pre-allocate list of ggplots
  studies <- sort(unique(data$study))
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

plot_age_one_study <- function(data, 
                               by_name = "study",
                               by_value = "Bangladesh",
                               min_n = 10, ...) {
  filter_criteria <- interp(~ which_column == by_value, 
                            which_column = as.name(by_name))
  data_plot <- data %>%
    filter_(filter_criteria) %>%
    filter(n >= min_n) %>%
    left_join(itemtable, by = "item") %>%
    mutate(item =  factor(item, levels = unique(itemtable$item))) %>%
    arrange(item)
  
  plot <- ggplot(data_plot, aes(a, p, group = item, colour = item)) + 
    scale_x_continuous("Age (in months)", limits = c(0, 48),
                       breaks = seq(0, 48, 6)) +
    scale_y_continuous("% pass", breaks = seq(0, 100, 20), 
                       limits = c(0, 100)) +
    scale_colour_manual(values = get_palette("item"), na.value = "grey") +
    geom_line() + geom_point() +
    facet_grid(study ~ .) + 
    theme(legend.position = "bottom") +
    guides(col = guide_legend(nrow = 3, byrow = FALSE))
  return(plot)
}

plots <- plot_age_study(data)

theme_set(theme_light())
p <- plot_age_study(pass, by_name = "study", by_value = "Netherlands 1")
p


