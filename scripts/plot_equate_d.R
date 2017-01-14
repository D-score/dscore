# Creates equate_d.pdf

# DIFplot per equate group

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

model_name <- "fx_1310"
model_name <- "fr_1310"
fn <- file.path(getwd(), "store", paste(model_name, "RData", sep = "."))
load(file = fn)

# merge data to obtain d
data <- left_join(data, model$dscore, by = c("study", "id", "age"))

# proportion pass per dscore group
# observations per months (n) by study and item
pass <- data %>%
  gather(key = item, value = value, -d, -age, -id, -study) %>%
  drop_na(value, d) %>%
  mutate(dgp = cut(d, breaks = seq(0, 60, 2))) %>%
  group_by(study, item, dgp) %>%
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

plot_by_grp <- function(pass, by_name = "equate", 
                        model_name = "unspecified", ...) {
  # pre-allocate list of ggplots
  by_grp <- gtools::mixedsort(unique(pass$equate))
  if (any(is.na(by_grp))) by_grp <- by_grp[-which(is.na(by_grp))]  # remove misisng 
  plot_list <- vector("list", length(by_grp))
  names(plot_list) <- by_grp
  
  # loop over plots
  for (i in 1:length(plot_list)) {
    cat("Group: ", as.character(i), by_grp[i], "\n")
    plot_list[[i]] <- plot_d_one_grp(pass, by_name = by_name, 
                                     by_value = by_grp[i],
                                     i = i, 
                                     model_name = model_name, ...)
  }
  return(plot_list)
}


plot_d_one_grp <- function(pass, 
                           by_name,
                           by_value,
                           i = 0,
                           min_n = 10, 
                           model_name = "",
                           ...) {
  filter_criteria <- lazyeval::interp(~ which_column == by_value, 
                                      which_column = as.name(by_name))
  data_plot <- pass %>%
    filter_(filter_criteria) %>%
    filter(n >= min_n)
  
  # items with data within equate group
  items <- unique(data_plot$item)
  labels <- data_plot$label[match(items, data_plot$item)]
  
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
    annotate("text", x = 1, y = 2, hjust = 0,
             label = as.character(i)) + 
    annotate("text", x = 7, y = 2, hjust = 0,
             label = by_value) 
  
  # item nams
  for (l in length(labels):1) {
    plot <- plot + annotate("text", x = 1, y = 7 + (l - 1) * 5, hjust = 0,
                            label = items[l])
    if (!is.na(labels[l]))
      plot <- plot + annotate("text", x = 7, y = 7 + (l - 1) * 5, hjust = 0,
                              label = labels[l])
  }
  return(plot)
}

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

theme_set(theme_light())
plots <- plot_by_grp(pass, model_name = model_name)

# add logistic curves
tau <- model$itembank$tau[match(names(plots), model$itembank$equate)]
plots <- show_logistic_curve(plots, location = tau)

pdf_file <- file.path(getwd(), "results", paste0("equate_d_", model_name,".pdf"))
pdf(pdf_file, onefile = TRUE, width = 10, height = 5)
for (i in seq(length(plots))) {
  print(plots[[i]])
}
dev.off()
