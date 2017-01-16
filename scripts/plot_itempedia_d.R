# Creates itempedia_d.pdf

library("ddata")
library("dplyr", warn.conflicts = FALSE)
library("tidyr")
library("ggplot2")
library("dscore")

# source functions
file.sources = list.files(file.path(getwd(), "scripts", "functions"), 
                          pattern = "*.R$", full.names = TRUE, 
                          ignore.case = TRUE)
sapply(file.sources, source, .GlobalEnv)

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
  drop_na(item, value, d) %>%
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

theme_set(theme_light())
plots <- plot_age_item(pass, model_name = model_name)

# add logistic curves
tau <- gettau(names(plots), model$itembank, lexicon = "gcdg")
plots <- show_logistic_curve(plots, tau)

# add fit statistics
plots <- show_item_fit(plots, model$item_fit)

pdf_file <- file.path(getwd(), "results", paste0("itempedia_d_", model_name,".pdf"))
pdf(pdf_file, onefile = TRUE, width = 10, height = 5)
for (i in seq(length(plots))) {
  print(plots[[i]])
}
dev.off()
