# fit to Rasch model to selected items

library("dscore")
library("ddata")
library("dplyr", warn.conflicts = FALSE)
library("tidyr")

# source functions
file.sources = list.files(file.path(getwd(), "scripts", "functions"), 
                          pattern = "*.R$", full.names = TRUE, 
                          ignore.case = TRUE)
sapply(file.sources, source, .GlobalEnv)

# test with Dutch items
items <- item_names("NL")
adm <- c("country", "study", "id", "wave", "age")
data <- ddata::gcdg %>%
  select_(.dots = c(adm, items)) %>%
  filter(study == "Netherlands 1")
d_dutch <- fit_dmodel(model_name = "Dutch", items = items, data = data)

fn <- file.path(getwd(), "store", paste(model_name, "RData", sep = "."))
save(model, file = fn, compress = "xz")
