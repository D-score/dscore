# models

library("dscore")
library("ddata")
library("dplyr", warn.conflicts = FALSE)
library("tidyr")

# take items
# 1) from a registered instrument
# 2) for which we have at least one observation in each category
items <- names(ddata::gcdg)[names(ddata::gcdg) %in% itemtable$item]
data <- ddata::gcdg %>%
  select_(.dots = items) %>%
   select_if(category_size_exceeds, 1)
items <- names(data)

# select equategroups among selected items
itemtable_select <- itemtable[which(itemtable$item %in% items), ]
equatelist <- tapply(itemtable_select$item, itemtable_select$equate, list)

# remove troublesome equates 
equatelist$GM1 <- NULL  # ag2 - n4
equatelist$GM2 <- NULL  # ag1 - n3
equatelist$FM2 <- NULL  # af3 - n8
equatelist$EXP5 <- NULL  # b3e5 - dl4 - n10
equatelist$COG18 <- NULL  # b1m45 - b2m34 - b3c18 - n9

# source functions
file.sources = list.files(file.path(getwd(), "scripts", "functions"), 
                          pattern = "*.R$", full.names = TRUE, 
                          ignore.case = TRUE)
sapply(file.sources, source, .GlobalEnv)

# test with all eligible items
model_name <- "d_1221"
adm <- c("country", "study", "id", "wave", "age")
data <- ddata::gcdg %>%
  select_(.dots = c(adm, items))
model <- fit_dmodel(model_name = model_name, items = items, data = data)
fn <- file.path(getwd(), "store", paste(model_name, "RData", sep = "."))
save(model, file = fn, compress = "xz")

# calculate figures --- 

# test with items with outfit MSQ >= 10 and not in multi-equate group

# item definition
items <- names(ddata::gcdg)[names(ddata::gcdg) %in% itemtable$item]
data <- ddata::gcdg %>%
  select_(.dots = items) %>%
  select_if(category_size_exceeds, 1)
items <- names(data)

screen_model_name <- "d_1221"
fn <- file.path(getwd(), "store", paste(screen_model_name, "RData", sep = "."))
load(file = fn)

