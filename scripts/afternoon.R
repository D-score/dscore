# models
library("dscore")
library("ddata")
library("dplyr", warn.conflicts = FALSE)
library("tidyr")

# source functions
file.sources = list.files(file.path(getwd(), "scripts", "functions"), 
                          pattern = "*.R$", full.names = TRUE, 
                          ignore.case = TRUE)
sapply(file.sources, source, .GlobalEnv)


# remove duplicated equates
itemtable <- ddata::itemtable
itemtable <- itemtable[!duplicated(itemtable$item),]

itemtable$item <- as.character(itemtable$item)
itemtable$equate <- as.character(itemtable$equate)
itemtable$instrument <- as.character(itemtable$instrument)
itemtable$domain <- as.character(itemtable$domain)


# item definition
items <- names(ddata::gcdg)[names(ddata::gcdg) %in% itemtable$item]
data <- ddata::gcdg %>%
  select_(.dots = items) %>%
  select_if(category_size_exceeds, 1)
items <- names(data)

# linking items
itemtable_select <- itemtable[which(itemtable$item %in% items), ]
equatelist <- tapply(itemtable_select$item, itemtable_select$equate, list)

# based on equate_d_fr_1310.pdf
eq_COG <- paste0("COG", c(23, 25, 26, 30, 33, 35, 43, 49, 50, 51,
                          52, 55, 59, 61, 63, 64))
eq_EXP <- paste0("EXP", c(10, 11, 18, 19, 21, 25, 26, 27, 31))
eq_FM <- paste0("FM", c(11, 17, 19, 22, 24, 25, 27, 28, 32, 36, 
                        37, 39, 41, 50))
eq_GM <- paste0("GM", c(24, 25, 26, 30, 35, 40, 42, 43, 48, 50, 
                        54, 57, 58, 59, 60))
eq_REC <- paste0("REC", c(6, 7, 8, 11, 13, 17))

equatelist <- equatelist[c(eq_COG, eq_EXP, eq_FM, eq_GM, eq_REC)]

# rough item elimination
old_model_name <- "d_1221_eq8_fx"
fn <- file.path(getwd(), "store", paste(old_model_name, "RData", sep = "."))
load(file = fn)

# rough item eliminiation
keep <- with(model$item_fit, 
             infit < 1.3 & infit > 0.7 & outfit < 1.7 & outfit > 0.7)
table(keep)
items <- model$item_fit$item[keep]
data <- gcdg %>%
  select_(.dots = items) %>%
  select_if(category_size_exceeds, 20)
items <- names(data)
length(items) # 529

# add anchors, remove orphans 
orphans <- c("bm9b")
items <- items[-match(orphans, items)]
items <- c(items, "n12", "n26")
length(items)

# 
# test with all eligible items
model_name <- "afternoon"
adm <- c("country", "study", "id", "wave", "age")
data <- ddata::gcdg %>%
  select_(.dots = c(adm, items))
model <- fit_dmodel(model_name = model_name, items = items, 
                    equatelist = equatelist, 
                    data = data, free = FALSE)

fn <- file.path(getwd(), "store", paste(model_name, "RData", sep = "."))
save(model, file = fn, compress = "xz")

