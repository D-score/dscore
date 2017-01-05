# rasch_on_all.R

library("ddata")
library("dscore")
library("dplyr", warn.conflicts = FALSE)
library("tidyr")


# Dutch items
items <- item_names("NL")
data <- ddata::gcdg %>% select_(.dots = items)
fit <- rasch(data)
get_diff(fit)
# 

# All items with k responses in rare category
idata <- ddata::gcdg %>%
  select_(.dots = item_names()) %>%
  select_if(category_size_exceeds, 1000)
# colSums(idata, na.rm = TRUE)
dim(idata)

idata <- select(idata, -dg2, -n7)
fit <- rasch(idata)
get_diff(fit)

# instruments <- unique(itemtable$instrument)
# for (i in 1:length(instruments)) {
#   ins <- instruments[i]
#   items <- filter(itemtable, instrument == ins) %>% .$item
#   items <- intersect(items, names(master))
#   scores <- select_(master, .dots = c("agedays", items))
#   nscores <- colSums(!is.na(scores))
#   # summarise only items with n > 100
#   scores <- select(scores, which(nscores > 100))
# }

Aij <- function(data) {
  # create count tables
  data[is.na(data)] <- 9
  Aij <- t(data == 0) %*% (data == 1)
  Aij
}

Aji <- function(data) {
  # create count tables
  data[is.na(data)] <- 9
  Aji <- t(data == 1) %*% (data == 0)
  Aji
}

# All items with k responses in rare category
idata <- ddata::gcdg %>%
  select_(.dots = item_names()) %>%
  select_if(category_size_exceeds, 25)
# colSums(idata, na.rm = TRUE)
dim(idata)

z25_Aij <- Aij(idata)
