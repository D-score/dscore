# test_Aij.R

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

system.time(fit1 <- rasch(data))

count <- fit1$count
system.time(fit2 <- rasch(data, count = count))



Aij1 <- function(data) {
  # create count tables
  data <- as.matrix(data)
  data[is.na(data)] <- 9
  Aij <- t(data == 0) %*% (data == 1)
  Aij
}

Aij2 <- function(data) {
  # create count tables
  data <- as.matrix(data)
  data[is.na(data)] <- 0
  Aij <- t(data) %*% data
  Aij
}

Aij3 <- function(data) {
  # create count tables
  data[is.na(data)] <- 0
  data <- as.matrix(data)
  Aij <- t(data) %*% data
  Aij
}

system.time(aij1 <- Aij1(data))
system.time(aij2 <- Aij2(data))
system.time(aij3 <- Aij3(data))
aji <- Aji(data)


# All items with k responses in rare category
idata <- ddata::gcdg %>%
  select_(.dots = item_names()) %>%
  select_if(category_size_exceeds, 25)
# colSums(idata, na.rm = TRUE)
dim(idata)

z25_Aij <- Aij(idata)

# work from count matrix
load(file = file.path(getwd(), "store", "z25_Aij.rda"))
largeset <- dimnames(z25_Aij)[[1]]
itemset <- largeset[largeset %in% items]
count <- z25_Aij[itemset, itemset]


