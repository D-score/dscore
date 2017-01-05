# rasch_on_all.R

library("ddata")
library("dscore")
library("dplyr", warn.conflicts = FALSE)
library("tidyr")

# Only Dutch items: 57 items
items <- item_names("NL")
data <- ddata::gcdg %>% select_(.dots = items)
system.time(fit1 <- rasch(data))
system.time(fit2 <- rasch(data, count = gcdg_count))
plot(get_diff(fit1), get_diff(fit2)); abline(0,1)

# All items with at least 1000 responses in each category
# There are 302 items
data <- ddata::gcdg %>%
  select_(.dots = item_names()) %>%
  select_if(category_size_exceeds, 1000)
dim(data)

# we'll get an error because there are two orphans
system.time(fit3 <- rasch(data))
# remove two items: 
orphans <- fit3$orphans
keep <- setdiff(item_names(), orphans)
data2 <- select_(data, .dots = keep)
system.time(fit3 <- rasch(data2))

system.time(fit4 <- rasch(data, count = gcdg_count))
system.time(fit4 <- rasch(data2, count = gcdg_count))
plot(get_diff(fit3), get_diff(fit4)); abline(0,1)

# And now: fit Rasch model on ALL items
# There are 1872 items with at least 1 observation in each category
data <- ddata::gcdg %>%
  select_(.dots = item_names()) %>%
  select_if(category_size_exceeds, 1)

# the following takes about 3 minutes on iMac 3.3 GHz Intel Core i5, 32Gb core
# system.time(fit6 <- rasch(data, count = gcdg_count, progress = TRUE))
system.time(fit6 <- rasch(data, count = gcdg_count))
hist(get_diff(fit6))

# how do estimates on Dutch items compare to published estimates?
length(get_diff(fit6)[paste("v",1:57)])
pub_tau <- filter(itembank, !is.na(lex.dutch1983)) %>% .$tau
names(pub_tau) <- filter(itembank, !is.na(lex.dutch1983)) %>% .$lex.jam
tau6 <- anchor(get_diff(fit6), items = c("n12", "n26"))
cor(pub_tau, tau6[paste0("n", 1:57)])
plot(x = pub_tau, y = pub_tau - tau6[paste0("n", 1:57)],
     xlab = "Published", ylab = "Published - estimated",
     main = "Difficulties items n1:n57")
abline(0, 0, lty = 2)

# It turns out that this solution is identical to the smocc-doove analysis
# that includes only 2 data sources (and far fewer items)

# The tau estimates for the other data sets are unconnected to the 
# SMOCC-Doove data since there is no overlap between Dutch items and 
# and of the instrument. It is possible that instruments and Dutch 
# items use different scales (despite being in the same analysis).

# TODO
# The overlap needs to be created with explicit equivalence assumptions
# We'll be making assumptions about the equivalence of items (rather than 
# equivalence of populations), so the populations can freely differ.
