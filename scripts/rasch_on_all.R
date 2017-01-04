# rasch_on_all.R

library("ddata")
library("dscore")
library("dplyr", warn.conflicts = FALSE)
library("tidyr")

# Obtain list of all items in gcdg


# length(table(x)) == 2
# min(table(x)) >= 2

# Dutch items
items <- item_names("NL")
data <- select_(gcdg, .dots = all_items)
fit <- rasch(data)
get_diff(fit)
# 
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
