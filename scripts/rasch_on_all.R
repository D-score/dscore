# rasch_on_all.R

library("ddata")
library("dscore")
library("dplyr", warn.conflicts = FALSE)
library("tidyr")

# Obtain list of all items in gcdg
all_items <- item_names()

# select all items that are in range 0-1
out_of_range <- check_items(ddata::gcdg, all_items)
inrange_items <- dplyr::setdiff(all_items, out_of_range$item)

dim(gcdg)

# length(table(x)) == 2
# min(table(x)) >= 2

instruments <- unique(itemtable$instrument)
for (i in 1:length(instruments)) {
  ins <- instruments[i]
  items <- filter(itemtable, instrument == ins) %>% .$item
  items <- intersect(items, names(master))
  scores <- select_(master, .dots = c("agedays", items))
  nscores <- colSums(!is.na(scores))
  # summarise only items with n > 100
  scores <- select(scores, which(nscores > 100))
}
