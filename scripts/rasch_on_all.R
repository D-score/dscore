# rasch_on_all.R

# stretch the dmetric::rasch() function on the ddata::master data

library("ddata")
library("dscore")
library("dplyr", warn.conflicts = FALSE)
library("tidyr")

dim(master)

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
