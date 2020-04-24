# Save built-in itembank for items in the model
# Fields: key, item, tau, instrument, domain, mode, number, label
library(dscore)
library(dplyr)

# define project
project <- path.expand("~/Package/dscore/dscore")

f1 <- file.path(project, "data-raw/data/keys/dutch.txt")
f2 <- file.path(project, "data-raw/data/keys/gcdg.txt")
f3 <- file.path(project, "data-raw/data/keys/gsed.txt")
f4 <- file.path(project, "data-raw/data/keys/mullen_itembank.txt")

key_dutch <- read.delim(file = f1, stringsAsFactors = FALSE)
key_dutch <- key_dutch[order_itemnames(key_dutch$item), ]

key_gcdg <- read.delim(file = f2, stringsAsFactors = FALSE)
key_gcdg <- key_gcdg[order_itemnames(key_gcdg$item), ]

key_gsed <- read.delim(file = f3, stringsAsFactors = FALSE)
key_gsed <- key_gsed[order_itemnames(key_gsed$item), ]

key_mullen <- read.delim(file = f4, stringsAsFactors = FALSE)
key_mullen <- key_mullen[order_itemnames(key_mullen$item), ]

builtin_itembank <- bind_rows(key_dutch, key_gcdg, key_gsed, key_mullen) %>%
  left_join(get_itemtable(decompose = TRUE), by = "item") %>%
  select(-equate)

# save to /data
usethis::use_data(builtin_itembank, overwrite = TRUE)
