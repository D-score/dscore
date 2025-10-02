# Save built-in itemtable
# Fields: item, equate, label
library(dplyr)
fn <- file.path("data-raw/data/items/itemnames_translate.txt")
builtin_translate <- read.delim(
  file = fn,
  quote = "",
  stringsAsFactors = FALSE,
  na = "",
  fileEncoding = "UTF-8",
  header = TRUE
)

usethis::use_data(builtin_translate, overwrite = TRUE)
