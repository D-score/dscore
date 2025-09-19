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

# Add SF items with selfreport mode 's'
library(stringr)
sf_s <- builtin_translate |>
  filter(startsWith(gsed4, "sf")) |>
  mutate(
    gsed4 = str_c(str_sub(gsed4, 1, 5), "s", str_sub(gsed4, 7)),
    gsed3 = str_c(str_sub(gsed3, 1, 5), "s", str_sub(gsed3, 7)),
    gsed2 = str_c(str_sub(gsed2, 1, 5), "s", str_sub(gsed2, 7)),
    gsed = str_c(str_sub(gsed, 1, 5), "s", str_sub(gsed, 7))
  )
builtin_translate <- bind_rows(builtin_translate, sf_s)

usethis::use_data(builtin_translate, overwrite = TRUE)
