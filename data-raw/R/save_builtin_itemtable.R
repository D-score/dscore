# Save built-in itemtable
# Fields: item, equate, label

fn <- file.path("data-raw/data/itemtable_20220601.txt")
builtin_itemtable <- read.delim(file = fn, quote = "",
                                stringsAsFactors = FALSE, na = "",
                                fileEncoding = "UTF-8",
                                header = TRUE)
usethis::use_data(builtin_itemtable, overwrite = TRUE)
