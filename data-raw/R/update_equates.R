# This script compares the equate groups defined in
# ddata::itemtable and dscore::itemtable
library(ddata)  # V0.50.0
it <- ddata::itemtable
rownames(it) <- NULL

# translate items to gsed lexicon
translate_fail <- c(1777:1796, 1823:1842, 1869:1908, 1939:1958)
it$gsed <- it$item
it$gsed[translate_fail] <- NA_character_
it$gsed <- dscore::rename_gcdg_gsed(it$gsed)

gsed_fail <- c(1:624, 2626:2750, 2852:2895, 3018:3022, 3106:3328)
it$gsed[gsed_fail] <- NA_character_

it_ddata <- it[!is.na(it$gsed), ]

# remove 7 duplicates
dup <- c(1414, 1415, 1421, 2563, 2581, 2937, 3000)
it_ddata <- it_ddata[!rownames(it_ddata) %in% dup, ]

it_ddata$gcdg <- it_ddata$item
it_ddata$item <- it_ddata$gsed

# itemtable from ddata 0.50.0, translated and cleaned

# match to builtin_itemtable

library(dplyr)
it_dscore <- dscore::get_itemtable()
# it_dscore[duplicated(it_dscore$item), ]

it <- full_join(it_dscore, it_ddata, by = "item")

# after visual inspection:
# - copy all equates from it_ddata to it equate.x
# - copy labels for rows 2652:3173 to labels.x
# - rename, sort, expand and save

it$equate.x <- it$equate.y
it$label.x[2652:3173] <- it$label.y[2652:3173]

it$equate <- it$equate.x
it$label  <- it$label.x

it <- it[, c("item", "equate", "label")]
it <- it[gtools::mixedorder(it$item), ]
rownames(it) <- NULL

write.table(x = it, file = "data-raw/data/itemtable_20200424.txt",
           quote = FALSE, sep = "\t", na = "", row.names = FALSE)

