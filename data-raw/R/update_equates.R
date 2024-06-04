# This script compares the equate groups defined in
# ddata::itemtable and dscore::itemtable
library(ddata) # V0.50.0
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
it$label <- it$label.x

it <- it[, c("item", "equate", "label")]
it <- it[gtools::mixedorder(it$item), ]
rownames(it) <- NULL

# remove gremlins
it[121, 3] <- "Draw a 4-inch circle on a piece of paper. Does your child use child-safe scissors to cut it out staying within a 1/4 inch of the lines? (Carefully watch your child's use of scissors for safety reasons.)"
it[251, 3] <- "Can your child count past '40'?"
it[252, 3] <- "Does your child correctly spell 3-letter words? For example, 'cat', 'dog',  'pen'."
it[253, 3] <- "Can your child tell you all 12 months of the year? Mark 'Sometimes' if your child can tell you more than 6 months of the year."
it[255, 3] <- "Can your child count to 100 by 10's?"
it[2781, 3] <- "Does your child stop what he/she is doing when you say 'Stop!' even if just for a second?"
it[2782, 3] <- "Does your child make a gesture to indicate 'No'?"
it[2853, 3] <- "Can your child greet people either by giving his/her hand or saying 'hello'?"
it[2972, 3] <- "Three-hole board - one in, two trials."
it[2973, 3] <- "Three-hole board - three in."

write.table(
  x = it, file = "data-raw/data/itemtable_20200424.txt",
  quote = FALSE, sep = "\t", na = "", row.names = FALSE,
  fileEncoding = "UTF-8"
)
