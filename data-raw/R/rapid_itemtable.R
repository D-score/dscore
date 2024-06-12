# script to create itembank mullen items
library(dplyr)
library(openxlsx)
library(gseddata)
library(dscore)
library(stringr)

sf <- read.xlsx("data-raw/data/Master data dictionary - Rapid V1.1 KB lex_gsed.xlsx", sheet = "Short form (wide)")[10:148, ]
colnames(sf) <- c("lex_sf", "form names", "type", "label", "values", "age_cat", "lex_gsed")

sf$lex_sf

rename_sf <- function(x, match_table) {
  match_y <- match_table[match(x, match_table[, "lex_sf"]), "lex_gsed"]
  deco <- decompose_itemnames(match_y)
  instr <- "rap"
  domn <- deco$domain
  domn[is.na(match_y)] <- "xx"
  rep <- "c"
  nr <- gsub("Ra_SF", "", x)
  nr <- stri_pad(nr, 3, pad = "0")

  y <- paste(instr, domn, rep, nr, sep = "")
  y
}

items_gsed <- rename_sf(sf$lex_sf, sf)

rapid_itemtable <- data.frame(
  item = items_gsed,
  decompose_itemnames(items_gsed),
  label = sf$label,
  stringsAsFactors = FALSE
)


usethis::use_data(rapid_itemtable, overwrite = TRUE)
