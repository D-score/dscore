# Save built-in itemtable
# Fields: item, equate, label
library(dplyr)
fn <- file.path("data-raw/data/itemtable_20221201.txt")
builtin_itemtable <- read.delim(
  file = fn, quote = "",
  stringsAsFactors = FALSE, na = "",
  fileEncoding = "UTF-8",
  header = TRUE
)

## Create LF and SF item codes corresponding to published
## GSED v1.0 Short Form and GSED v1.0 Long Form
## NOTE: tab also contains crosswalk to gsed and gsed2 lexicon
tab <- read.delim("data-raw/data/items/phase2_items.txt",
                  stringsAsFactors = FALSE, na = "",
                  fileEncoding = "UTF-8",
                  header = TRUE)
items_sf_ <- tab |>
  filter(instrument == "gs1") |>
  mutate(item = paste0("sf_", domain, mode, formatC(number, width = 3, flag = "0")),
         equate = NA_character_,
         label = paste(paste0("SF", formatC(number, width = 3, flag = "0")), label)) |>
  select(item, equate, label)
# Add SF item names for selfreport mode s
items_sf_s <- tab |>
  filter(instrument == "gs1") |>
  mutate(item = paste0("sf_", domain, "s", formatC(number, width = 3, flag = "0")),
         equate = NA_character_,
         label = paste(paste0("SF", formatC(number, width = 3, flag = "0")), label)) |>
  select(item, equate, label)
items_gs1 <- tab |>
  filter(instrument == "gs1") |>
  mutate(item = paste0("gs1", domain, mode, formatC(number, width = 3, flag = "0")),
         equate = NA_character_,
         label = paste(paste0("SF", formatC(number, width = 3, flag = "0")), label)) |>
  select(item, equate, label)
items_lf_ <- tab |>
  filter(instrument == "gl1") |>
  mutate(item = paste0("lf", c(rep("a", 49), rep("b", 52), rep("c", 54)),
                       domain, "d", formatC(number, width = 3, flag = "0")),
         equate = NA_character_,
         label = label) |>
  select(item, equate, label)
items_gl1 <- tab |>
  filter(instrument == "gl1") |>
  mutate(item = paste0("gl1", domain, "d", formatC(number, width = 3, flag = "0")),
         equate = NA_character_,
         label = label) |>
  select(item, equate, label)

gsx_itemtable <- bind_rows(items_sf_, items_sf_s, items_lf_,
                           items_gs1, items_gl1)

## add ecdi items to itemtable
ecdi_itemtable <- read.delim("data-raw/data/ecdi_itemtable.txt")
ecdi_itemtable <- ecdi_itemtable |>
  select(item, label) |>
  mutate(equate = NA_character_) |>
  select(item, equate, label) |>
  mutate(
    equate = ifelse(item %in% c("ecdxxc001", "gpamoc097"), "ECD1", NA),
    equate = ifelse(item %in% c("ecdxxc002", "gpamoc106"), "ECD2", equate),
    equate = ifelse(item %in% c("ecdxxc003", "gpamoc129"), "ECD3", equate),
    equate = ifelse(item %in% c("ecdxxc004", "gpamoc132"), "ECD4", equate),
    equate = ifelse(item %in% c("ecdxxc005", "gpacmc090"), "ECD5", equate),
    equate = ifelse(item %in% c("ecdxxc006", "gpaclc112"), "ECD6", equate),
    equate = ifelse(item %in% c("ecdxxc008", "gpaclc113"), "ECD8", equate),
    equate = ifelse(item %in% c("ecdxxc009", "gpaclc101"), "ECD9", equate),
    equate = ifelse(item %in% c("ecdxxc013", "gpaclc126"), "ECD13", equate)
  )

## add HF items to itemtable, creates instrument code gh1, overwrites item
hh_itemtable <- openxlsx::read.xlsx("data-raw/data/ageforms_2025-07-15.xlsx")
info <- dscore::decompose_itemnames(hh_itemtable$item)
info$instrument <- "gh1"
info$domain <- recode(hh_itemtable$voted_domain, cog = "cg", lang = "lg", life = "li", motor = "mo", sem = "se")
info$number <- formatC(1:55, width = 3, flag = "0")
hh_itemtable$item <- with(info, paste0(instrument, domain, mode, number))
hh_itemtable <- hh_itemtable |>
  mutate(equate = NA_character_) |>
  select(item, equate, label)

builtin_itemtable <- bind_rows(
  gsx_itemtable,
  builtin_itemtable,
  ecdi_itemtable,
  hh_itemtable,
)

info <- dscore::decompose_itemnames(builtin_itemtable$item)
builtin_itemtable <- builtin_itemtable |>
  bind_cols(info) |>
  arrange(instrument, domain, mode, number) |>
  select(item, equate, label)

# check
if (any(duplicated(builtin_itemtable$item))) cat("Duplicated items found.")

usethis::use_data(builtin_itemtable, overwrite = TRUE)

