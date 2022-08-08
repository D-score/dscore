# Save built-in itemtable
# Fields: item, equate, label

fn <- file.path("data-raw/data/itemtable_20220601.txt")
builtin_itemtable <- read.delim(file = fn, quote = "",
                                stringsAsFactors = FALSE, na = "",
                                fileEncoding = "UTF-8",
                                header = TRUE)

## add ecdi items to itemtable
ecdi_itemtable <- read.xlsx("data-raw/data/ecdi_itemtable.xlsx", sheet = "ecdi")

ecdi_itemtable <-
ecdi_itemtable %>% select(item, label) %>%
  mutate(equate = NA) %>%
  select(item, equate, label)

builtin_itemtable <-
bind_rows(builtin_itemtable,
          ecdi_itemtable) %>%
  mutate(equate = ifelse(item %in% c("ecdxxc001", "gpamoc097"), "ECD1", NA),
         equate = ifelse(item %in% c("ecdxxc002", "gpamoc106"), "ECD2", equate),
         equate = ifelse(item %in% c("ecdxxc003", "gpamoc129"), "ECD3", equate),
         equate = ifelse(item %in% c("ecdxxc004", "gpamoc132"), "ECD4", equate),
         equate = ifelse(item %in% c("ecdxxc005", "gpacmc090"), "ECD5", equate),
         equate = ifelse(item %in% c("ecdxxc006", "gpaclc112"), "ECD6", equate),
         equate = ifelse(item %in% c("ecdxxc008", "gpaclc113"), "ECD8", equate),
         equate = ifelse(item %in% c("ecdxxc009", "gpaclc101"), "ECD9", equate),
         equate = ifelse(item %in% c("ecdxxc013", "gpaclc126"), "ECD13", equate))


usethis::use_data(builtin_itemtable, overwrite = TRUE)
