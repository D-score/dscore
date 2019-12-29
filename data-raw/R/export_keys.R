library(dscore)
library(dplyr)
library(openxlsx)

# define project
project <- path.expand("~/Package/dscore/dscore")

# ------------- export dutch key
fn <- file.path(project, "data-raw/data/bds_edited.csv")
ib_dutch <- read.csv2(file = fn, stringsAsFactors = FALSE) %>%
  mutate(key = "dutch") %>%
  select(one_of(c("key", "lex_gsed", "tau"))) %>%
  filter(tau != "") %>%
  rename(item = lex_gsed)
fo <- file.path(project, "data-raw/data/keys/dutch.txt")
write.table(ib_dutch,
  file = fo, quote = FALSE, sep = "\t",
  na = "", row.names = FALSE
)

# ------------- export gcdg key
fn <- path.expand("~/Package/dscore/dscore/data-raw/data/gcdg_itembank.txt")
ib_gcdg <- read.delim(fn, stringsAsFactors = FALSE) %>%
  mutate(
    key = "gcdg",
    item = dscore:::rename_gcdg_gsed(lex_gcdg),
    tau = round(tau, 2)
  ) %>%
  select(one_of(c("key", "item", "tau")))
ib_gcdg <- ib_gcdg[order_itemnames(ib_gcdg$item), ]
fo <- file.path(project, "data-raw/data/keys/gcdg.txt")
write.table(ib_gcdg,
  file = fo, quote = FALSE, sep = "\t",
  na = "", row.names = FALSE
)

# ------------- export gsed key
fn <- path.expand("~/Project/GSED/dmetric/models/807_17/model.Rds")
gsed_model_807_17 <- readRDS(file = fn)
ib_gsed <- gsed_model_807_17$itembank %>%
  mutate(
    key = "gsed",
    tau = round(tau, 2)
  ) %>%
  select(one_of("key", "item", "tau"))
ib_gsed <- ib_gsed[order_itemnames(ib_gsed$item), ]
fo <- file.path(project, "data-raw/data/keys/gsed.txt")
write.table(ib_gsed,
  file = fo, quote = FALSE, sep = "\t",
  na = "", row.names = FALSE
)

