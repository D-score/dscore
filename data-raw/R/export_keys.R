library(dscore)
library(dplyr)
library(openxlsx)

# define project
project <- path.expand("~/Package/dscore/dscore")
# project <- path.expand("~/OneDrive - TNO/Documents/GitHub/dscore")

# ------------- export dutch key
fn <- file.path(project, "data-raw/data/bds_edited.csv")
ib_dutch <- read.csv2(file = fn, stringsAsFactors = FALSE) |>
  mutate(key = "dutch") |>
  select(one_of(c("key", "lex_gsed", "tau"))) |>
  filter(tau != "") |>
  rename(item = lex_gsed)
fo <- file.path(project, "data-raw/data/keys/dutch.txt")
write.table(ib_dutch,
  file = fo, quote = FALSE, sep = "\t",
  na = "", row.names = FALSE
)

# ------------- export gcdg key
fn <- path.expand("~/Package/dscore/dscore/data-raw/data/gcdg_itembank.txt")
ib_gcdg <- read.delim(fn, stringsAsFactors = FALSE) |>
  mutate(
    key = "gcdg",
    item = dscore:::rename_gcdg_gsed(lex_gcdg),
    tau = round(tau, 2)
  ) |>
  select(one_of(c("key", "item", "tau")))
ib_gcdg <- ib_gcdg[order_itemnames(ib_gcdg$item), ]
fo <- file.path(project, "data-raw/data/keys/gcdg.txt")
write.table(ib_gcdg,
  file = fo, quote = FALSE, sep = "\t",
  na = "", row.names = FALSE
)

# ------------- export gsed1912 key
fn <- path.expand("~/Project/GSED/dmetric/models/807_17/model.Rds")
gsed_model_807_17 <- readRDS(file = fn)
ib_gsed <- gsed_model_807_17$itembank |>
  mutate(
    key = "gsed1912",
    tau = round(tau, 2)
  ) |>
  select(one_of("key", "item", "tau"))
ib_gsed <- ib_gsed[order_itemnames(ib_gsed$item), ]
fo <- file.path(project, "data-raw/data/keys/gsed1912.txt")
write.table(ib_gsed,
  file = fo, quote = FALSE, sep = "\t",
  na = "", row.names = FALSE
)

# ------------- export gsed2206 key
fn <- path.expand("~/Project/GSED/phase1/joint/818_17_joint_fixed/model.Rds")
gsed_model_818_17 <- readRDS(file = fn)
ib_gsed <- gsed_model_818_17$itembank |>
  mutate(
    key = "gsed2206",
    tau = round(tau, 2)
  ) |>
  select(one_of("key", "item", "tau"))
ib_gsed <- ib_gsed[order_itemnames(ib_gsed$item), ]
fo <- file.path(project, "data-raw/data/keys/gsed2206.txt")
write.table(ib_gsed,
  file = fo, quote = FALSE, sep = "\t",
  na = "", row.names = FALSE
)

# ------------- export 293_0 key
fn <- path.expand("~/Project/GSED/phase1/remodel/293_0/model.Rds")
gsed_model_293_0 <- readRDS(file = fn)
ib_gsed <- gsed_model_293_0$itembank |>
  mutate(
    key = "293_0",
    tau = round(tau, 2)
  ) |>
  select(one_of("key", "item", "tau"))
gpa <- ib_gsed[1:138, ]
gpa <- gpa[order_itemnames(gpa$item, order = "imnd"), ]
ib_gsed <- bind_rows(gpa, ib_gsed[139:293, ])
fo <- file.path(project, "data-raw/data/keys/293_0.txt")
write.table(ib_gsed,
  file = fo, quote = FALSE, sep = "\t",
  na = "", row.names = FALSE
)

# ------------- export gsed2208 key
fn <- path.expand("~/Project/GSED/phase1/remodel/818_6/model.Rds")
gsed_model_818_6 <- readRDS(file = fn)
ib_gsed <- gsed_model_818_6$itembank |>
  mutate(
    key = "gsed2208",
    tau = round(tau, 2)
  ) |>
  select(one_of("key", "item", "tau"))
ib_gsed <- ib_gsed[order_itemnames(ib_gsed$item), ]
fo <- file.path(project, "data-raw/data/keys/gsed2208.txt")
write.table(ib_gsed,
  file = fo, quote = FALSE, sep = "\t",
  na = "", row.names = FALSE
)


# ------------- export lf2206 key
fn <- path.expand("~/Project/GSED/phase1/lf/155_0/model.Rds")
gsed_model <- readRDS(file = fn)
ib_gsed <- gsed_model$itembank |>
  mutate(
    key = "lf2206",
    tau = round(tau, 2)
  ) |>
  select(one_of("key", "item", "tau"))
ib_gsed <- ib_gsed[order_itemnames(ib_gsed$item), ]
fo <- file.path(project, "data-raw/data/keys/lf2206.txt")
write.table(ib_gsed,
  file = fo, quote = FALSE, sep = "\t",
  na = "", row.names = FALSE
)

# ------------- export sf2206 key
fn <- path.expand("~/Project/GSED/phase1/sf/139_0/model.Rds")
gsed_model <- readRDS(file = fn)
ib_gsed <- gsed_model$itembank |>
  mutate(
    key = "sf2206",
    tau = round(tau, 2)
  ) |>
  select(one_of("key", "item", "tau"))
ib_gsed <- ib_gsed[order_itemnames(ib_gsed$item, order = "indm"), ]
fo <- file.path(project, "data-raw/data/keys/sf2206.txt")
write.table(ib_gsed,
  file = fo, quote = FALSE, sep = "\t",
  na = "", row.names = FALSE
)



# ---------- export ecdi keys

## model gsed2206 extended with ecdi items
fn <- path.expand("~/OneDrive - TNO/Documents/GitHub/decdi/models/gsed2206/ECDI_142_6_fixed/model.Rds")
ecdi_model <- readRDS(file = fn)
ib_ecdi <- ecdi_model$itembank |>
  filter(stringr::str_detect(item, "^ecd")) |>
  mutate(
    key = "gsed2206",
    tau = round(tau, 2)
  ) |>
  select(one_of("key", "item", "tau"))
ib_ecdi <- ib_ecdi[order_itemnames(ib_ecdi$item, order = "indm"), ]
fo <- file.path(project, "data-raw/data/keys/ecd2206.txt")
write.table(ib_ecdi,
  file = fo, quote = FALSE, sep = "\t",
  na = "", row.names = FALSE
)


## model 294_0 extende with ecdi items
fn <- path.expand("~/OneDrive - TNO/Documents/GitHub/decdi/models/294_0/ECDI_142_7_fixed/model.Rds")
ecdi_model <- readRDS(file = fn)
ib_ecdi <- ecdi_model$itembank |>
  filter(stringr::str_detect(item, "^ecd")) |>
  mutate(
    key = "294_0",
    tau = round(tau, 2)
  ) |>
  select(one_of("key", "item", "tau"))
ib_ecdi <- ib_ecdi[order_itemnames(ib_ecdi$item, order = "indm"), ]
fo <- file.path(project, "data-raw/data/keys/ecd294_0.txt")
write.table(ib_ecdi,
  file = fo, quote = FALSE, sep = "\t",
  na = "", row.names = FALSE
)



## model gsed2208 extended with ecdi items
fn <- path.expand("~/OneDrive - TNO/Documents/GitHub/decdi/models/gsed2208/ECDI_142_7_fixed/model.Rds")
ecdi_model <- readRDS(file = fn)
ib_ecdi <- ecdi_model$itembank |>
  filter(stringr::str_detect(item, "^ecd")) |>
  mutate(
    key = "gsed2208",
    tau = round(tau, 2)
  ) |>
  select(one_of("key", "item", "tau"))
ib_ecdi <- ib_ecdi[order_itemnames(ib_ecdi$item, order = "indm"), ]
fo <- file.path(project, "data-raw/data/keys/ecd2208.txt")
write.table(ib_ecdi,
  file = fo, quote = FALSE, sep = "\t",
  na = "", row.names = FALSE
)
