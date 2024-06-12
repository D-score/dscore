# Save built-in itembank for items in the model
# Fields: key, item, tau, instrument, domain, mode, number, label
library(dplyr)
library(dscore)

check_single_key <- function(x) {
  ok <- TRUE
  if (any(duplicated(x$item))) {
    warning("Duplicated items found.")
    ok <- FALSE
  }
  if (length(unique(x$key)) > 1L) {
    warning("Only one key allowed.")
    ok <- FALSE
  }
  if (any(is.na(x$tau))) {
    warning("Missing tau detected.")
    ok <- FALSE
  }
  if (ok) cat("Key", unique(x$key), "OK.\n")
  invisible(ok)
}

key_file <- c(
  dutch = "dutch.txt",
  gcdg = "gcdg.txt",
  gsed1912 = "gsed1912.txt",
  gsed1912_mul = "gsed1912_mul.txt",
  lf2206 = "lf2206.txt",
  sf2206 = "sf2206.txt",
  gsed2206 = "gsed2206.txt",
  gsed2206_ecd = "gsed2206_ecd.txt",
  "294_0" = "294_0.txt",
  "294_0_ecd" = "294_0_ecd.txt",
  "293_0" = "293_0.txt",
  gsed2208 = "gsed2208.txt",
  gsed2208_ecd = "gsed2208_ecd.txt",
  gsed2208_gs1_gs2 = "gsed2208_gs1_gs2.txt",
  gsed2212 = "gsed2212.txt",
  gsed2212_gs1_gl1 = "gsed2212_gs1_gl1.txt",
  gsed2212_gh1 = "gsed2212_gh1.txt"
)
key_name <- names(key_file)
key_path <- "data-raw/data/keys"
key_file <- file.path(key_path, key_file)
names(key_file) <- key_name

# read key files

key_dutch <- read.delim(file = key_file["dutch"])
key_dutch <- key_dutch[order_itemnames(key_dutch$item), ]

key_gcdg <- read.delim(file = key_file["gcdg"])
key_gcdg <- key_gcdg[order_itemnames(key_gcdg$item), ]

key_gsed1912 <- read.delim(file = key_file["gsed1912"])
key_gsed1912 <- key_gsed1912[order_itemnames(key_gsed1912$item), ]

key_gsed1912_mul <- read.delim(file = key_file["gsed1912_mul"])
key_gsed1912_mul <- key_gsed1912_mul[order_itemnames(key_gsed1912_mul$item), ]

key_lf2206 <- read.delim(file = key_file["lf2206"])
key_lf2206 <- key_lf2206[order_itemnames(key_lf2206$item), ]

key_sf2206 <- read.delim(file = key_file["sf2206"])
key_sf2206 <- key_sf2206[order_itemnames(key_sf2206$item, order = "indm"), ]

key_gsed2206 <- read.delim(file = key_file["gsed2206"])
key_gsed2206 <- key_gsed2206[order_itemnames(key_gsed2206$item), ]

key_gsed2206_ecd <- read.delim(file = key_file["gsed2206_ecd"])
key_gsed2206_ecd <- key_gsed2206_ecd[order_itemnames(key_gsed2206_ecd$item), ]

key_294_0 <- read.delim(file = key_file["294_0"])
items_gpa <- key_294_0$item[starts_with("gpa", vars = key_294_0$item)]
items_gto <- key_294_0$item[starts_with("gto", vars = key_294_0$item)]
key_294_0_gto <- key_294_0 |> filter(item %in% items_gto)
key_294_0_gto <- key_294_0_gto[order_itemnames(key_294_0_gto$item), ]
key_294_0_gpa <- key_294_0 |> filter(item %in% items_gpa)
key_294_0_gpa <- key_294_0_gpa[order_itemnames(key_294_0_gpa$item, order = "indm"), ]
key_294_0 <- bind_rows(
  key_294_0_gto,
  key_294_0_gpa
)

key_294_0_ecd <- read.delim(file = key_file["294_0_ecd"])
key_294_0_ecd <- key_294_0_ecd[order_itemnames(key_294_0_ecd$item), ]

key_293_0 <- read.delim(file = key_file["293_0"])

key_gsed2208 <- read.delim(file = key_file["gsed2208"])
key_gsed2208 <- key_gsed2208[order_itemnames(key_gsed2208$item), ]

key_gsed2208_ecd <- read.delim(file = key_file["gsed2208_ecd"])
key_gsed2208_ecd <- key_gsed2208_ecd[order_itemnames(key_gsed2208_ecd$item), ]

key_gsed2208_gs1_gs2 <- read.delim(file = key_file["gsed2208_gs1_gs2"])
key_gsed2208_gs1_gs2 <- key_gsed2208_gs1_gs2[order_itemnames(key_gsed2208_gs1_gs2$item, order = "imnd"), ]

key_gsed2212 <- read.delim(file = key_file["gsed2212"])
key_gsed2212 <- key_gsed2212[order_itemnames(key_gsed2212$item), ]

key_gsed2212_gs1_gl1 <- read.delim(file = key_file["gsed2212_gs1_gl1"])

key_gsed2212_gh1 <- read.delim(file = key_file["gsed2212_gh1"])

# --- key_dutch (76 items)
check_single_key(key_dutch)

# --- key_gcdg (565 items)
check_single_key(key_gcdg)

# --- key1912 (807 items)
key_gsed1912 <- bind_rows(
  key_gsed1912,
  key_gsed1912_mul
)
check_single_key(key_gsed1912)

# --- key_lf2206 Deprecated --> gsed2212
# Extend lf2206 with gsed item names
lf_gsed <- gsedread::rename_vector(key_lf2206$item, lexin = "gsed2", lexout = "gsed")
key_lf2206 <- bind_rows(
  key_lf2206,
  data.frame(key = "lf2206", item = lf_gsed, tau = key_lf2206$tau)
)
check_single_key(key_lf2206)

# --- key_sf2206 Deprecated --> gsed2212
# Extend sf2206 with gsed item names
sf_gsed <- gsedread::rename_vector(key_sf2206$item, lexin = "gsed2", lexout = "gsed")
key_sf2206 <- bind_rows(
  key_sf2206,
  data.frame(key = "sf2206", item = sf_gsed, tau = key_sf2206$tau)
)
check_single_key(key_sf2206)

# --- key2206 Superseeded by key2208 - do not use
# Extend gsed2206 with gsed2 item names
lf_tau <- dscore::get_tau(lf_gsed, key = "gsed2206", itembank = key_gsed2206)
sf_tau <- dscore::get_tau(sf_gsed, key = "gsed2206", itembank = key_gsed2206)
key_gsed2206 <- bind_rows(
  key_gsed2206,
  data.frame(key = "gsed2206", item = lf_gsed, tau = lf_tau),
  data.frame(key = "gsed2206", item = sf_gsed, tau = sf_tau),
  key_gsed2206_ecd
) |>
  filter(!is.na(tau))
check_single_key(key_gsed2206)

# --- key_294_0 DEPRECATED --> key_293_0
# Extend 294_0 with gsed item names
lfsf_gsed <- gsedread::rename_vector(key_294_0$item, lexin = "gsed2", lexout = "gsed")
key_294_0 <- bind_rows(
  key_294_0,
  data.frame(key = "294_0", item = lfsf_gsed, tau = key_294_0$tau),
  key_294_0_ecd
)
check_single_key(key_294_0)

# --- key_293_0 GSED CORE MODEL (part of key_gsed2212)
check_single_key(key_293_0)

# --- key2208
# Extend 293_0 key with 818 items from the previous model 818_17
# Add gs1 and gs2 instrument names (gpa=gs1)
# Add ecdi
# Save as gsed2208
# Superseeded by gsed2212 because of LF item order problem - do not use
key_gsed2208 <- bind_rows(key_gsed2208_gs1_gs2, key_293_0, key_gsed2208, key_gsed2208_ecd) |>
  mutate(key = "gsed2208") |>
  select(key, item, tau)
check_single_key(key_gsed2208)

# --- key2212
# Extend 293_0 key with model items 818_6 (version 20221201_remodel)
# Add gs1 and gs2 instrument names (gpa=gs1)
# Add ecdi
# Save as gsed2212
key_gsed2212 <- bind_rows(
  key_gsed2212_gs1_gl1,
  key_gsed2212_gh1,
  key_293_0,
  key_gsed2212,
  key_gsed2208_ecd) |>
  mutate(key = "gsed2212") |>
  select(key, item, tau)
check_single_key(key_gsed2212)

# --- key2406
# Creat copy of gsed2212 to make D-score calculation consistent
# with the adoption of the preliminary_standards
key_gsed2406 <- key_gsed2212 |>
  mutate(key = "gsed2406")
check_single_key(key_gsed2406)

# --- Build itembank in reverse history order
builtin_itembank <- bind_rows(
  key_gsed2406,
  key_gsed2212,
  key_293_0,
  key_gsed1912,
  key_gcdg,
  key_dutch) |>
  left_join(get_itemtable(decompose = TRUE), by = "item") |>
  select(-equate)

# save to /data
usethis::use_data(builtin_itembank, overwrite = TRUE)

