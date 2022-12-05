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

f1 <- "data-raw/data/keys/dutch.txt"
f2 <- "data-raw/data/keys/gcdg.txt"
f3 <- "data-raw/data/keys/gsed1912.txt"
f4 <- "data-raw/data/keys/mullen_itembank.txt"
f5 <- "data-raw/data/keys/gsed2206.txt"
f6 <- "data-raw/data/keys/lf2206.txt"
f7 <- "data-raw/data/keys/sf2206.txt"
f8 <- "data-raw/data/keys/294_0.txt"
f9 <- "data-raw/data/keys/ecd2206.txt"
f10 <- "data-raw/data/keys/ecd294_0.txt"
f11 <- "data-raw/data/keys/293_0.txt"
f12 <- "data-raw/data/keys/gsed2208.txt"
f13 <- "data-raw/data/keys/ecd2208.txt"
f14 <- "data-raw/data/keys/items_sf.txt"
f15 <- "data-raw/data/keys/gsed2212.txt"
f16 <- "data-raw/data/keys/items_gs1_gl1.txt"

key_dutch <- read.delim(file = f1, stringsAsFactors = FALSE)
key_dutch <- key_dutch[order_itemnames(key_dutch$item), ]

key_gcdg <- read.delim(file = f2, stringsAsFactors = FALSE)
key_gcdg <- key_gcdg[order_itemnames(key_gcdg$item), ]

key_gsed1912 <- read.delim(file = f3, stringsAsFactors = FALSE)
key_gsed1912 <- key_gsed1912[order_itemnames(key_gsed1912$item), ]

key_mullen <- read.delim(file = f4, stringsAsFactors = FALSE)
key_mullen$key <- "gsed1912"
key_mullen <- key_mullen[order_itemnames(key_mullen$item), ]

key_gsed2206 <- read.delim(file = f5, stringsAsFactors = FALSE)
key_gsed2206 <- key_gsed2206[order_itemnames(key_gsed2206$item), ]

key_lf2206 <- read.delim(file = f6, stringsAsFactors = FALSE)
key_lf2206 <- key_lf2206[order_itemnames(key_lf2206$item), ]

key_sf2206 <- read.delim(file = f7, stringsAsFactors = FALSE)
key_sf2206 <- key_sf2206[order_itemnames(key_sf2206$item, order = "indm"), ]

key_294_0 <- read.delim(file = f8, stringsAsFactors = FALSE)
items_gpa <- key_294_0$item[starts_with("gpa", vars = key_294_0$item)]
items_gto <- key_294_0$item[starts_with("gto", vars = key_294_0$item)]
key_294_0_gto <- key_294_0 %>% filter(item %in% items_gto)
key_294_0_gto <- key_294_0_gto[order_itemnames(key_294_0_gto$item), ]
key_294_0_gpa <- key_294_0 %>% filter(item %in% items_gpa)
key_294_0_gpa <- key_294_0_gpa[order_itemnames(key_294_0_gpa$item, order = "indm"), ]
key_294_0 <- bind_rows(key_294_0_gto,
                       key_294_0_gpa)

key_ecd2206 <- read.delim(file = f9, stringsAsFactors = FALSE)
key_ecd2206 <- key_ecd2206[order_itemnames(key_ecd2206$item), ]

key_ecd294_0 <- read.delim(file = f10, stringsAsFactors = FALSE)
key_ecd294_0 <- key_ecd294_0[order_itemnames(key_ecd294_0$item), ]

key_293_0 <- read.delim(file = f11, stringsAsFactors = FALSE)

key_gsed2208 <- read.delim(file = f12, stringsAsFactors = FALSE)
key_gsed2208 <- key_gsed2208[order_itemnames(key_gsed2208$item), ]

key_ecd2208 <- read.delim(file = f13, stringsAsFactors = FALSE)
key_ecd2208 <- key_ecd2208[order_itemnames(key_ecd2208$item), ]

key_sf12 <- read.delim(file = f14, stringsAsFactors = FALSE)
key_sf12 <- key_sf12[order_itemnames(key_sf12$item, order = "imnd"), ]

key_gsed2212 <- read.delim(file = f15, stringsAsFactors = FALSE)
key_gsed2212 <- key_gsed2212[order_itemnames(key_gsed2212$item), ]

key_gsed2212_gs1_gl1 <- read.delim(file = f16, stringsAsFactors = FALSE) %>%
  select(key, item, tau)

# --- key2212
# Extend 293_0 key with model items 818_6 (version 20221201_remodel)
# Add gs1 and gs2 instrument names (gpa=gs1)
# Add ecdi
# Save as gsed2208
key_gsed2212 <- bind_rows(key_gsed2212_gs1_gl1,
                          key_293_0, key_gsed2212, key_ecd2208) %>%
  mutate(key = "gsed2212") %>%
  select(key, item, tau)
check_single_key(key_gsed2212)

# --- key2208
# Extend 293_0 key with 818 items from the previous model 818_17
# Add gs1 and gs2 instrument names (gpa=gs1)
# Add ecdi
# Save as gsed2208
# Superseeded by gsed2212 because of LF item order problem - do not use
key_gsed2208 <- bind_rows(key_sf12, key_293_0, key_gsed2208, key_ecd2208) %>%
  mutate(key = "gsed2208") %>%
  select(key, item, tau)
check_single_key(key_gsed2208)

# --- key2206 Superseeded by key2208 - do not use
# Extend gsed2206 with gsed2 item names
lf_gsed <- gsedread::rename_vector(key_lf2206$item, lexin = "gsed2", lexout = "gsed")
sf_gsed <- gsedread::rename_vector(key_sf2206$item, lexin = "gsed2", lexout = "gsed")
lfsf_gsed <- gsedread::rename_vector(key_294_0$item, lexin = "gsed2", lexout = "gsed")
lf_tau <- dscore::get_tau(lf_gsed, key = "gsed2206", itembank = key_gsed2206)
sf_tau <- dscore::get_tau(sf_gsed, key = "gsed2206", itembank = key_gsed2206)
key_gsed2206 <- bind_rows(key_gsed2206,
                          data.frame(key = "gsed2206", item = key_lf2206$item, tau = lf_tau),
                          data.frame(key = "gsed2206", item = key_sf2206$item, tau = sf_tau),
                          key_ecd2206) %>%
  filter(!is.na(tau))
check_single_key(key_gsed2206)

# --- key1912 (807 items)
key_gsed1912 <- bind_rows(key_gsed1912,
                          key_mullen)
check_single_key(key_gsed1912)

# --- key_gcdg (565 items)
check_single_key(key_gcdg)

# --- key_lf2206 Deprecated --> gsed2208
# Extend lf2206 with gsed item names
key_lf2206 <- bind_rows(key_lf2206,
                        data.frame(key = "lf2206", item = lf_gsed, tau = key_lf2206$tau))
check_single_key(key_lf2206)

# --- key_sf2206 Deprecated --> gsed2208
# Extend sf2206 with gsed item names
key_sf2206 <- bind_rows(key_sf2206,
                        data.frame(key = "sf2206", item = sf_gsed, tau = key_sf2206$tau))
check_single_key(key_sf2206)

# --- key_294_0 DEPRECATED --> key_293_0
# Extend 294_0 with gsed item names
key_294_0 <- bind_rows(key_294_0,
                       data.frame(key = "294_0", item = lfsf_gsed, tau = key_294_0$tau),
                       key_ecd294_0)
check_single_key(key_294_0)

# --- key_293_9 GSED CORE MODEL (part of key_gsed2208)
check_single_key(key_293_0)

# --- key_dutch (76 items)
check_single_key(key_dutch)


builtin_itembank <- bind_rows(key_gsed2212,
                              key_gsed2208,
                              key_gsed2206,
                              key_gsed1912,
                              key_gcdg,
                              key_lf2206,
                              key_sf2206,
                              key_294_0,
                              key_293_0,
                              key_dutch) %>%
  left_join(get_itemtable(decompose = TRUE), by = "item") %>%
  select(-equate)

# save to /data
usethis::use_data(builtin_itembank, overwrite = TRUE)
