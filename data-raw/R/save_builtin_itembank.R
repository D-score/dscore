# Save built-in itembank for items in the model
# Fields: key, item, tau, instrument, domain, mode, number, label
library(dscore)
library(dplyr)

# define project
project <- path.expand("~/OneDrive - TNO/Documents/Github/dscore")
#project <- path.expand("~/Package/dscore/dscore")

f1 <- file.path(project, "data-raw/data/keys/dutch.txt")
f2 <- file.path(project, "data-raw/data/keys/gcdg.txt")
f3 <- file.path(project, "data-raw/data/keys/gsed1912.txt")
f4 <- file.path(project, "data-raw/data/keys/mullen_itembank.txt")
f5 <- file.path(project, "data-raw/data/keys/gsed2206.txt")
f6 <- file.path(project, "data-raw/data/keys/lf2206.txt")
f7 <- file.path(project, "data-raw/data/keys/sf2206.txt")
f8 <- file.path(project, "data-raw/data/keys/294_0.txt")
f9 <- file.path(project, "data-raw/data/keys/ecd2206.txt")
f10 <- file.path(project, "data-raw/data/keys/ecd294_0.txt")

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


# Extend gsed2206 with gsed2 item names
lf_gsed <- gsedread::rename_vector(key_lf2206$item, lexin = "gsed2", lexout = "gsed")
sf_gsed <- gsedread::rename_vector(key_sf2206$item, lexin = "gsed2", lexout = "gsed")
lfsf_gsed <- gsedread::rename_vector(key_294_0$item, lexin = "gsed2", lexout = "gsed")
lf_tau <- dscore::get_tau(lf_gsed, itembank = key_gsed2206)
sf_tau <- dscore::get_tau(sf_gsed, itembank = key_gsed2206)

key_gsed2206 <- bind_rows(key_gsed2206,
                          data.frame(key = "gsed2206", item = key_lf2206$item, tau = lf_tau),
                          data.frame(key = "gsed2206", item = key_sf2206$item, tau = sf_tau))

# Extend lf2206 with gsed item names
key_lf2206 <- bind_rows(key_lf2206,
                        data.frame(key = "lf2206", item = lf_gsed, tau = key_lf2206$tau))
# Extend sf2206 with gsed item names
key_sf2206 <- bind_rows(key_sf2206,
                        data.frame(key = "sf2206", item = sf_gsed, tau = key_sf2206$tau))

# Extend 294_0 with gsed item names
key_294_0 <- bind_rows(key_294_0,
                       data.frame(key = "294_0", item = lfsf_gsed, tau = key_294_0$tau))

builtin_itembank <- bind_rows(key_gsed2206, key_ecd2206, key_gsed1912, key_lf2206, key_sf2206,
                              key_294_0, key_ecd294_0,
                              key_mullen, key_gcdg, key_dutch) %>%
  left_join(get_itemtable(decompose = TRUE), by = "item") %>%
  select(-equate)



# save to /data
usethis::use_data(builtin_itembank, overwrite = TRUE)
