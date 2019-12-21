# script to create itembank mullen items
library(dplyr)
library(openxlsx)
library(gseddata)
library(dscore)

# load mullen keys
mm1 <- read.xlsx("data-raw/data/Mullen items with labels match to gcdg_180614.xlsx", sheet = 1)
mm1$domain <- "Gross Motor"
mm2 <- read.xlsx("data-raw/data/Mullen items with labels match to gcdg_180614.xlsx", sheet = 2)
mm2$domain <- "Visual Reception"
mm3 <- read.xlsx("data-raw/data/Mullen items with labels match to gcdg_180614.xlsx", sheet = 3)
mm3$domain <- "Fine Motor"
mm4 <- read.xlsx("data-raw/data/Mullen items with labels match to gcdg_180614.xlsx", sheet = 4)
mm4$domain <- "Receptive Language"
mm5 <- read.xlsx("data-raw/data/Mullen items with labels match to gcdg_180614.xlsx", sheet = 5)
mm5$domain <- "Expressive"
mm <- bind_rows(mm1, mm2, mm3, mm4, mm5)


items_gsed <- dscore::rename_gcdg_gsed(mm$item)

mullen_itemtable <- data.frame(
  item = items_gsed,
  decompose_itemnames(items_gsed),
  label = mm$label,
  stringsAsFactors = FALSE
)


usethis::use_data(mullen_itemtable, overwrite = TRUE)
