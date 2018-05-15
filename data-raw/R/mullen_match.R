## move this to dscore package?
library(dscore)
library(openxlsx)
library(dplyr)
nrow(dscore::gcdg_itembank)
write.xlsx(gcdg_itembank, "data-raw/data/gcdg_itembank.xlsx")

mm1 <- read.xlsx("data-raw/data/Mullen items with labels match to gcdg.xlsx", sheet=1)
mm1$domain <- "Gross Motor"
mm2 <- read.xlsx("data-raw/data/Mullen items with labels match to gcdg.xlsx", sheet=2)
mm2$domain <- "Visual Reception"
mm3 <- read.xlsx("data-raw/data/Mullen items with labels match to gcdg.xlsx", sheet=3)
mm3$domain <- "Fine Motor"
mm4 <- read.xlsx("data-raw/data/Mullen items with labels match to gcdg.xlsx", sheet=4)
mm4$domain <- "Receptive Language"
mm5 <- read.xlsx("data-raw/data/Mullen items with labels match to gcdg.xlsx", sheet=5)
mm5$domain <- "Expressive"
mm <- bind_rows(mm1,mm2,mm3,mm4,mm5)

head(gcdg_itembank)

#match the mullenmatch to the itembank_lex and get tau for these items from reference
mm$tau <- gcdg_itembank[match(mm$gcdg_item, table=as.character(gcdg_itembank$lex_gcdg)),"tau"]
#checks
#head(mm)
#gcdg_itembank[460:490,]

mm$instrument <- "Mullen"
mm$lex_gcdg <- mm$item
mm$max <- mm$gcdg_item <- mm$item <-  NULL
gcdg_itembank_mm <- bind_rows(gcdg_itembank,mm)
tail(gcdg_itembank_mm)



