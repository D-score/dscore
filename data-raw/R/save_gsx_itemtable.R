# Processes SF item order info, and save table in keys
library(dplyr)
library(dscore)

fn <- file.path("data-raw/data/SF_LF_Phase_2_Item_Ordering.txt")
io <- read.delim(file = fn, quote = "",
                 stringsAsFactors = FALSE, na = "",
                 fileEncoding = "UTF-8",
                 header = TRUE)

# Note: io$tau is taken from key 294_0. This should be key 293_0.

colnames(io) <- c("start", "ph2", "ph1", "tau", "label", "domain", "gsed2", "gsed1")

# info <- dscore::decompose_itemnames(io$gsed2)
# table(info$domain)
# unique(io$domain)

io$domain <- recode(io$domain, sem = "se", motor = "mo", lang = "lg", cog = "cg", life = "li")

# construct item names sf1 and sf2
# gs1: GSED SF Version 1 (Phase 1)
# gs2: GSED SF Version 2 (Phase 2)

# select gsed2008 tau values
ib <- dplyr::filter(builtin_itembank, key == "gsed2208") %>%
  dplyr::select(all_of(c("key", "item", "tau", "label")))

# create gs2 itembank part
gs2_names <- paste0("gs2", io$domain, "c", formatC(1:139, width = 3, flag = "0"))
gs2 <- data.frame(gs2_names, item = io$gsed2)
gs2 <- left_join(x = gs2, y = ib, by = "item")
gs2 <- data.frame(
  key = "gsed2208",
  item = gs2$gs2_names,
  tau = gs2$tau,
  label = gs2$label,
  decompose_itemnames(gs2$gs2_names))
gs2[28, "label"] <- "Does your child hold his/her hands in fists all the time?"

# create gs1 itembank part
info <- dscore::decompose_itemnames(io$gsed2)
idx <- order(info$number)
io1 <- io[idx, ]
gs1_names <- paste0("gs1", io1$domain, "c", formatC(1:139, width = 3, flag = "0"))
gs1 <- data.frame(gs1_names, item = io1$gsed2)
gs1 <- left_join(x = gs1, y = ib, by = "item")
gs1 <- data.frame(
  key = "gsed2208",
  item = gs1$gs1_names,
  tau = gs1$tau,
  label = gs1$label,
  decompose_itemnames(gs1$gs1_names))
gs1[8, "label"] <- "Does your child hold his/her hands in fists all the time?"

gsx <- bind_rows(gs1, gs2)

write.table(gsx, file = "data-raw/data/keys/items_sf.txt",
            quote = FALSE, sep = "\t", row.names = FALSE)
