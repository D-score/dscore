# ADDED DEC 1, 2022, SVB
# NOTE: THIS SCRIPT IS INVALIDATED BY WRONG ITEM MAPPING IN LF_stem.txt
# DO NOT USE

# Processes LF item order info, and save table in keys
library(dplyr)
library(dscore)

# Read item reordering Jun 22 (LF1) --> Oct 22 (LF2)
fn <- file.path("data-raw/data/LF_stem.txt")
lf <- read.delim(file = fn, quote = "", sep = "\t",
                 stringsAsFactors = FALSE, na = "",
                 fileEncoding = "UTF-8",
                 header = TRUE)
lf <- data.frame(stream = c(rep("A", 49), rep("B", 52), rep("C", 54)),
                 lf2 = substr(lf$new, 2, 10),
                 lf1 = substr(lf$old, 2, 10),
                 stem = lf$stem)

# Fetch proper names and tau estimates
items <- get_itemnames(instrument = "gto")
items <- items[c(55:103, 104:155, 1:54)]  # reorder along streams A,B,C

# sort gto rows in new order
matched_item <- c(
  items[1:49][as.numeric(lf[1:49, "lf1"])],
  items[50:101][as.numeric(lf[50:101, "lf1"])],
  items[102:155][as.numeric(lf[102:155, "lf1"])])
matched_label <- get_labels(matched_item)
matched_tau <- get_tau(matched_item, key = "gsed2208")

compare <- bind_cols(lf,
                     matched_item = matched_item,
                     matched_label = matched_label,
                     matched_tau = matched_tau)

write.table(compare, file = "data-raw/data/lf_gto_match.txt",
            sep = "\t", quote = FALSE, row.names = FALSE)

# construct item names for instrument gl1 and gl2
# gl1: GSED LF Version 1 (June 2022)
# gl2: GSED LF Version 2 (Oct 2022)

# create gl1 itembank
gl1_names <- paste0("gl1",
                    c(rep("aa", 49), rep("bb",52), rep("cc", 54)),
                    "d",
                    c(formatC(1:49, width = 3, flag = "0"),
                      formatC(1:52, width = 3, flag = "0"),
                      formatC(1:54, width = 3, flag = "0")))
gl1 <- data.frame(
  key = "gsed2208",
  item = gl1_names,
  tau = gto$tau,
  label = gto$tau,
  decompose_itemnames(gl1_names))

# create gl2 itembank
gl2_names <- paste0("gl2",
                    c(rep("aa", 49), rep("bb",52), rep("cc", 54)),
                    "d",
                    c(formatC(1:49, width = 3, flag = "0"),
                      formatC(1:52, width = 3, flag = "0"),
                      formatC(1:54, width = 3, flag = "0")))
taun <- c(
  gto$tau[1:49][as.numeric(tr[1:49, "gl1$item"])],
  tau[50:101][as.numeric(tr[50:101, "gl1$item"])],
  tau[102:155][as.numeric(tr[102:155, "gl1$item"])])
gl2 <- data.frame(
  key = "gsed2208",
  item = gl2_names,
  tau = taun,
  label = get_labels(names(taun)),
  decompose_itemnames(gl2_names)
  )

compare <- bind_cols(tr,
                     gto_item = item,
                     gl1_item = gl1$item,
                     gl1_tau = gl1$tau,
                     gl1_label = gl2$label,
                     gl2_item = gl2$item,
                     gl2_tau = gl2$tau,
                     gl2_label = gl2$label)

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
