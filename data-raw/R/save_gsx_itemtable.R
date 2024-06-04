# Processes SF item order info, and save table in keys
# Updated 22021205: Reconstructs gs1 and gl1 item codes
library(dplyr)
library(dscore)
library(openxlsx)

# NOTE: The following file is invalid for LF, use only for SF
# NOTE: sfi$tau is taken from key 294_0. IGNORE
fn <- file.path("data-raw/data/SF_LF_Phase 2_Item Ordering.txt")
sfi <- read.delim(
  file = fn, quote = "",
  stringsAsFactors = FALSE, na = "",
  fileEncoding = "UTF-8",
  header = TRUE
)
colnames(sfi) <- c("start", "ph2", "ph1", "tau", "label", "domain", "gsed2", "gsed1")
sfi$domain <- recode(sfi$domain, sem = "se", motor = "mo", lang = "lg", cog = "cg", life = "li")

# LF Item order (corrected 22021201)
fn <- file.path("data-raw/data/lf_gto_match_2.xlsx")
lfi <- read.xlsx(fn, sheet = "gto_LF1_LF2 (221130)", startRow = 2)
lfi <- lfi[, c("stream", "matched_item", "matched_tau", "LF2_correct", "LF2_Stem")]
lfi <- lfi[order(lfi$LF2_correct), ]
lfi <- lfi[!is.na(lfi$matched_tau), ]

# select core model, using gpa and gto instrument codes
fn <- file.path("data-raw/data/keys/293_0.txt")
core <- read.delim(
  file = fn, quote = "",
  stringsAsFactors = FALSE, na = "",
  fileEncoding = "UTF-8",
  header = TRUE
)
core$label <- get_labels(core$item)

# Construct item names gs1
# gs1: GSED SF Version 1 (Validation Phase 2)
# create gs1 itembank part
gs1_names <- paste0("gs1", sfi$domain, "c", formatC(1:139, width = 3, flag = "0"))
gs1 <- data.frame(gs1_names, item = sfi$gsed2)
gs1 <- left_join(x = gs1, y = core, by = "item")
gs1 <- data.frame(
  key = "gsed2212",
  item = gs1$gs1_names,
  tau = gs1$tau,
  label = gs1$label,
  decompose_itemnames(gs1_names)
)
gs1[28, "label"] <- "Does your child hold his/her hands in fists all the time?"

# Construct item names gl1
# gl1: GSED LF Version 1 (Validation Phase 2)
# create gl1 itembank part
gl1_names <- paste0(
  "gl1",
  c(rep("gm", 49), rep("lg", 52), rep("fm", 54)),
  "d",
  c(
    formatC(1:49, width = 3, flag = "0"),
    formatC(1:52, width = 3, flag = "0"),
    formatC(1:54, width = 3, flag = "0")
  )
)
gl1 <- data.frame(new = gl1_names, item = lfi$matched_item)
gl1 <- left_join(x = gl1, y = core, by = "item")
gl1 <- data.frame(
  key = "gsed2212",
  item = gl1_names,
  tau = lfi$matched_tau,
  label = lfi$LF2_Stem,
  decompose_itemnames(gl1_names)
)

gsx <- bind_rows(gs1, gl1)

write.table(gsx,
  file = "data-raw/data/keys/items_gs1_gl1.txt",
  quote = FALSE, sep = "\t", row.names = FALSE
)
