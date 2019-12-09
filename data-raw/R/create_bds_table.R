library(dscore)
library(dplyr)
library(openxlsx)

# define project
project <- path.expand("~/Package/dscore/dscore")

# ------------- combine Wiechen keys
fn <- file.path(project, "data-raw/data/itembankVWO1.txt")  # May 27, 2019
ib1 <- read.delim(file = fn, stringsAsFactors = FALSE)

fn <- file.path(project, "data-raw/data/itembank.json") # from webtool package
lines <- readLines(fn, encoding = "UTF-8")
ib2 <- data.frame(jsonlite::fromJSON(lines), stringsAsFactors = FALSE)
names(ib2) <- c("lex_dutch1996", "bds_label", "domain", "type", "occ2", "bdsr", "bdsl") 

ib <- left_join(ib1, ib2, by = "lex_dutch1996")
idx <- gtools::mixedorder(ib$lex_dutch2005)
ib <- ib[idx, c("lex_dutch2005", "lex_dutch1996", "lex_dutch1983", 
               "lex_smocc", "lex_ghap", "lex_gcdg", "lex_gsed", 
               "occ", "labelNL", "labelEN", "bds_label", 
               "bdsr", "bdsl",
               "occ", "occ2", "type", "LR", 
               "tau", "m", "hot", "domain")]

fo <- file.path(project, "data-raw/data/bds.txt")
write.table(ib, file = fo, quote = FALSE, sep = "\t",
            na = "", row.names = FALSE)

