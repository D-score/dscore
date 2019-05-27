# This job adds the gsed naming scheme to itembankVWO.txt
# and saves the result to itembankVWO1.txt
library(gseddata)

# define project
project <- path.expand("~/Package/dscore/dscore")
datafile <- file.path(project, "data-raw/data/itembankVWO.txt")  # Dec 14, 2016

# ------------- read data
itembank <- read.delim(file = datafile, stringsAsFactors = FALSE)

# change names to lex.xxx convention
names(itembank)[1:6] <- c("lex_dutch1996", "lex_dutch2005", 
                          "lex_dutch1983", "lex_smocc", 
                          "lex_ghap", "lex_gcdg")

lex_gsed <- rename_gcdg_gsed(itembank$lex_gcdg)

itembank1 <- cbind(itembank[, 1:6], lex_gsed, itembank[, 7:ncol(itembank)])

# save to /data
fn <- file.path(project, "data-raw/data/itembankVWO1.txt")
write.table(itembank1, file = fn, quote = FALSE, sep = "\t") 
