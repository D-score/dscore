# Saves the itembankVWO4 datasets to /data

# define project
project <- path.expand("~/Package/dscore/dscore")
datafile <- file.path(project, "data-raw/data/itembankVWO.txt")  # Dec 14, 2016

# ------------- read data
itembank <- read.delim(file = datafile)

# change names to lex.xxx convention
names(itembank)[1:6] <- c("lex.dutch1996", "lex.dutch2005", 
                          "lex.dutch1983", "lex.SMOCC", 
                          "lex.GHAP", "lex.jam")

# save to /data
devtools::use_data(itembank, overwrite = TRUE)
