# Saves the itembankVWO4 datasets to /data

# define project
project <- path.expand("~/Package/dscore/dscore")
datafile <- file.path(project, "data-raw/data/itembankVWO.txt")  # Dec 14, 2016

# ------------- read data
itembank <- read.delim(file = datafile, stringsAsFactors = FALSE)

# change names to lex.xxx convention
names(itembank)[1:6] <- c("lex_dutch1996", "lex_dutch2005", 
                          "lex_dutch1983", "lex_smocc", 
                          "lex_ghap", "lex_gcdg")

# save to /data
devtools::use_data(itembank, overwrite = TRUE)
