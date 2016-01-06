# Saves the itembankVWO4 datasets to /data

# define project
project <- path.expand("~/Package/dscore/dscore")
datafile <- file.path(project, "data-raw/data/itembankVWO.txt")  # Dated Jan 6, 2016

# ------------- read data
itembank <- read.delim2(file = datafile)

# change names to lex.xxx convention
names(itembank)[1:5] <- c("lex.dutch1996", "lex.dutch2005", 
                          "lex.dutch1983", "lex.SMOCC", 
                          "lex.GHAP")

# save to /data
devtools::use_data(itembank, overwrite = TRUE)
