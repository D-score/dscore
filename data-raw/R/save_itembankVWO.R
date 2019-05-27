# Saves the itembankVWO4 datasets to /data

# define project
project <- path.expand("~/Package/dscore/dscore")
datafile <- file.path(project, "data-raw/data/itembankVWO1.txt")  # May 27, 2019

# ------------- read data
itembank <- read.delim(file = datafile, stringsAsFactors = FALSE)

# save to /data
devtools::use_data(itembank, overwrite = TRUE)
