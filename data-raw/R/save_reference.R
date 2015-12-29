# Saves the itembankVWO4 datasets to /data

# define project
project <- path.expand("~/Package/dscore/dscore")
datafile <- file.path(project, "data-raw/data/Dreference.txt")  # Created 13 Jun 2013

# ------------- read data
Dreference <- read.delim2(file = datafile)

# save to /data
devtools::use_data(Dreference, overwrite = TRUE)
