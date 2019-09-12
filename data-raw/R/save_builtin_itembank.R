# Saves the itembankVWO4 datasets to /data

# define project
project <- path.expand("~/Package/dscore/dscore")
datafile <- file.path(project, "data-raw/data/itembankVWO1.txt")  # May 27, 2019

# ------------- read data
builtin_itembank <- read.delim(file = datafile, stringsAsFactors = FALSE)

# save to /data
usethis::use_data(builtin_itembank, overwrite = TRUE)
