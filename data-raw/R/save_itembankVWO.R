# Saves the itembankVWO4 datasets to /data

# define project
project <- path.expand("~/Package/dscore/dscore")
datafile <- file.path(project, "data-raw/data/itembankVWOv4.txt")  # Created 31 Mar 2015

# ------------- read data
itembank <- read.delim2(file = datafile)

# repair one duplicate variable name in the itembank
levels(itembank$ID.VWO1996) <- c(levels(itembank$ID.VWO1996), "na10")
itembank[51, "ID.VWO1996"] <- "na10"

# change names to lex.xxx convention
names(itembank)[1:4] <- c("lex.dutch1996", "lex.dutch2005", 
                          "lex.dutch1983", "lex.smocc")

# save to /data
devtools::use_data(itembank, overwrite = TRUE)
