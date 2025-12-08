file <- "data-raw/data/sample/triple.txt"
triple <- read.table(file, header = TRUE, sep = "\t")
usethis::use_data(triple, overwrite = TRUE)
