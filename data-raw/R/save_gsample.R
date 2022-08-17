file <- "data-raw/data/sample/gsed_sample.txt"
gsample <- read.table(file, header = TRUE, sep = "\t")
usethis::use_data(gsample, overwrite = TRUE)

