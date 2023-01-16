file <- "data-raw/data/sample/gsed_sample.txt"
gsample <- read.table(file, header = TRUE, sep = "\t")
usethis::use_data(gsample, overwrite = TRUE)

sample_sf <- cbind(gsample[, 1:9], gpamoc008 = NA, gsample[, 10:140])
colnames(sample_sf) <- c("subjid", "agedays", paste0("sf", formatC(1:139, width = 3, flag = "0")))

sample_lf <- cbind(gsample[, 1:2], gsample[, 141:ncol(gsample)])
colnames(sample_lf) <- c("subjid", "agedays", paste0("lf", formatC(1:155, width = 3, flag = "0")))

usethis::use_data(sample_sf, overwrite = TRUE)
usethis::use_data(sample_lf, overwrite = TRUE)
