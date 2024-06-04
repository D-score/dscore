hh_subset <- c(
  "gpasec004", "gpasec015", "gpalac001", "gpacgc002", "gpamoc011",
  "gpasec010", "gpaclc007", "gpalgc012", "gpasec014", "gpalgc021",
  "gpasec020", "gpamoc017", "gpalgc019", "gpasec025", "gpaclc033",
  "gpaclc023", "gpasec032", "gpasec045", "gpamoc024", "gpamoc037",
  "gpaclc034", "gpamoc029", "gpasec039", "gpamoc028", "gpamoc044",
  "gpamoc042", "gpamoc043", "gpaclc046", "gpamoc049", "gpasec064",
  "gpamoc054", "gpamoc058", "gpamoc056", "gpalgc059", "gpamoc060",
  "gpalgc068", "gpamoc061", "gpasec075", "gpamoc062", "gpamoc065",
  "gpamoc063", "gpalgc072", "gpasec086", "gpamoc071", "gpagmc067",
  "gpamoc078", "gpaclc088", "gpaclc093", "gpamoc084", "gpaxxc092",
  "gpaclc101", "gpacmc090", "gpaclc112", "gpamoc106", "gpaclc113"
)
file <- "data-raw/data/sample/gsed_sample.txt"
gsample <- read.table(file, header = TRUE, sep = "\t")
usethis::use_data(gsample, overwrite = TRUE)

sample_sf <- cbind(gsample[, 1:9], gpamoc008 = NA, gsample[, 10:140])
sample_lf <- cbind(gsample[, 1:2], gsample[, 141:ncol(gsample)])
sample_hf <- sample_sf[, c("subjid", "agedays", hh_subset)]

colnames(sample_sf) <- c("subjid", "agedays", paste0("sf", formatC(1:139, width = 3, flag = "0")))
colnames(sample_lf) <- c("subjid", "agedays", paste0("lf", formatC(1:155, width = 3, flag = "0")))
colnames(sample_hf) <- c("subjid", "agedays", paste0("hf", formatC(1:55, width = 3, flag = "0")))

usethis::use_data(sample_sf, overwrite = TRUE)
usethis::use_data(sample_lf, overwrite = TRUE)
usethis::use_data(sample_hf, overwrite = TRUE)
