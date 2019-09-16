# Saves built-in scales

# define project
project <- path.expand("~/Package/dscore/dscore")

f1 <- file.path(project, "data-raw/data/scales/dutch.txt")
f2 <- file.path(project, "data-raw/data/scales/gcdg.txt")
f3 <- file.path(project, "data-raw/data/scales/gsed.txt")

scale_dutch <- read.delim(file = f1, stringsAsFactors = FALSE)
scale_gcdg  <- read.delim(file = f2, stringsAsFactors = FALSE)
scale_gsed  <- read.delim(file = f3, stringsAsFactors = FALSE)

builtin_scales <- bind_rows(scale_dutch, scale_gcdg, scale_gsed)

# save to /data
usethis::use_data(builtin_scales, overwrite = TRUE)
