fn <- path.expand("~/Package/dscore/dscore/data-raw/data/gcdg_itembank.txt")
gcdg_itembank <- read.delim(fn)

devtools::use_data(gcdg_itembank, overwrite = TRUE)
