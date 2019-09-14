# Creates the built-in itembank
library(openxlsx)

# define project
project <- path.expand("~/Package/dscore/dscore")

# ------------- dutch model
fn <- file.path(project, "data-raw/data/itembankVWO1.txt")  # May 27, 2019
ib_dutch <- read.delim(file = fn, stringsAsFactors = FALSE)
ib_dutch$tau_dutch <- ib_dutch$tau
ib_dutch$tau <- NULL

# ------------- gcdg model
fn  <- path.expand("~/Package/dscore/dscore/data-raw/data/gcdg_itembank.txt")
ib_gcdg <- read.delim(fn)
ib_gcdg$tau_gcdg <- ib_gcdg$tau
ib_gcdg$tau <- NULL

# ------------- gsed model
fn <- path.expand("~/Project/GSED/dmetric/models/807_17/model.Rds")
gsed_model_807_17 <- readRDS(file = fn)
ib_gsed <- gsed_model_807_17$itembank
ib_gsed$tau_gsed <- ib_gsed$tau
ib_gsed$tau <- NULL


# save to /data
# usethis::use_data(builtin_itembank, overwrite = TRUE)
