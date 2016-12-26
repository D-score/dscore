# save_pops.R
# Saves the pops_dev and pops_dev97 datasets to /data

# ------------- preliminaries
library("haven")
library("dplyr")

# define project
project <- path.expand("~/Package/dscore/dscore")
datafile <- file.path(project, "data-raw/data/POPS19groeiSDS2whoTranslatedExtra.sav")  # Created 29 Jan 2016

# ------------- read data
pops <- haven::read_sav(datafile)

# ------------- select developmental data, children <= 2 years
pops <- dplyr::filter(pops, occ <= 4) %>%
  dplyr::select(patid, gender, gestationalage, moment,
                age, occ, daycor, dead,
                42:98, dscore, daz)

# ---- save first 25 children
popsdemo <- dplyr::slice(pops, 1:100)

# save to /data
devtools::use_data(popsdemo, overwrite = TRUE)
