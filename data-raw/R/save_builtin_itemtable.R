# Save built-in itemtable
# Fields: item, instrument, domain, mode, number, equate, label

# define project
project <- path.expand("~/Package/dscore/dscore")

fn <- file.path(project, "data-raw/data/gseddata_itemtable.txt")
it <- read.delim(file = fn, quote = "", stringsAsFactors = FALSE, na = "")

# add mullen items
it <- bind_rows(it, mullen_itemtable)
# add gsed short form items
it <- bind_rows(it, rapid_itemtable)


# save to /data
builtin_itemtable <- it
usethis::use_data(builtin_itemtable, overwrite = TRUE)
