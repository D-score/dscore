# ------------- preliminaries
library("dplyr")
library("haven")
library("labelled")

# read data
pops_orig <- haven::read_sav(path.expand("~/Websites/dbook/dbook1/data-raw/data/pops/POPS19groeiSDS2whoTranslatedExtrav2PLUS.sav"))

# translate DDI itemnames to lex_gsed
items <- gseddata::rename_gcdg_gsed(paste0("n", 1:57))
names(pops_orig)[44:100] <- items

# rename variables
pops_data <- pops_orig %>% 
  mutate(
    subjid    = as.integer(patid),
    sex       = recode(gender, `1` = "male", `2` = "female", .missing = "unknown"),
    agedays   = as.integer(Age),
    age       = agedays / 365.25,
    gagebrth  = as.integer(gestationalage * 7)
  ) %>% 
  mutate_at(vars(items), function(x) as.integer(1 - x)) %>% 
  dplyr::select(subjid, sex, agedays, age,
                gagebrth, 
                dead, handicap,
                items)

# count nuber of NA's in items
nas <- apply(pops_data[, items], MARGIN = 1, function(x) sum(is.na(x)))

# select rows with at least one DDI-item
#        infants below 32 weeks (224 days) gestational age
#                no dead, no handicaps
pops_pt <- pops_data %>% 
  dplyr::filter(nas < 57) %>% 
  dplyr::filter(gagebrth < 224 & dead == 0 & handicap == 0)

# Data on 258 pre-terms
# length(unique(pops_pt$subjid))
# [1] 258

# ---- save first 100 rows
popsdemo <- dplyr::slice(pops_pt, 1:100) %>%
  as.data.frame()

ids <- table(popsdemo$subjid)

# -- scramble ID's
set.seed(15199)
id <- sample(100:999, size = length(ids))
popsdemo$id <- rep(id, ids)
milestones <- popsdemo %>%
  select(-subjid, agedays, -dead, -handicap) %>% 
  select(id, agedays, age, sex, everything()) %>% 
  arrange(id, age)

# save to /data
usethis::use_data(milestones, overwrite = TRUE)
