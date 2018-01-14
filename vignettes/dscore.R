## ----setup, include=FALSE-----------------------------------------------------
  knitr::opts_chunk$set(echo = TRUE, fig.retina = 2)
  options(width = 80)

## ----rename-------------------------------------------------------------------
library("dscore")

head(popsdemo[, 1:10], 5)
nrow(popsdemo)

## -----------------------------------------------------------------------------
items <- names(popsdemo)[8:64]

## -----------------------------------------------------------------------------
names(itembank)[grep("lex_", names(itembank))]

## -----------------------------------------------------------------------------
itemset <- !is.na(itembank$lex_dutch1983)
head(cbind(items, itembank[itemset, c("lex_ghap")]))

## -----------------------------------------------------------------------------
ib <- itembank[itemset,c("lex_dutch1983", "lex_ghap", "labelEN", "tau")]
head(ib, 3)

## -----------------------------------------------------------------------------
item_locations <- names(popsdemo) %in% items
names(popsdemo)[item_locations] <- as.character(ib$lex_ghap)

## -----------------------------------------------------------------------------
gettau(items = names(popsdemo)[item_locations])

## ----warnings=FALSE, message=FALSE--------------------------------------------
library("tidyr")
library("dplyr")
data <- popsdemo %>% 
  select(patid, moment, age, daycor, GSFIXEYE:GSKIK) %>%
  gather(items, scores, GSFIXEYE:GSKIK, na.rm = TRUE) %>%
  arrange(patid, moment)
head(data)

## -----------------------------------------------------------------------------
child1 <- filter(data, patid == 11126)

scores <- child1$scores
items <- as.character(child1$items)
ages <- round(child1$daycor/365.25, 4)

# calculate dscore and daz for each time point for given child
(d <- dscore(scores, items, ages))
daz(d)

## -----------------------------------------------------------------------------
zad(daz(d))

## -----------------------------------------------------------------------------
# use age corrected for gestational age
data <- data.frame(data)
data$ages <- round(data$daycor/365.25, 4)

# calculate D-score and DAZ
ds <- split(data, data$patid)
dl <- parallel::mclapply(ds, FUN = dscore)
dazl <- lapply(dl, FUN = daz)
df <- data.frame(
  patid = rep(as.numeric(names(dl)), times = unlist(lapply(dl, length))),
  ages = as.numeric(unlist(lapply(dl, names))),
  dscore = as.numeric(unlist(dl)),
  daz = as.numeric(unlist(dazl)))
head(df)

## -----------------------------------------------------------------------------
# merge dscore and daz into popsdemo data
popsdemo$ages <- round(popsdemo$daycor/365.25, 4)
popsdemo <- merge(popsdemo, df, all.x = TRUE)
head(select(popsdemo, patid, moment, ages, dscore, daz))

