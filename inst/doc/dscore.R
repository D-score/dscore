## ----rename--------------------------------------------------------------
library("dscore")

popsdemo
class(popsdemo)
nrow(popsdemo)

## ----children------------------------------------------------------------
# 25 children, 4 time points per child
length(unique(popsdemo$patid))

## ------------------------------------------------------------------------
test <- 9:65

## ------------------------------------------------------------------------
names(itembank)[1:6]

## ------------------------------------------------------------------------
itemset <- !is.na(itembank$lex.dutch1983)
cbind(names(popsdemo)[test], itembank[itemset, c("lex.dutch1983", "labelEN", "tau")])

## ------------------------------------------------------------------------
ib <- itembank[itemset,c("lex.dutch1983", "lex.GHAP", "labelEN", "tau")]
head(ib, 3)

## ------------------------------------------------------------------------
names(popsdemo)[test] <- as.character(ib$lex.GHAP)

## ------------------------------------------------------------------------
gettau(names(popsdemo)[test])

## ------------------------------------------------------------------------
library("tidyr")
library("dplyr")
data <- popsdemo %>% 
  select(patid, moment, age, daycor, GSFIXEYE:GSKIK) %>%
  gather(items, scores, GSFIXEYE:GSKIK, na.rm = TRUE) %>%
  mutate(scores = 1 - scores) %>% 
  arrange(patid, moment)
data

## ------------------------------------------------------------------------
child1 <- filter(data, patid == 1)

scores <- child1$scores
items <- as.character(child1$items)
ages <- round(child1$daycor/365.25, 4)

# calculate dscore and daz for each time point for given child
(d <- dscore(scores, items, ages))
daz(d)

## ------------------------------------------------------------------------
zad(daz(d))

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
# merge dscore and daz into popsdemo data
popsdemo$ages <- round(popsdemo$daycor/365.25, 4)
popsdemo <- merge(popsdemo, df, all.x = TRUE)
head(select(popsdemo, patid, moment, ages, dscore, daz))

