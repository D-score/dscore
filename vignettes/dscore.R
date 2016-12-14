## ----rename--------------------------------------------------------------
library("dscore")

pops
class(pops)
nrow(pops)

## ----children------------------------------------------------------------
# 25 children, 4 time points per child
length(unique(pops$patid))

## ------------------------------------------------------------------------
test <- 9:65

## ------------------------------------------------------------------------
names(itembank)[1:6]

## ------------------------------------------------------------------------
itemset <- !is.na(itembank$lex.dutch1983)
cbind(names(pops)[test], itembank[itemset, c("lex.dutch1983", "labelEN", "tau")])

## ------------------------------------------------------------------------
ib <- itembank[itemset,c("lex.dutch1983", "lex.GHAP", "labelEN", "tau")]
head(ib, 3)

## ------------------------------------------------------------------------
names(pops)[test] <- as.character(ib$lex.GHAP)

## ------------------------------------------------------------------------
gettau(names(pops)[test])

## ------------------------------------------------------------------------
library("tidyr")
library("dplyr")
data <- pops %>% 
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
# merge dscore and daz into pops data
pops$ages <- round(pops$daycor/365.25, 4)
pops <- merge(pops, df, all.x = TRUE)
head(select(pops, patid, moment, ages, dscore, daz))

