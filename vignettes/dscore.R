## ----rename--------------------------------------------------------------
library("dscore")

pops
class(pops)
nrow(pops)

## ----children------------------------------------------------------------
# 25 children, 4 time points per child
length(unique(pops$patid))

## ------------------------------------------------------------------------
names(pops)[11:67]

## ------------------------------------------------------------------------
names(itembank)[1:5]

## ------------------------------------------------------------------------
ib <- itembank[!is.na(itembank$tau),c("lex.GHAP", "labelEN", "tau")]
head(ib, 3)

## ------------------------------------------------------------------------
head(data.frame(source = names(pops)[11:67], ghap = ib$lex.GHAP))

## ------------------------------------------------------------------------
names(pops)[11:67] <- as.character(ib$lex.GHAP)
names(pops)

## ------------------------------------------------------------------------
gettau(names(pops)[11:67])

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
(DAZ <- daz(d))

## ------------------------------------------------------------------------
zad(DAZ)

## ----eval = FALSE--------------------------------------------------------
#  ## still a bit messy, but works for now, looking for dplyr solution
#  data <- data.frame(data)
#  data$ages <- round(data$daycor/365.25, 4)
#  ds <- split(data[, c("patid", "items", "scores", "ages")], f = data$patid)
#  dl <- lapply(ds, FUN = dscore)  # repeat estimation per person
#  dazl <- lapply(dl, FUN = daz)
#  d <- as.numeric(unlist(dl))
#  DAZ <- as.numeric(unlist(dazl))

## ------------------------------------------------------------------------
# still needs some work

# prepare for join
summ <- as.tbl(data) %>%
  group_by(patid, moment) %>%
  summarise(nd = n()) %>%
  ungroup()
#summ <- data.frame(summ, d = d, DAZ = DAZ, 
#          idc = as.character(summ$patid), moc = as.character(summ$moment))


## ------------------------------------------------------------------------
#pops <- mutate(pops, idc = as.character(patid), moc = as.character(moment))

## ----join, eval = FALSE--------------------------------------------------
#  # and join
#  pops2 <- left_join(pops, summ, by = c("idc", "moc"))
#  print(select(pops2, idc, moc, nd, d, DAZ), n = 15)

