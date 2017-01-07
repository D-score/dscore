library(dscore)
library(ddata)
library(dplyr)
library(plyr)
library(tidyr)
#items in data that are in itemtable as well
gcdg_items <- gcdg[,which(names(gcdg)%in%itemtable[,"item"])]

#alle items die in de data zitten
#gcdg_items <- gcdg[,which(names(gcdg)%in%item_names())]

#function to diagnose if items are eligible
diag_items <- function(x){
  min=min(x,na.rm=TRUE) 
  max=max(x,na.rm=TRUE)
  table=table(x)
  diagout <- (min<0 | max >1 | min==max|min(table)<2|min==Inf|min==(-Inf))
  if(diagout){c(item=names(x),min=min,max=max,table)}
}
out_diagitems <- do.call("rbind",lapply(gcdg_items, diag_items))

#exclude items that are not eligible
gcdg_items_c <- gcdg_items[,which(!names(gcdg_items) %in% rownames(out_diagitems))]

#select equategroups for items that are in the data
itemtable_select <- itemtable[which(itemtable$item %in%names(gcdg_items_c)),]

equatelist <- tapply(itemtable_select$item, itemtable_select$equate, list)

#only dutch items to set the difficulty parameters
items <- item_names("NL")
data <- ddata::gcdg %>% select_(.dots = items)
fit2 <- rasch(data, count = gcdg_count)
nlb <- get_diff(fit2)

fit1 <- rasch(gcdg_items_c, equate = equatelist, b_fixed=nlb)

#schatten van tau obv model fit
tau1 <- anchor(get_diff(fit1), items = c("n12", "n26"))

#maak de tau koppelbaar aan itemtable
tau_out <- data.frame(item=names(tau1),tau=tau1)

#koppel de tau aan de itemtable om een itembank te maken
itembank1 <- join(itemtable, tau_out, by="item", type="left")

alldscore <- gcdg %>%
  select(id, wave, age, acom1:apbs5 ) %>%  ##hier moeten juiste items geselecteerd worden
  gather(item, score,  acom1:apbs5, na.rm = TRUE) %>%
  arrange(id, age) %>%
  group_by(id, age) %>%
  summarise(d = dscore(score, item, age, mu = "model", itembank=itembank1 ,lexicon = "item")) %>%
  ungroup()

plot(x = NULL, y = NULL, xlim = c(0, 48), ylim = c(0, 80),
     xaxp = c(0, 48, 8),
     xlab = "Age (in months)", ylab = "D-score")
abline(h = seq(0, 80, 10), v = seq(0, 48, 6), col = "grey90", lty = 3)
xgrid <- seq(1/12, 4, 1/12)
prov_mu <- function(t) {44.35 - 1.8 * t + 28.47 * log(t + 0.25)}
lines(x = xgrid *12, y = prov_mu(xgrid), lty = 1, lwd = 3,
      col = "olivedrab")
with(alldscore, points(x = age * 12, y = d, cex = 0.4, col = "navy"))

