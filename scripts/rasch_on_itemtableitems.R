library(dscore)
library(ddata)

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

#run rasch - works
#rasch(gcdg_items_c)


#select equategroups for items that are in the data
itemtable_select <- itemtable[which(itemtable$item %in%names(gcdg_items_c)),]

equatelist <- tapply(itemtable_select$item, itemtable_select$equate, list)

fit1 <- rasch(gcdg_items_c, equate = equatelist)


#compare dutch items with published tau
length(get_diff(fit1)[paste("v",1:57)])
pub_tau <- filter(itembank, !is.na(lex.dutch1983)) %>% .$tau
names(pub_tau) <- filter(itembank, !is.na(lex.dutch1983)) %>% .$lex.jam
tau1 <- anchor(get_diff(fit1), items = c("n12", "n26"))
cor(pub_tau, tau1[paste0("n", 1:57)])
plot(x = pub_tau, y = pub_tau - tau1[paste0("n", 1:57)],
     xlab = "Published", ylab = "Published - estimated",
     main = "Difficulties items n1:n57")
abline(0, 0, lty = 2)
summary(tau1)


#only dutch items to set the difficulty parameters
items <- item_names("NL")
data <- ddata::gcdg %>% select_(.dots = items)
fit2 <- rasch(data, count = gcdg_count)
nlb <- get_diff(fit2)

fit2 <- rasch(gcdg_items_c, equate = equatelist, b_fixed=nlb)

#compare dutch items with published tau
length(get_diff(fit2)[paste("v",1:57)])
pub_tau <- filter(itembank, !is.na(lex.dutch1983)) %>% .$tau
names(pub_tau) <- filter(itembank, !is.na(lex.dutch1983)) %>% .$lex.jam
tau2 <- anchor(get_diff(fit2), items = c("n12", "n26"))
cor(pub_tau, tau2[paste0("n", 1:57)])
plot(x = pub_tau, y = pub_tau - tau1[paste0("n", 1:57)],
     xlab = "Published", ylab = "Published - estimated",
     main = "Difficulties items n1:n57")
abline(0, 0, lty = 2)

summary(tau2)

tau_out <- data.frame(item=names(tau1),tau=tau1)

itembank <- join(itemtable, tau_out, by="item", type="left")

