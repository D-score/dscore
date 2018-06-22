context("ability:check")

items <- names(popsdemo)[8:64]
itemset <- !is.na(itembank$lex_dutch1983)
ib <- itembank[itemset,c("lex_dutch1983", "lex_ghap", "labelEN", "tau")]

item_locations <- names(popsdemo) %in% items
names(popsdemo)[item_locations] <- as.character(ib$lex_ghap)

delta <- gettau(items = names(popsdemo)[item_locations])
items <- names(delta)

data <- popsdemo
data$age <- round(data$age/365.25, 3)
key <- data.frame(item = items, 
                  delta = delta)

z <- ability(data, items, age = "age", key = key)
