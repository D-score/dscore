context("ability")

items <- names(popsdemo)[8:64]
itemset <- !is.na(itembank$lex_dutch1983)
ib <- itembank[itemset,c("lex_dutch1983", "lex_ghap", "labelEN", "tau")]

item_locations <- names(popsdemo) %in% items
names(popsdemo)[item_locations] <- as.character(ib$lex_ghap)

delta <- gettau(items = names(popsdemo)[item_locations])
items <- names(delta)

data <- popsdemo
data$age <- round(data$daycor / 365.25, 3)

key <- data.frame(item = items, 
                  delta = delta, 
                  stringsAsFactors = FALSE)
abil <- ability(data, items, age = "age", key = key)

# --- using dscore()

dsco <- data %>%
  select(patid, age, one_of(items)) %>%
  gather(item, score, -age, -patid, na.rm = TRUE) %>%
  arrange(patid, age) %>%
  group_by(patid, age) %>%
  summarise(d = dscore::dscore(scores = score, items = item,
                               ages = age, 
                               itembank = dscore::itembank, 
                               lexicon = "ghap")) %>%
  ungroup() %>%
  pull(d)

test_that("ability() and dscore() produce same length", {
  expect_identical(length(abil), length(dsco))
})

test_that("ability() and dscore() produce same D-scores", {
  expect_identical(abil, dsco)
})
